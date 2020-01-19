{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Intcode
  ( ProgMem(..)
  , ProgState
  , ProgSt
  , ProgStateM
  , ProgStM
  , EndState(..)
  , RunState(..)
  , run
  , runToEnd
  ) where

import Data.Int
import Data.List.Index
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.State.Lazy
import Data.Functor.Identity
import qualified Data.Vector.Unboxed as VI

class Monad m => ProgMem s m where
  -- Set up an initial program state.
  -- The result can be used by `runStateT` to run a `ProgStS` computation.
  initMem :: Int -> [Int64] -> m (ProgStateS s)

  -- Read or write at a given memory address.
  readMem :: Int64 -> ProgStS s m Int64
  writeMem :: Int64 -> Int64 -> ProgStS s m ()

data ImVec = ImVec (VI.Vector Int64)
instance ProgMem ImVec Identity where
  initMem memoryLimit program
    | length program > memoryLimit
    = error "Program too large for given memory limit"
    | otherwise
    = let mem = VI.fromListN memoryLimit $ program ++ repeat 0
      in return ProgStateS{ memory = ImVec mem, instrP = 0, relBase = 0 }

  readMem i = do
    ImVec mem <- gets memory
    return $ mem VI.! fromIntegral i

  writeMem i e = do
    progState <- get
    let ImVec mem = memory progState
    let newMem = mem VI.// [(fromIntegral i, e)]
    put progState{ memory = ImVec newMem }

type ProgState = ProgStateS ImVec
type ProgSt = ProgStS ImVec Identity

data MVec = MVec (V.MVector (PrimState IO) Int64)
instance ProgMem MVec IO where
  initMem memoryLimit program
    | length program > memoryLimit
    = error "Program too large for given memory limit"
    | otherwise
    = do
        a <- V.replicate memoryLimit 0
        imapM_ (V.write a) program
        return $ ProgStateS (MVec a) 0 0

  readMem i = do
    MVec mem <- gets memory
    liftIO $ V.read mem (fromIntegral i)

  writeMem i e = do
    MVec mem <- gets memory
    liftIO $ V.write mem (fromIntegral i) e

type ProgStateM = ProgStateS MVec
type ProgStM = ProgStS MVec IO

type ProgStS s m = StateT (ProgStateS s) m

-- Internal state to the IntCode runtime.
data ProgStateS s = ProgStateS
  { memory :: s
  , instrP :: Int64
  , relBase :: Int64
  }

data EndState
  = Halted -- { remainingInputs :: [Int64] }
    -- ^ Reached a "Halt" instruction without failure.
  | AwaitingInput
    -- ^ Reached an "Input" instruction with no inputs left.

-- Result of running an IntCode program.
data RunState
  = Finished EndState
  | Output
    { out :: Int64
    , ins :: [Int64]
    }
    -- ^ Reached an "Output" instruction returning `out` with remaining inputs `ins`.
    -- The latter component allows the client to continue execution with the remaining inputs.

-- Continue to run an Intcode program until it halts, collecting all output values.
runToEnd :: ProgMem s m => [Int64] -> ProgStS s m ([Int64], EndState)
runToEnd inputs =
  run inputs >>= \case
    Finished endState -> return ([], endState)
    Output{..} -> do
      (outs, endState) <- runToEnd ins
      return (out : outs, endState)

-- Run an IntCode program until either it halts or outputs a value.
run :: ProgMem s m => [Int64] -> ProgStS s m RunState
run inputs = do
  (opCode, paramModes) <- readInstr
  case opCode of
    1 -> binaryOp paramModes (+)
    2 -> binaryOp paramModes (*)
    3 -> storeInput paramModes
    4 -> output paramModes
    5 -> jump paramModes (/= 0)
    6 -> jump paramModes (== 0)
    7 -> binaryOp paramModes (\x y -> if x < y then 1 else 0)
    8 -> binaryOp paramModes (\x y -> if x == y then 1 else 0)
    9 -> relBaseOffset paramModes
    99 -> return $ Finished Halted
    _ -> error "Invalid program"
 where
  binaryOp :: ProgMem s m => [Int] -> (Int64 -> Int64 -> Int64) -> ProgStS s m RunState
  binaryOp (mode1 : mode2 : mode3 : _) op = do
    i <- gets instrP
    val1 <- evalParam mode1 (i + 1)
    val2 <- evalParam mode2 (i + 2)
    writePos <- evalWriteParam mode3 (i + 3)
    writeMem writePos $ val1 `op` val2
    setInstrP (i + 4)
    run inputs
  binaryOp _ _ = error "Not enough parameter modes passed to binary operation"

  storeInput :: ProgMem s m => [Int] -> ProgStS s m RunState
  storeInput (mode : _) =
    case inputs of
      [] -> return $ Finished AwaitingInput
      first : rest -> do
        i <- gets instrP
        writePos <- evalWriteParam mode (i + 1)
        writeMem writePos first
        setInstrP (i + 2)
        run rest
  storeInput _ = error "No parameter modes passed to store instruction"

  output :: ProgMem s m => [Int] -> ProgStS s m RunState
  output (mode : _) = do
    i <- gets instrP
    val <- evalParam mode (i + 1)
    setInstrP (i + 2)
    return $ Output val inputs
  output _ = error "No parameter modes passed to output instruction"

  jump :: ProgMem s m => [Int] -> (Int64 -> Bool) -> ProgStS s m RunState
  jump (mode1 : mode2 : _) cond = do
    i <- gets instrP
    val1 <- evalParam mode1 (i + 1)
    val2 <- evalParam mode2 (i + 2)
    setInstrP $ if cond val1 then val2 else i + 3
    run inputs
  jump _ _ = error "Not enough parameter modes passed to jump instruction"

  relBaseOffset :: ProgMem s m => [Int] -> ProgStS s m RunState
  relBaseOffset (mode : _) = do
    ProgStateS mem i rb <- get
    offset <- evalParam mode (i + 1)
    put ProgStateS{ memory = mem, instrP = i + 2, relBase = rb + offset }
    run inputs
  relBaseOffset _ = error "Not enough parameter modes passed to relative base offset instruction"

-- Parse the instruction at the current instruction pointer,
-- returning the opcode and parameter modes.
readInstr :: ProgMem s m => ProgStS s m (Int, [Int])
readInstr = do
  i <- gets instrP
  instr <- readMem i
  return $ splitInstr instr

splitInstr :: Int64 -> (Int, [Int])
splitInstr instr =
  let revInstr = reverse $ show instr
      opCode = read $ reverse $ take 2 revInstr
      paramModes = map (\c -> read [c]) (drop 2 revInstr) ++ repeat 0
  in (opCode, paramModes)

-- Interpret a parameter using its parameter mode.
evalParam :: ProgMem s m => Int -> Int64 -> ProgStS s m Int64
evalParam mode pos = do
  param <- readMem pos
  case mode of
    0 -> readMem param
    1 -> return param
    2 -> do rb <- gets relBase; readMem (rb + param)
    _ -> error "Unknown parameter mode"

evalWriteParam :: ProgMem s m => Int -> Int64 -> ProgStS s m Int64
evalWriteParam mode pos = do
  param <- readMem pos
  case mode of
    0 -> return param
    1 -> error "Write parameter cannot be in immediate mode"
    2 -> (param +) <$> gets relBase
    _ -> error "Unknown parameter mode"

setInstrP :: Monad m => Int64 -> ProgStS s m ()
setInstrP k = do
  progState <- get
  put progState{ instrP = k }
