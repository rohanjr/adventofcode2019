module IntCode
  ( initialise
  , run
  , ProgSt
  , ProgState -- abstract
  , RunState(..)
  ) where

import Data.Int
import Data.Functor.Identity
import qualified Data.Vector.Unboxed as V
import Control.Monad.Trans.State.Lazy


-- State monad for client use.
type ProgSt = StateT ProgState Identity

-- Internal state to the IntCode runtime.
data ProgState = ProgState
  { memory :: V.Vector Int64
  , instrP :: Int64
  , relBase :: Int64
  }

-- Result of running an IntCode program.
data RunState
  = Finished
    -- ^ Reached a "Halt" instruction without failure.
  | Output Int64
    -- ^ Reached an "Output" instruction with this value.

-- Set up an initial program state.
-- This can be passed to `runStateT` to run a `ProgSt` computation.
initialise :: Int -> [Int64] -> ProgState
initialise memoryLimit program
  | length program > memoryLimit
  = error "Program too large for given memory limit"
  | otherwise
  = let mem = V.fromListN memoryLimit $ program ++ repeat 0
    in ProgState{ memory = mem, instrP = 0, relBase = 0 }

-- Run an IntCode program until either it halts or outputs a value.
run :: [Int64] -> ProgSt RunState
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
    99 -> return Finished
    _ -> error "Invalid program"
 where
  binaryOp :: [Int] -> (Int64 -> Int64 -> Int64) -> ProgSt RunState
  binaryOp (mode1 : mode2 : mode3 : _) op = do
    i <- gets instrP
    val1 <- evalParam mode1 (i + 1)
    val2 <- evalParam mode2 (i + 2)
    writePos <- evalWriteParam mode3 (i + 3)
    writeMem writePos $ val1 `op` val2
    setInstrP (i + 4)
    run inputs
  binaryOp _ _ = error "Not enough parameter modes passed to binary operation"

  storeInput :: [Int] -> ProgSt RunState
  storeInput (mode : _) =
    case inputs of
      [] -> error "No input value to store"
      first : rest -> do
        i <- gets instrP
        writePos <- evalWriteParam mode (i + 1)
        writeMem writePos first
        setInstrP (i + 2)
        run rest
  storeInput _ = error "No parameter modes passed to store instruction"

  output :: [Int] -> ProgSt RunState
  output (mode : _) = do
    i <- gets instrP
    val <- evalParam mode (i + 1)
    setInstrP (i + 2)
    return $ Output val
  output _ = error "No parameter modes passed to output instruction"

  jump :: [Int] -> (Int64 -> Bool) -> ProgSt RunState
  jump (mode1 : mode2 : _) cond = do
    i <- gets instrP
    val1 <- evalParam mode1 (i + 1)
    val2 <- evalParam mode2 (i + 2)
    setInstrP $ if cond val1 then val2 else i + 3
    run inputs
  jump _ _ = error "Not enough parameter modes passed to jump instruction"

  relBaseOffset :: [Int] -> ProgSt RunState
  relBaseOffset (mode : _) = do
    ProgState mem i rb <- get
    offset <- evalParam mode (i + 1)
    put ProgState{ memory = mem, instrP = i + 2, relBase = rb + offset }
    run inputs
  relBaseOffset _ = error "Not enough parameter modes passed to relative base offset instruction"

-- Helper functions for program execution --

-- Wrappers around Vector read and write.
-- Note that vector index must be Int, so we need `fromIntegral` to
-- convert from values in memory.
readMem :: Int64 -> ProgSt Int64
readMem i = do
  mem <- gets memory
  return $ mem V.! fromIntegral i

writeMem :: Int64 -> Int64 -> ProgSt ()
writeMem i e = do
  progState <- get
  let newMem = memory progState V.// [(fromIntegral i, e)]
  put progState{ memory = newMem }

-- Parse the instruction at the current instruction pointer,
-- returning the opcode and parameter modes.
readInstr :: ProgSt (Int, [Int])
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
evalParam :: Int -> Int64 -> ProgSt Int64
evalParam mode pos = do
  param <- readMem pos
  case mode of
    0 -> readMem param
    1 -> return param
    2 -> do rb <- gets relBase; readMem (rb + param)
    _ -> error "Unknown parameter mode"

evalWriteParam :: Int -> Int64 -> ProgSt Int64
evalWriteParam mode pos = do
  param <- readMem pos
  case mode of
    0 -> return param
    1 -> error "Write parameter cannot be in immediate mode"
    2 -> (param +) <$> gets relBase
    _ -> error "Unknown parameter mode"

setInstrP :: Int64 -> ProgSt ()
setInstrP k = do
  progState <- get
  put progState{ instrP = k }
