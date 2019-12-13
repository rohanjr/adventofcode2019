{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Int
import Data.List
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  prog <- T.getLine
  let strs = T.split (== ',') prog
      nums :: [Int64] = map (read . T.unpack) strs
  outputs <- runWhole 2 nums
  mapM_ (putStrLn . show) outputs

memoryLimit :: Int
memoryLimit = 1000000

runWhole :: Int64 -> [Int64] -> IO [Int64]
runWhole input prog = do
  a <- V.replicate memoryLimit 0
  fill 0 prog a
  run [input] 0 0 a
 where
  fill :: Int -> [Int64] -> V.MVector (PrimState IO) Int64 -> IO ()
  fill i elems v
    | i >= V.length v || null elems
    = return ()
    | e : es <- elems
    = V.write v i e >> fill (i + 1) es v

-- Return new array storage and outputs so far
run :: [Int64] -> Int64 -> Int64 -> V.MVector (PrimState IO) Int64 -> IO [Int64]
run inputs i relBase a = do
  instr <- V.read a (fromIntegral i)
  let (opCode, paramModes) = splitInstr instr
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
    99 -> return []
    _ -> error "Invalid program"
 where
  -- Interpret a parameter using its parameter mode.
  evalParam :: Int -> Int64 -> IO Int64
  evalParam mode pos = do
    param <- V.read a (fromIntegral pos)
    case mode of
      0 -> V.read a (fromIntegral param)
      1 -> return param
      2 -> V.read a $ fromIntegral (relBase + param)
      _ -> error "Unknown parameter mode"

  evalWriteParam :: Int -> Int64 -> IO Int64
  evalWriteParam mode pos = do
    param <- V.read a (fromIntegral pos)
    return $ case mode of
      0 -> param
      1 -> error "Write parameter cannot be in immediate mode"
      2 -> relBase + param
      _ -> error "Unknown parameter mode"

  binaryOp :: [Int] -> (Int64 -> Int64 -> Int64) -> IO [Int64]
  binaryOp (mode1 : mode2 : mode3 : _) op = do
    val1 <- evalParam mode1 (i + 1)
    val2 <- evalParam mode2 (i + 2)
    writePos <- evalWriteParam mode3 (i + 3)
    let val = val1 `op` val2
    V.write a (fromIntegral writePos) val
    run inputs (i + 4) relBase a
  binaryOp _ _ = error "Not enough parameter modes passed to binary operation"

  storeInput :: [Int] -> IO [Int64]
  storeInput (mode : _) =
    case inputs of
      [] -> error "No input value to store"
      first : rest -> do
        writePos <- evalWriteParam mode (i + 1)
        V.write a (fromIntegral writePos) first
        run rest (i + 2) relBase a
  storeInput _ = error "No parameter modes passed to store instruction"

  output :: [Int] -> IO [Int64]
  output (mode : _) = do
    val <- evalParam mode (i + 1)
    rest <- run inputs (i + 2) relBase a
    return $ val : rest
  output _ = error "No parameter modes passed to output instruction"

  jump :: [Int] -> (Int64 -> Bool) -> IO [Int64]
  jump (mode1 : mode2 : _) cond = do
    val1 <- evalParam mode1 (i + 1)
    val2 <- evalParam mode2 (i + 2)
    let newI = if cond val1 then val2 else i + 3
    run inputs newI relBase a
  jump _ _ = error "Not enough parameter modes passed to jump instruction"

  relBaseOffset :: [Int] -> IO [Int64]
  relBaseOffset (mode : _) = do
    offset <- evalParam mode (i + 1)
    run inputs (i + 2) (relBase + offset) a

splitInstr :: Int64 -> (Int, [Int])
splitInstr instr =
  let revInstr = reverse $ show instr
      opCode :: Int = read $ reverse $ take 2 revInstr
      paramModes :: [Int] = map (\c -> read [c]) (drop 2 revInstr) ++ repeat 0
  in (opCode, paramModes)
