{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Int
import Data.List
import Data.Array
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  prog <- T.getLine
  let strs = T.split (== ',') prog
      nums :: [Int64] = map (read . T.unpack) strs
      outputs = runWhole 1 nums
  putStrLn $ show outputs

memoryLimit :: Int64
memoryLimit = 1000000

runWhole :: Int64 -> [Int64] -> [Int64]
runWhole input prog =
  let progArray = listArray (0, memoryLimit) (prog ++ repeat 0)
  in run [input] 0 0 progArray

-- Return new array storage and outputs so far
run :: [Int64] -> Int64 -> Int64 -> Array Int64 Int64 -> [Int64]
run inputs i relBase a =
  let (opCode, paramModes) = splitInstr $ a ! i
  in case opCode of
      1 -> binaryOp paramModes (+)
      2 -> binaryOp paramModes (*)
      3 -> storeInput paramModes
      4 -> output paramModes
      5 -> jump paramModes (/= 0)
      6 -> jump paramModes (== 0)
      7 -> binaryOp paramModes (\x y -> if x < y then 1 else 0)
      8 -> binaryOp paramModes (\x y -> if x == y then 1 else 0)
      9 -> relBaseOffset paramModes
      99 -> []
      _ -> error "Invalid program"
 where
  write :: Array Int64 Int64 -> Int64 -> Int64 -> Array Int64 Int64
  write a i v = a // [(i, v)]

  -- Interpret a parameter using its parameter mode.
  evalParam :: Int -> Int64 -> Int64
  evalParam mode pos =
    let param = a ! pos
    in case mode of
        0 -> a ! param
        1 -> param
        2 -> a ! (relBase + param)
        _ -> error "Unknown parameter mode"

  evalWriteParam :: Int -> Int64 -> Int64
  evalWriteParam mode pos =
    let param = a ! pos
    in case mode of
        0 -> param
        1 -> error "Write parameter cannot be in immediate mode"
        2 -> relBase + param
        _ -> error "Unknown parameter mode"

  binaryOp :: [Int] -> (Int64 -> Int64 -> Int64) -> [Int64]
  binaryOp (mode1 : mode2 : mode3 : _) op =
    let val1 = evalParam mode1 (i + 1)
        val2 = evalParam mode2 (i + 2)
        writePos = evalWriteParam mode3 (i + 3)
        val = val1 `op` val2
        newA = write a writePos val
    in run inputs (i + 4) relBase newA
  binaryOp _ _ = error "No parameter modes passed to binary operation"

  storeInput :: [Int] -> [Int64]
  storeInput (mode : _)=
    case inputs of
      [] -> error "No input value to store"
      first : rest ->
        let pos = evalWriteParam mode (i + 1)
        in run rest (i + 2) relBase $ write a pos first
  storeInput _ = error "No parameter modes passed to store instruction"

  output :: [Int] -> [Int64]
  output (mode : _) =
    let val = evalParam mode (i + 1)
    in val : run inputs (i + 2) relBase a
  output _ = error "No parameter code for output instruction"

  jump :: [Int] -> (Int64 -> Bool) -> [Int64]
  jump (mode1 : mode2 : _) cond =
    let val1 = evalParam mode1 (i + 1)
        val2 = evalParam mode2 (i + 2)
        newI = if cond val1 then val2 else i + 3
    in run inputs newI relBase a
  jump _ _ = error "No parameter modes passed to jump instruction"

  relBaseOffset :: [Int] -> [Int64]
  relBaseOffset (mode : _) =
    let offset = evalParam mode (i + 1)
    in run inputs (i + 2) (relBase + offset) a

splitInstr :: Int64 -> (Int, [Int])
splitInstr instr =
  let revInstr = reverse $ show instr
      opCode :: Int = read $ reverse $ take 2 revInstr
      paramModes :: [Int] = map (\c -> read [c]) (drop 2 revInstr) ++ repeat 0
  in (opCode, paramModes)
