{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Array
import Control.Monad

main :: IO ()
main = do
  prog <- T.getLine
  let strs = T.split (== ',') prog
      nums :: [Int] = map (read . T.unpack) strs
  maxOutput <- findOptimal nums
  putStrLn $ show maxOutput

findOptimal :: [Int] -> IO Int
findOptimal prog = do
  let len = length prog
      progArray = listArray (1, len) prog
      phaseSeqs = permutations [0..4]
  outputs <- mapM (\phaseSeq -> runPhases phaseSeq progArray) phaseSeqs
  return $ maximum outputs

runPhases :: [Int] -> Array Int Int -> IO Int
runPhases phaseSeq prog =
  foldM runWithPhase 0 phaseSeq
 where
  runWithPhase :: Int -> Int -> IO Int
  runWithPhase input phase = do
    (_, outputs) <- run [phase, input] 1 prog
    return $ head outputs -- assume exactly one output per run

-- Return new array storage and outputs so far
run :: [Int] -> Int -> Array Int Int -> IO (Array Int Int, [Int])
run inputs i a =
  let (opCode, paramModes) = splitInstr (a ! i)
  in case opCode of
      1 -> binaryOp paramModes (+)
      2 -> binaryOp paramModes (*)
      3 -> storeInput
      4 -> output paramModes
      5 -> jump paramModes (/= 0)
      6 -> jump paramModes (== 0)
      7 -> binaryOp paramModes (\x y -> if x < y then 1 else 0)
      8 -> binaryOp paramModes (\x y -> if x == y then 1 else 0)
      99 -> return (a, [])
      _ -> error "Invalid program"
 where
  binaryOp :: [Int] -> (Int -> Int -> Int) -> IO (Array Int Int, [Int])
  binaryOp (mode1 : mode2 : _) op =
    let pos1 = a ! (i + 1)
        pos2 = a ! (i + 2)
        pos3 = (a ! (i + 3)) + 1
        val1 = if mode1 == 0 then a ! (pos1 + 1) else pos1
        val2 = if mode2 == 0 then a ! (pos2 + 1) else pos2
        val3 = val1 `op` val2
        newA = a // [(pos3, val3)]
    in run inputs (i + 4) newA
  binaryOp _ _ = error "No parameter modes passed to binary operation"

  storeInput :: IO (Array Int Int, [Int])
  storeInput =
    case inputs of
      [] -> error "No input value to store"
      first : rest ->
        run rest (i + 2) $ a // [((a ! (i + 1)) + 1, first)]

  output :: [Int] -> IO (Array Int Int, [Int])
  output (mode : _) = do
    let pos = a ! (i + 1)
        val = if mode == 0 then a ! (pos + 1) else pos
    (newA, outputs) <- run inputs (i + 2) a
    return (newA, val : outputs)
  output _ = error "No parameter code for output instruction"

  jump :: [Int] -> (Int -> Bool) -> IO (Array Int Int, [Int])
  jump (mode1 : mode2 : _) cond =
    let pos1 = a ! (i + 1)
        pos2 = a ! (i + 2)
        val1 = if mode1 == 0 then a ! (pos1 + 1) else pos1
        val2 = if mode2 == 0 then a ! (pos2 + 1) else pos2
        newI = if cond val1 then val2 + 1 else i + 3
    in run inputs newI a
  jump _ _ = error "No parameter modes passed to jump instruction"

splitInstr :: Int -> (Int, [Int])
splitInstr instr =
  let revInstr = reverse $ show instr
      opCode :: Int = read $ reverse $ take 2 revInstr
      paramModes :: [Int] = map (\c -> read [c]) (drop 2 revInstr) ++ [0, 0]
  in (opCode, paramModes)
