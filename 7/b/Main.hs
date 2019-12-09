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
      progs = listArray (1, 5) $ replicate 5 $ listArray (1, len) prog
      ips = listArray (1, 5) $ replicate 5 1
      phaseSeqs = permutations [5..9]
  outputs <- mapM (\phaseSeq -> runLoop phaseSeq [0] 1 ips progs) phaseSeqs
  return $ maximum outputs

data Status = Finished | Pending Int

runLoop :: [Int] -> [Int] -> Int -> Array Int Int -> Array Int (Array Int Int) -> IO Int
runLoop phaseSeq inputs amp ips progs = do
  let (inputs', phaseSeq') =
        case phaseSeq of
          [] -> (inputs, [])
          phase : rest -> (phase : inputs, rest)
  (newProg, outputs, status) <- run inputs' (ips ! amp) (progs ! amp)
  case status of
    Finished
      | amp == length progs
      -- Circuit finished, so return final output (assuming exactly one)
      -> return (head outputs)
      | otherwise
      -- This amp has halted, but we need to finish the round
      -> runLoop phaseSeq' outputs (amp + 1) ips progs
    Pending i ->
      let amp' = if amp == length progs then 1 else amp + 1
          ips' = ips // [(amp, i)]
          progs' = progs // [(amp, newProg)]
      in runLoop phaseSeq' outputs amp' ips' progs'

-- Return new array storage, outputs so far and progress status
run :: [Int] -> Int -> Array Int Int -> IO (Array Int Int, [Int], Status)
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
      99 -> return (a, [], Finished)
      _ -> error "Invalid program"
 where
  binaryOp :: [Int] -> (Int -> Int -> Int) -> IO (Array Int Int, [Int], Status)
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

  storeInput :: IO (Array Int Int, [Int], Status)
  storeInput =
    case inputs of
      [] -> return (a, [], Pending i)
      first : rest ->
        run rest (i + 2) $ a // [((a ! (i + 1)) + 1, first)]

  output :: [Int] -> IO (Array Int Int, [Int], Status)
  output (mode : _) = do
    let pos = a ! (i + 1)
        val = if mode == 0 then a ! (pos + 1) else pos
    (newA, outputs, status) <- run inputs (i + 2) a
    return (newA, val : outputs, status)
  output _ = error "No parameter code for output instruction"

  jump :: [Int] -> (Int -> Bool) -> IO (Array Int Int, [Int], Status)
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
