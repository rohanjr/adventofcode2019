{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Array
import Control.Monad

main :: IO ()
main = do
  input <- T.getLine
  let strs = T.split (== ',') input
      nums :: [Int] = map (read . T.unpack) strs
  void $ runWhole nums

input :: Int
input = 1

runWhole :: [Int] -> IO [Int]
runWhole nums =
  let len = length nums
      numsArray = listArray (1, len) nums
  in elems <$> run 1 numsArray

run :: Int -> Array Int Int -> IO (Array Int Int)
run i a =
  let (opCode, paramModes) = splitInstr (a ! i)
  in case opCode of
      1 -> performOpAndCont paramModes (+)
      2 -> performOpAndCont paramModes (*)
      3 -> run (i + 2) $ a // [((a ! (i + 1)) + 1, input)]
      4 -> printAndCont paramModes
      99 -> return a
      _ -> error "Invalid program"
 where
  performOpAndCont :: [Int] -> (Int -> Int -> Int) -> IO (Array Int Int)
  performOpAndCont (mode1 : mode2 : _) op =
    let pos1 = a ! (i + 1)
        pos2 = a ! (i + 2)
        pos3 = (a ! (i + 3)) + 1
        val1 = if mode1 == 0 then a ! (pos1 + 1) else pos1
        val2 = if mode2 == 0 then a ! (pos2 + 1) else pos2
        val3 = val1 `op` val2
        newA = a // [(pos3, val3)]
    in run (i + 4) newA
  performOpAndCont _ _ = error "No parameter modes passed to operation"
  printAndCont :: [Int] -> IO (Array Int Int)
  printAndCont (mode : _) =
    let pos = a ! (i + 1)
        val = if mode == 0 then a ! (pos + 1) else pos
    in putStrLn (show val) >> run (i + 2) a
  printAndCont _ = error "No parameter code for output instruction"

splitInstr :: Int -> (Int, [Int])
splitInstr instr =
  let revInstr = reverse $ show instr
      opCode :: Int = read $ reverse $ take 2 revInstr
      paramModes :: [Int] = map (\c -> read [c]) (drop 2 revInstr) ++ [0, 0]
  in (opCode, paramModes)
