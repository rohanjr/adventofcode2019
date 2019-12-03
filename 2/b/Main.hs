{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Array
import Data.List

main :: IO ()
main = do
  input <- T.getLine
  let strs = T.split (== ',') input
      nums :: [Int] = map (read . T.unpack) strs
      sol = case findNounVerb 19690720 nums of
              Nothing -> "No noun/verb combination found"
              Just (noun, verb) -> show (100 * noun + verb)
  putStrLn sol

findNounVerb :: Int -> [Int] -> Maybe (Int, Int)
findNounVerb target nums =
  find
    (\(noun, verb) -> tweakAndRun noun verb nums == target)
    [(noun, verb) | noun <- [0..99], verb <- [0..99]]

tweakAndRun :: Int -> Int -> [Int] -> Int
tweakAndRun noun verb nums =
  let len = length nums
      numsArray = listArray (1, len) nums
      tweakedArray = tweak noun verb numsArray
      finishedProgram = run 1 tweakedArray
  in finishedProgram ! 1

tweak :: Int -> Int -> Array Int Int -> Array Int Int
tweak noun verb a = a // zip (map (+1) [1, 2]) [noun, verb]

run :: Int -> Array Int Int -> Array Int Int
run i a =
  case a ! i of
    1 -> performOpAndCont (+)
    2 -> performOpAndCont (*)
    99 -> a
    _ -> error "Invalid program"
 where
  performOpAndCont :: (Int -> Int -> Int) -> Array Int Int
  performOpAndCont op =
    let pos1 = (a ! (i + 1)) + 1
        pos2 = (a ! (i + 2)) + 1
        pos3 = (a ! (i + 3)) + 1
        val1 = a ! pos1
        val2 = a ! pos2
        val3 = op val1 val2
        newA = a // [(pos3, val3)]
    in run (i + 4) newA

