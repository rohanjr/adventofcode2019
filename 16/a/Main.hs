module Main where

import System.Environment
import Data.List

main :: IO ()
main = do
  [nStr] <- getArgs
  input <- getLine
  let n = read nStr
      output = fftMain n input
  putStrLn output

readChar :: Char -> Int
readChar c = read [c]

fftMain :: Int -> String -> String
fftMain n input =
  let ins = map readChar input
      output = fft n ins
  in concatMap show $ take 8 output

-- Run `n` phases on input signal
fft :: Int -> [Int] -> [Int]
fft n signal =
  (iterate' fftPhase signal) !! n

fftPhase :: [Int] -> [Int]
fftPhase ins =
  let n = length ins
  in map (\k -> fftDigit k ins) [1..n]

fftDigit :: Int -> [Int] -> Int
fftDigit k ins =
  let pattern = tail $ cycle $ concatMap (replicate k) [0, 1, 0, -1]
  in (`mod` 10) $ abs $ sum $ zipWith (*) ins pattern
