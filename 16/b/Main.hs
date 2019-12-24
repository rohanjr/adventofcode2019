module Main where

import System.Environment
import Data.List
import qualified Data.Vector.Unboxed as V

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
  let offset = read $ take 7 input
      ins = map readChar input
      m = length ins * 10000
      signal = V.fromList $ concat $ replicate 10000 ins
      output = fft n m signal
      message = V.toList $ V.slice offset 8 output
  in concatMap show message

-- Run `n` phases on input signal of length `m`.
fft :: Int -> Int -> V.Vector Int -> V.Vector Int
fft n m signal = (iterate' (fftPhase m) signal) !! n

-- Run one phase on the signal given as a vector.
fftPhase :: Int -> V.Vector Int -> V.Vector Int
fftPhase m signal = out
 where
  sums = V.scanl' (+) 0 signal
  out = V.generate m (\i -> fftDigit sums (i + 1) m)

fftDigit :: V.Vector Int -> Int -> Int -> Int
fftDigit sums k m =
  let sum = sumForDigit sums k m
  in abs sum `mod` 10

-- Get the sum for one digit of the FFT phase.
sumForDigit :: V.Vector Int -> Int -> Int -> Int
sumForDigit sums k m = go 0 1 k
 where
  go s sign i
    | i > m = s
    | otherwise =
        let thisSum = sign * sumRange sums i (min (i + k - 1) m)
        in go (s + thisSum) (sign * (-1)) (i + 2 * k)

-- Sum an inclusive range of the signal using the precomputed array of sums.
sumRange :: V.Vector Int -> Int -> Int -> Int
sumRange sums i j = (sums V.! j) - (sums V.! (i - 1))
