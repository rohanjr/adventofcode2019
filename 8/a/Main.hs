{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.Ord

main :: IO ()
main = do
  input <- getLine
  let pixels :: [Int] = map (\c -> read [c]) input
  putStrLn $ show $ check 25 6 pixels

check :: Int -> Int -> [Int] -> Int
check w h pixels =
  let layers = splitLayers (w * h) pixels
      l = minimumBy (comparing $ countNum 0) layers
  in countNum 1 l * countNum 2 l
 where
  countNum :: Int -> [Int] -> Int
  countNum n = length . filter (== n)

splitLayers :: Int -> [Int] -> [[Int]]
splitLayers res = unfoldr $ \pixels ->
  if res <= length pixels then Just (splitAt res pixels) else Nothing


