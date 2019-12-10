{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.Ord
import Data.Maybe

main :: IO ()
main = do
  input <- getLine
  let output = decode 25 6 input
  putStrLn output

decode :: Int -> Int -> String -> String
decode w h input =
  let pixels :: [Int] = map (\c -> read [c]) input
      layers = splitLayers (w * h) pixels
      outputPixels = map firstOpaque $ align layers
      outputRows = splitLayers w outputPixels
  in unlines $ map (concatMap renderPixel) outputRows
 where
  firstOpaque :: [Int] -> Int
  firstOpaque = fromMaybe 2 . find (/= 2)

  renderPixel :: Int -> String
  renderPixel 0 = " "
  renderPixel 1 = "*"
  renderPixel _ = error "Unknown colour"

splitLayers :: Int -> [Int] -> [[Int]]
splitLayers res = unfoldr $ \pixels ->
  if res <= length pixels then Just (splitAt res pixels) else Nothing

align :: [[Int]] -> [[Int]]
align layers
  | [] <- head layers
  = [] -- no more pixels to layer
  | otherwise
  = let heads = map head layers
        tails = map tail layers
    in heads : align tails
