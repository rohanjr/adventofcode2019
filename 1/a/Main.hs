{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Foldable

main :: IO ()
main = do
  wholeFile <- getContents
  let ls = lines wholeFile
      nums :: [Int] = map read ls
      sol = getFuelReqs nums
  putStrLn $ show sol

getFuelReqs :: [Int] -> Int
getFuelReqs ms = foldl' (+) 0 $ map fuelReq ms
  where
    fuelReq :: Int -> Int
    fuelReq m = (m `div` 3) - 2
