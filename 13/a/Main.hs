{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Trans.State.Lazy

import Intcode

main :: IO ()
main = do
  progText <- T.getLine
  let cells = T.split (== ',') progText
      prog = map (read . T.unpack) cells
  outputs <- runWhole prog
  let tiles = parseTiles outputs
  putStrLn $ show $ countBlocks tiles

type Tile = (Int, Int, Int)

countBlocks :: [Tile] -> Int
countBlocks = length . filter isBlock
 where
  isBlock :: Tile -> Bool
  isBlock (_, _, 2) = True
  isBlock _ = False

parseTiles :: [Int64] -> [Tile]
parseTiles = go . map fromIntegral
 where
  go :: [Int] -> [Tile]
  go (x : y : t : ts) =
    (x, y, t) : go ts
  go _ = []

runWhole :: [Int64] -> IO [Int64]
runWhole prog = do
  startState :: ProgStateM <- initMem memoryLimit prog
  (outs, _endState) <- evalStateT (runToEnd []) startState
  return outs
 where
  memoryLimit :: Int
  memoryLimit = 1000000
