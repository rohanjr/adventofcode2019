{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char
import Data.Maybe
import Data.List.Split
import Control.Monad.Trans.State.Lazy
import Data.List.Index
import qualified Data.Set as Set

import Intcode

main :: IO ()
main = do
  input <- getContents
  let prog = map read $ splitOn "," input
  s0 :: ProgStateM <- initMem 1000000 prog
  (outs, _) <- evalStateT (runToEnd []) s0
  let outChars = map (chr . fromIntegral) outs
  --putStr outChars
  --putStrLn $ show $ getIntersections $ parseMap outChars
  putStrLn $ show $ sumAlignmentParams $ parseMap outChars
  return ()

parseMap :: [Char] -> Set.Set (Int, Int)
parseMap cs =
  let ls = lines cs in
  Set.fromList $ concat $
    flip imap ls $ \y l ->
      catMaybes $ flip imap l $ \x c ->
        if c `elem` ['#', '^', 'v', '<', '>']
          then Just (x, y)
          else Nothing

getIntersections :: Set.Set (Int, Int) -> Set.Set (Int, Int)
getIntersections s =
  flip Set.filter s $ \(x, y) ->
    Set.member (x-1, y) s && Set.member (x+1, y) s &&
    Set.member (x, y-1) s && Set.member (x, y+1) s

sumAlignmentParams :: Set.Set (Int, Int) -> Int
sumAlignmentParams s =
  Set.foldl' (+) 0 $ Set.map (uncurry (*)) $ getIntersections s
