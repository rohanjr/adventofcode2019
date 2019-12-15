{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Ratio
import Data.Maybe
import Data.List
import Data.List.Index
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
      asteroids =
        concat $ flip imap ls $ \y l ->
          catMaybes $ flip imap l $ \x c ->
            case c of
              '.' -> Nothing
              '#' -> Just (x, y)
              _ -> error "Unknown space object"
  putStrLn $ show $ mostAngles asteroids

mostAngles :: [(Int, Int)] -> Int
mostAngles ps = maximum $ map Set.size $ getAngles ps

getAngles :: [(Int, Int)] -> [Set.Set Angle]
getAngles ps =
  flip map ps $ \p ->
    Set.delete None $ Set.fromList $
    map (angle p) ps

data Angle = None | VUp | VDown | HLeft | HRight | Diag Quadrant (Ratio Int)
  deriving (Eq, Ord, Show)

data Quadrant = Q1 | Q2 | Q3 | Q4
  deriving (Eq, Ord, Show)

angle :: (Int, Int) -> (Int, Int) -> Angle
angle (x1, y1) (x2, y2) =
  case (compare x1 x2, compare y1 y2) of
    (EQ, EQ) -> None
    (EQ, LT) -> VUp
    (EQ, GT) -> VDown
    (LT, EQ) -> HRight
    (GT, EQ) -> HLeft
    (LT, LT) -> Diag Q1 getRatio
    (GT, LT) -> Diag Q2 getRatio
    (GT, GT) -> Diag Q3 getRatio
    (LT, GT) -> Diag Q4 getRatio
 where
  -- Fetched lazily, so no runtime errors
  getRatio :: Ratio Int
  getRatio = abs (y2 - y1) % abs (x2 - x1)
