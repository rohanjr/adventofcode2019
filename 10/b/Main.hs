{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Ord
import Data.Function
import Data.Functor
import Data.Ratio
import Data.Maybe
import Data.List
import Data.List.Index

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
  putStrLn $ show $ getTargetSum 200 asteroids

getTargetSum :: Int -> [(Int, Int)] -> Int
getTargetSum i ps =
  let targets = snd $ getStationAndTargets ps
      (tx, ty) = targets !! (i - 1)
  in tx * 100 + ty

getStationAndTargets :: [(Int, Int)] -> ((Int, Int), [(Int, Int)])
getStationAndTargets ps =
  let pointsByAngle = map (\p -> (p, getPointsByAngle ps p)) ps
      (station, targets) = maximumBy (comparing $ length . snd) pointsByAngle
  in (station, map fst $ sortByLaser targets)

sortByLaser :: [[((Int, Int), (Angle, Int))]] -> [((Int, Int), (Angle, Int))]
sortByLaser [] = []
sortByLaser pads =
  map head pads ++ sortByLaser (filter (not . null) (map tail pads))

-- Given list of all points and point of interest,
-- order the points by angle and distance from the point of interest.
-- Return a list of lists which is grouped by the angle.
getPointsByAngle :: [(Int, Int)] -> (Int, Int) -> [[((Int, Int), (Angle, Int))]]
getPointsByAngle ps p1 =
  groupBy (on (==) (fst . snd)) $ -- Group by angle
  sortBy (comparing snd) $ -- Sort by angle and then distance
  mapMaybe (\p2 -> (p2,) <$> angleWithDistSq p1 p2) ps

data Angle = VUp | VDown | HLeft | HRight | Diag Quadrant (Ratio Int)
  deriving (Eq, Show)

data Quadrant = Q1 | Q2 | Q3 | Q4
  deriving (Eq, Ord, Show)

-- Clockwise from the positive y axis
instance Ord Angle where
  a1 <= a2 = angleClockwise a1 <= angleClockwise a2

-- Calculate the angle and squared distance from point 1 to point 2.
-- Return Nothing if the points are the same (no meaningful angle in that case).
-- Note that we set up the coordinates with y values increasing downwards,
-- which affects the angle we calculate.
angleWithDistSq :: (Int, Int) -> (Int, Int) -> Maybe (Angle, Int)
angleWithDistSq (x1, y1) (x2, y2) = angle <&> (, distSq)
 where
  (dx, dy) = (abs (x2 - x1), abs (y2 - y1))
  distSq = dx * dx + dy * dy
  ratio = dy % dx -- Fetched lazily, so no runtime error when dx == 0
  angle = case (compare x1 x2, compare y1 y2) of
    (EQ, EQ) -> Nothing
    (EQ, LT) -> Just VDown
    (EQ, GT) -> Just VUp
    (LT, EQ) -> Just HRight
    (GT, EQ) -> Just HLeft
    (LT, GT) -> Just $ Diag Q1 ratio
    (LT, LT) -> Just $ Diag Q2 ratio
    (GT, LT) -> Just $ Diag Q3 ratio
    (GT, GT) -> Just $ Diag Q4 ratio

angleClockwise :: Angle -> Double
angleClockwise = \case
  VUp -> 0
  HRight -> 0.5 * pi
  VDown -> pi
  HLeft -> 1.5 * pi
  Diag q r ->
    let (num, denom) = (numerator r, denominator r)
        rr :: Double = fromIntegral num / fromIntegral denom
    in case q of
        Q1 -> 0.5 * pi - atan rr
        Q2 -> 0.5 * pi + atan rr
        Q3 -> 1.5 * pi - atan rr
        Q4 -> 1.5 * pi + atan rr
