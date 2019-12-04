{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Data.List
import Data.Ord

type Path = [(Char, Int)]
type Line = ((Int, Int), (Int, Int)) -- in order of varying component

main :: IO ()
main = do
  line1 <- T.getLine
  line2 <- T.getLine
  let path1 = parseLine line1
      path2 = parseLine line2
      sol = manhattanDistance $ findClosestIntersection path1 path2
  putStrLn $ show sol

parseLine :: T.Text -> [(Char, Int)]
parseLine line =
  let strs = T.split (== ',') line
  in map parseMove strs
 where
  parseMove :: T.Text -> (Char, Int)
  parseMove move =
    let (dir, lenStr) = fromJust $ T.uncons move
        len :: Int = read $ T.unpack lenStr
    in (dir, len)

pathToLines :: (Int, Int) -> Path -> [Line]
pathToLines (x, y) path =
  case path of
    [] -> []
    (dir, len) : rest ->
      let newPos =
            case dir of
              'L' -> (x - len, y)
              'R' -> (x + len, y)
              'D' -> (x, y - len)
              'U' -> (x, y + len)
          newLine =
            case dir of
              'L' -> (newPos, (x, y))
              'R' -> ((x, y), newPos)
              'D' -> (newPos, (x, y))
              'U' -> ((x, y), newPos)
      in newLine : pathToLines newPos rest

isHorizontal :: Line -> Bool
isHorizontal ((x1, y1), (x2, y2)) = y1 == y2

findHorizVertIntersection :: Line -> Line -> Maybe (Int, Int)
findHorizVertIntersection ((hx1, hy1), (hx2, hy2)) ((vx1, vy1), (vx2, vy2)) =
  if hy1 >= vy1 && hy1 <= vy2 && vx1 >= hx1 && vx1 <= hx2
    then Just (vx1, hy1)
    else Nothing

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

findClosestIntersection :: Path -> Path -> (Int, Int)
findClosestIntersection path1 path2 =
  let (horiz1, vert1) = partition isHorizontal $ pathToLines (0, 0) path1
      (horiz2, vert2) = partition isHorizontal $ pathToLines (0, 0) path2
      -- Check if horizontal lines on one path intersect with vertical lines on the
      -- other path and vice versa.
      lineCombos = [ (h, v) | h <- horiz1, v <- vert2 ] ++ [ (h, v) | h <- horiz2, v <- vert1 ]
      intersections = mapMaybe (uncurry findHorizVertIntersection) lineCombos
      intersectionsNonZero = filter (/= (0, 0)) intersections
  in minimumBy (comparing manhattanDistance) intersectionsNonZero
