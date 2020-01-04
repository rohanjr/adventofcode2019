{-# LANGUAGE TupleSections #-}

module Main where

import Data.Int
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map

main :: IO ()
main = do
  ls <- lines <$> getContents
  let ps = map parsePosition ls
  putStrLn $ show $ stepsToRepeatedState ps

parsePosition :: String -> [Int]
parsePosition s =
  let p = map read $ wordsBy (\c -> not $ isDigit c || c == '-') s
  in if length p == 3
      then p
      else error $ "Point " <> s <> " should have 3 dimensions."

stepsToRepeatedState :: [[Int]] -> Int64
stepsToRepeatedState ps =
  let -- Organise positions by dimension instead of by moon
      psT = transpose ps
      -- Find (k, m) for each dimension as explained below
      (ks, ms) = unzip $ map (findRepeat . (, initialVs)) psT
      -- The number of steps to repeat a state in all dimensions is the sum of:
      k = maximum ks -- the number of steps until every dimension is in a cycle
      m = foldl lcm 1 ms -- the combination (lowest common multiple) of cycle lengths
  in k + m
 where
  -- Initial velocity (0) for all moons in a single component
  initialVs :: [Int]
  initialVs = replicate 4 0

-- Positions and velocities of all moons in a single dimension.
type State = ([Int], [Int])

-- Find the number of steps at which the state (in one dimension) is repeated.
-- Return a pair (k, m) where k is the number of steps to reach that state for
-- the first time, and m is the number of steps it takes to cycle back to it.
-- So (k + m) is the total number of steps to reach the repeated state, but we
-- will need k and m separately for combining the results from different components.
findRepeat :: State -> (Int64, Int64)
findRepeat = go 0 Map.empty
 where
  -- Keep track of every state we've seen so far and the number of steps it took
  -- to reach it in a map.
  -- Beware that this solution uses memory proportional to (k + m), which might
  -- not be satisfactory in general.
  go :: Int64 -> Map.Map State Int64 -> State -> (Int64, Int64)
  go i seen s =
    case Map.lookup s seen of
      Nothing -> go (i + 1) (Map.insert s i seen) (step s)
      Just k -> (k, i - k)

-- Given the positions and velocities of the moons in a single dimension,
-- calculate the new values after one time step.
step :: State -> State
step (ps, vs) = (newPs, newVs)
 where
  as = map (getAccel ps) ps
  newVs = zipWith (+) vs as
  newPs = zipWith (+) ps newVs

-- Calculate the acceleration (change in velocity) to be applied to point `p`
-- (in a single dimension) due to the other points `ps`.
-- Note that we can include `p` in `ps` as it simply adds `0` to the result.
getAccel :: [Int] -> Int -> Int
getAccel ps p =
  sum $ map (diff p) ps
 where
  diff :: Int -> Int -> Int
  diff c1 c2 = signum (c2 - c1)
