{-# LANGUAGE TupleSections #-}

module Main where

import Data.Int
import Data.Char
import Data.List
import Data.List.Split

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
findRepeat s0 =
  let sC = findStateInCycle s0
      m = findCycleLength sC
      k = findCycleStart m s0
  in (k, m)

-- Given an initial state, use the `step` function to find a state within its
-- cycle (assuming a cycle exists).
findStateInCycle :: State -> State
findStateInCycle s0 = go s0 s0
 where
  -- Use a tortoise-and-hare approach to find a state in the cycle,
  -- i.e. advance at single speed and double speed until we reach equal states.
  go s1 s2 =
    let (s1', s2') = (step s1, step (step s2))
    in if s1' == s2' then s1' else go s1' s2'

-- Given a state we assume is in a cycle, find the length of that cycle,
-- simply by iterating the step function until we reach the same state.
findCycleLength :: State -> Int64
findCycleLength s0 = go 0 s0
 where
  go i s =
    let (i', s') = (i + 1, step s)
    in if s' == s0 then i' else go i' s'

-- Given an initial state and the length of a cycle, determine the start of the
-- cycle, specifically the number of steps to reach the start of the cycle.
findCycleStart :: Int64 -> State -> Int64
findCycleStart m s0 =
  -- Advance two states from 0 steps and m steps until we find them equal.
  let sM = iter m s0
  in go 0 s0 sM
 where
  iter n s = if n <= 0 then s else iter (n - 1) (step s)
  go i s1 s2 = if s1 == s2 then i else go (i + 1) (step s1) (step s2)

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
