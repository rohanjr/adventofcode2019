module Main where

import System.Environment
import Data.Char
import Data.List.Split
import Linear.V3
import Control.Monad.Zip

main :: IO ()
main = do
  [arg] <- getArgs
  ls <- lines <$> getContents
  let numSteps = read arg
      ps = map parsePosition ls
      energy = energyAfterSteps numSteps ps
  putStrLn $ show energy

-- State of moons, either positions or velocities
type Moons = [V3 Int]
-- State of moons in both positions and velocities
type State = (Moons, Moons)

parsePosition :: String -> V3 Int
parsePosition s =
  case map read $ wordsBy (\c -> not $ isDigit c || c == '-') s of
    [x, y, z] -> V3 x y z
    _ -> error "Cannot parse moon position"

energyAfterSteps :: Int -> Moons -> Int
energyAfterSteps n ps = totalEnergy $ stepN n (ps, initialVs)
 where
  -- Initially all moons are stationary
  initialVs :: Moons
  initialVs = replicate 4 (V3 0 0 0)

totalEnergy :: State -> Int
totalEnergy (ps, vs) = sum $ zipWith moonEnergy ps vs
 where
  moonEnergy :: V3 Int -> V3 Int -> Int
  moonEnergy p v = sumAbs p * sumAbs v
  sumAbs :: V3 Int -> Int
  sumAbs = sum . fmap abs

-- Calculate the state of the moons `n` steps after an initial state `s`.
stepN :: Int -> State -> State
stepN n s = (iterate step s) !! n

-- Calculate the state of the moons after one time step.
step :: State -> State
step (ps, vs) =
  let as = map (getAccel ps) ps
      newVs = zipWith (+) vs as
      newPs = zipWith (+) ps newVs
  in (newPs, newVs)

-- Calculate the acceleration (change in velocity) to be applied to a moon with
-- position `p` due to the other moons with positions `ps`.
-- Note that we can include `p` in `ps` as it simply adds `0` to the result.
getAccel :: Moons -> V3 Int -> V3 Int
getAccel ps p = sum $ map (getA p) ps
 where
  getA :: V3 Int -> V3 Int -> V3 Int
  getA = mzipWith diff
  diff :: Int -> Int -> Int
  diff c1 c2 = signum (c2 - c1)
