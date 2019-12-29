{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Int
import Data.Maybe
import Data.List
import Data.Tuple.Extra
import Linear.V2
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Functor.Identity
import Control.Monad.Trans.State.Lazy

import IntCode

main :: IO ()
main = do
  progText <- T.getLine
  let cells = T.split (== ',') progText
      prog = map (read . T.unpack) cells
  putStrLn $ show $ runDroid prog

memoryLimit :: Int
memoryLimit = 1000000

type Pos = V2 Int
type Dir = Int

-- Control the droid to find the minimum number of moves
-- to reach the goal position.
runDroid :: [Int64] -> Int
runDroid prog =
  let s0 = initialise memoryLimit prog
      oxygenState = runToGoal 0 (Set.singleton $ V2 0 0) $ Seq.singleton (V2 0 0, 0, s0)
  in case oxygenState of
      Left _ -> error "No way to reach oxygen system!"
      Right (p, _, s1) ->
        case runToGoal 0 (Set.singleton p) $ Seq.singleton (p, 0, s1) of
          Left dist -> dist
          Right _ -> error "Found another source of oxygen?"

-- Current position, distance from starting position, droid program state.
type DroidState = (Pos, Int, ProgState)

-- Run the droid for two purposes:
-- 1. To find the oxygen (indicated by status code 2),
--    returning the state at the point you reach it.
-- 2. To fill up the space with oxygen, returning the distance
--    (number of moves on the map) to the last position to get filled.
runToGoal :: Int -> Set.Set Pos -> Seq.Seq DroidState -> Either Int DroidState
runToGoal dist visited (Seq.viewl -> toVisit) =
  case toVisit of
    Seq.EmptyL -> Left dist
    (pos, numMoves, progState) Seq.:< restToVisit ->
      let nextStates = flip map [1..4] $ \dir ->
            let (status, newProgState) = runStep dir progState
            in (status, movePos dir pos, newProgState)
      in case find (\s -> fst3 s == 2) nextStates of
          Just (2, p, st)
            | p `Set.notMember` visited ->
              -- Need to check whether we've visited `p` for when
              -- we're running in oxygen filling mode.
                Right (p, numMoves + 1, st) -- state at the oxygen
          _ ->
            let nextPs = flip mapMaybe nextStates $ \(status, nextPos, nextProgSt) ->
                  if status == 1 && nextPos `Set.notMember` visited
                    then Just (nextPos, numMoves + 1, nextProgSt)
                    else Nothing
                visitedNow = foldl' (flip Set.insert) visited (map fst3 nextPs)
                toVisitNow = restToVisit Seq.>< Seq.fromList nextPs
            in runToGoal numMoves visitedNow toVisitNow

movePos :: Dir -> Pos -> Pos
movePos dir pos = pos + dirToPos dir
 where
  dirToPos :: Dir -> Pos
  dirToPos = \case
    1 -> V2 0 1
    2 -> V2 0 (-1)
    3 -> V2 (-1) 0
    4 -> V2 1 0
    _ -> error $ "Unknown direction" <> show dir

runStep :: Int -> ProgState -> (Int64, ProgState)
runStep dir progState =
  runIdentity $ flip runStateT progState $ do
    out <- run [fromIntegral dir]
    return $ getOutput out

getOutput :: RunState -> Int64
getOutput Finished = error "Repair droid halted but should run forever"
getOutput (Output val) = val
