{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Int
import Linear.V2
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Trans.State.Lazy
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Functor.Identity

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
      numMoves = runToGoal Set.empty $ Seq.singleton (V2 0 0, 0, s0)
  in case numMoves of
      Nothing -> error "No way to reach oxygen system!"
      Just n -> n

runToGoal :: Set.Set Pos -> Seq.Seq (Pos, Int, ProgState) -> Maybe Int
runToGoal visited (Seq.viewl -> toVisit) =
  case toVisit of
    Seq.EmptyL -> Nothing
    (pos, numMoves, progState) Seq.:< restToVisit ->
      if pos `Set.member` visited
        then runToGoal visited restToVisit
        else
          let visitedNow = Set.insert pos visited
              nextStates = flip map [1..4] $ \dir ->
                let (status, newProgState) = runStep dir progState
                in (movePos dir pos, status, newProgState)
          in if any (\s -> snd3 s == 2) nextStates -- found the goal state
              then Just (numMoves + 1)
              else
                let nextPs = flip mapMaybe nextStates $ \(p, status, progSt) ->
                      if status == 1 then Just (p, numMoves + 1, progSt) else Nothing
                    toVisitNow = restToVisit Seq.>< Seq.fromList nextPs
                in runToGoal visitedNow toVisitNow

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
