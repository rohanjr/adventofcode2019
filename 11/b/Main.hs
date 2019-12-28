{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Int
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Trans.State.Lazy

import IntCode

main :: IO ()
main = do
  progText <- T.getLine
  let cells = T.split (== ',') progText
      prog = map (read . T.unpack) cells
  painted <- runWhole prog
  renderMap painted

renderMap :: Map.Map Pos Int64 -> IO ()
renderMap painted = do
  let (xs, ys) = unzip $ Map.keys painted
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
  forM_ (reverse [minY..maxY]) $ \y -> do
    forM_ [minX..maxX] $ \x ->
      putChar $ renderColour $ getColour (x, y) painted
    putChar '\n'
 where
  renderColour :: Int64 -> Char
  renderColour 0 = '.'
  renderColour 1 = '#'
  renderColour _ = ' '

memoryLimit :: Int
memoryLimit = 1000000

runWhole :: [Int64] -> IO (Map.Map Pos Int64)
runWhole prog = do
  let initialRobotState = RobotState (0, 0) U Map.empty
  initialProgState <- initialise memoryLimit prog
  RobotState{..} <- evalStateT (runRobot initialRobotState) initialProgState
  return  painted

getColour :: Pos -> Map.Map Pos Int64 -> Int64
getColour pos painted =
  fromMaybe (initialColour pos) (Map.lookup pos painted)
 where
  initialColour :: Pos -> Int64
  initialColour (0, 0) = 1
  initialColour _ = 0

runRobot :: RobotState -> ProgSt RobotState
runRobot robotState@RobotState{..} = do
  let colour = getColour pos painted
  runState1 <- run (repeat colour)
  case runState1 of
    Finished -> return robotState
    Output newColour -> do
      let robotState2 = robotState { painted = Map.insert pos newColour painted }
      runState2 <- run (repeat newColour)
      case runState2 of
        Finished -> return robotState2
        Output rot -> do
          let newDir = turn rot dir
              newPos = addPos pos (dirToPos newDir)
              robotState3 = robotState2 { pos = newPos, dir = newDir }
          runRobot robotState3

data RobotState = RobotState
  { pos :: Pos
  , dir :: Dir
  , painted :: Map.Map Pos Int64
  }

type Pos = (Int, Int)
data Dir = U | D | L | R

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dirToPos :: Dir -> Pos
dirToPos U = (0, 1)
dirToPos D = (0, -1)
dirToPos L = (-1, 0)
dirToPos R = (1, 0)

turn :: Int64 -> Dir -> Dir
turn 0 U = L
turn 0 L = D
turn 0 D = R
turn 0 R = U
turn 1 U = R
turn 1 L = U
turn 1 D = L
turn 1 R = D
turn _ _ = error "Undefined rotation number"
