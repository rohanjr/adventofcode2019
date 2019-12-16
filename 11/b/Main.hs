{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  prog <- T.getLine
  let strs = T.split (== ',') prog
      nums :: [Int64] = map (read . T.unpack) strs
  painted <- runWhole nums
  renderMap painted

memoryLimit :: Int
memoryLimit = 1000000

renderMap :: Map.Map Pos Int64 -> IO ()
renderMap painted = do
  let ps = Map.keys painted
      (xs, ys) = unzip ps
      minX = minimum xs
      minY = minimum ys
      maxX = maximum xs
      maxY = maximum ys
      newPainted = Map.mapKeys (addPos (-minX, -minY)) painted
      dX = abs (maxX - minX)
      dY = abs (maxY - minY)
  forM_ (reverse [0..dY]) $ \y -> do
    forM_ [0..dX] $ \x ->
      putChar $ renderColour $ getColour (x, y) newPainted
    putChar '\n'
 where
  renderColour :: Int64 -> Char
  renderColour 0 = '.'
  renderColour 1 = '#'
  renderColour _ = ' '

runWhole :: [Int64] -> IO (Map.Map Pos Int64)
runWhole prog = do
  a <- V.replicate memoryLimit 0
  initialise prog a
  let initialRobotState = RobotState (0, 0) U Map.empty
  RobotState{..} <- runRobot initialRobotState 0 0 a
  return painted

type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data Dir = U | D | L | R

dirToVector :: Dir -> Pos
dirToVector U = (0, 1)
dirToVector D = (0, -1)
dirToVector L = (-1, 0)
dirToVector R = (1, 0)

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

data RobotState = RobotState
  { pos :: Pos
  , dir :: Dir
  , painted :: Map.Map Pos Int64
    -- ^ What the robot has painted
  }

getColour :: Pos -> Map.Map Pos Int64 -> Int64
getColour pos painted =
  fromMaybe (initialColour pos) (Map.lookup pos painted)
 where
  initialColour :: Pos -> Int64
  initialColour (0, 0) = 1
  initialColour _ = 0

runRobot :: RobotState -> Int64 -> Int64 -> V.MVector (PrimState IO) Int64 -> IO RobotState
runRobot robotState@RobotState{..} i rb a = do
  let colour = getColour pos painted
  runState1 <- run (repeat colour) i rb a
  case runState1 of
    Finished -> return robotState
    Pending newColour i2 rb2 -> do
      let robotState2 = robotState { painted = Map.insert pos newColour painted }
      runState2 <- run (repeat newColour) i2 rb2 a
      case runState2 of
        Finished -> return robotState2
        Pending rot i3 rb3 -> do
          let newDir = turn rot dir
              newPos = addPos pos (dirToVector newDir)
              robotState3 = robotState2 { pos = newPos, dir = newDir }
          runRobot robotState3 i3 rb3 a

initialise :: [Int64] -> V.MVector (PrimState IO) Int64 -> IO ()
initialise = fill 0
 where
  fill :: Int -> [Int64] -> V.MVector (PrimState IO) Int64 -> IO ()
  fill i elems v
    | i >= V.length v || null elems
    = return ()
    | e : es <- elems
    = V.write v i e >> fill (i + 1) es v

data RunState
  = Finished
  | Pending
    { outVal :: Int64
    , instrP :: Int64
    , relBase :: Int64
    }

run :: [Int64] -> Int64 -> Int64 -> V.MVector (PrimState IO) Int64 -> IO RunState
run inputs i relBase a = do
  instr <- V.read a (fromIntegral i)
  let (opCode, paramModes) = splitInstr instr
  case opCode of
    1 -> binaryOp paramModes (+)
    2 -> binaryOp paramModes (*)
    3 -> storeInput paramModes
    4 -> output paramModes
    5 -> jump paramModes (/= 0)
    6 -> jump paramModes (== 0)
    7 -> binaryOp paramModes (\x y -> if x < y then 1 else 0)
    8 -> binaryOp paramModes (\x y -> if x == y then 1 else 0)
    9 -> relBaseOffset paramModes
    99 -> return Finished
    _ -> error "Invalid program"
 where
  -- Interpret a parameter using its parameter mode.
  evalParam :: Int -> Int64 -> IO Int64
  evalParam mode pos = do
    param <- V.read a (fromIntegral pos)
    case mode of
      0 -> V.read a (fromIntegral param)
      1 -> return param
      2 -> V.read a $ fromIntegral (relBase + param)
      _ -> error "Unknown parameter mode"

  evalWriteParam :: Int -> Int64 -> IO Int64
  evalWriteParam mode pos = do
    param <- V.read a (fromIntegral pos)
    return $ case mode of
      0 -> param
      1 -> error "Write parameter cannot be in immediate mode"
      2 -> relBase + param
      _ -> error "Unknown parameter mode"

  binaryOp :: [Int] -> (Int64 -> Int64 -> Int64) -> IO RunState
  binaryOp (mode1 : mode2 : mode3 : _) op = do
    val1 <- evalParam mode1 (i + 1)
    val2 <- evalParam mode2 (i + 2)
    writePos <- evalWriteParam mode3 (i + 3)
    let val = val1 `op` val2
    V.write a (fromIntegral writePos) val
    run inputs (i + 4) relBase a
  binaryOp _ _ = error "Not enough parameter modes passed to binary operation"

  storeInput :: [Int] -> IO RunState
  storeInput (mode : _) =
    case inputs of
      [] -> error "No input value to store"
      first : rest -> do
        writePos <- evalWriteParam mode (i + 1)
        V.write a (fromIntegral writePos) first
        run rest (i + 2) relBase a
  storeInput _ = error "No parameter modes passed to store instruction"

  output :: [Int] -> IO RunState
  output (mode : _) = do
    val <- evalParam mode (i + 1)
    return $ Pending
      { outVal = val
      , instrP = i + 2
      , relBase
      }
  output _ = error "No parameter modes passed to output instruction"

  jump :: [Int] -> (Int64 -> Bool) -> IO RunState
  jump (mode1 : mode2 : _) cond = do
    val1 <- evalParam mode1 (i + 1)
    val2 <- evalParam mode2 (i + 2)
    let newI = if cond val1 then val2 else i + 3
    run inputs newI relBase a
  jump _ _ = error "Not enough parameter modes passed to jump instruction"

  relBaseOffset :: [Int] -> IO RunState
  relBaseOffset (mode : _) = do
    offset <- evalParam mode (i + 1)
    run inputs (i + 2) (relBase + offset) a

splitInstr :: Int64 -> (Int, [Int])
splitInstr instr =
  let revInstr = reverse $ show instr
      opCode :: Int = read $ reverse $ take 2 revInstr
      paramModes :: [Int] = map (\c -> read [c]) (drop 2 revInstr) ++ repeat 0
  in (opCode, paramModes)
