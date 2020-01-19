{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Environment
import Data.Int
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import System.Console.Haskeline

import Intcode

main :: IO ()
main = do
  progFile : args <- getArgs
  progText <- withFile progFile ReadMode T.hGetLine
  let cells = T.split (== ',') progText
      prog = map (read . T.unpack) cells
      progFree = 2 : tail prog
  case args of
    [] -> do
      score <- runWhole progFree
      putStrLn $ show score
    ["-i"] -> runWholeInteractive progFree
    _ -> error "Unknown command line arguments"

memoryLimit :: Int
memoryLimit = 1000000

type Tile = ((Int, Int), Int)

parseTiles :: [Int64] -> [Tile]
parseTiles = go . map fromIntegral
 where
  go :: [Int] -> [Tile]
  go (x : y : t : rest) = ((x, y), t) : go rest
  go _ = []

data PlayerState = PlayerState
  { paddlePos :: (Int, Int)
  , ballPos :: (Int, Int)
  , score :: Int
  }

runBot :: ProgStM Int
runBot = go dummyPlayerState []
 where
  dummyPlayerState :: PlayerState
  dummyPlayerState = PlayerState (0, 0) (0, 0) 0

  go :: PlayerState -> [Int64] -> ProgStM Int
  go PlayerState{..} inputs = do
    (outs, endState) <- runToEnd inputs
    let tiles = parseTiles outs
        newScore = fromMaybe score $ lookup (-1, 0) tiles
    case endState of
      Halted -> return newScore
      AwaitingInput -> do
        -- Always move the paddle in the direction of the ball on the x-axis
        let newPaddlePos = maybe ballPos fst $ find ((== 3) . snd) tiles
            newBallPos = maybe ballPos fst $ find ((== 4) . snd) tiles
            move = fromIntegral $ signum $ fst newBallPos - fst newPaddlePos
        go (PlayerState newPaddlePos newBallPos newScore) [move]

runWhole :: [Int64] -> IO Int
runWhole prog = do
  ps0 <- initMem memoryLimit prog
  evalStateT runBot ps0

-- The rest is for playing the game interactively

showTiles :: Map.Map (Int, Int) Int -> String
showTiles tiles =
  -- Hardcode the dimensions of the map for now
  unlines $ flip map [0..25] $ \y ->
    flip map [0..39] $ \x ->
      showTile $ fromJust $ Map.lookup (x, y) tiles
 where
  showTile :: Int -> Char
  showTile = \case
    0 -> ' '
    1 -> '#'
    2 -> '*'
    3 -> '_'
    4 -> 'o'
    t -> error $ "Unknown tile id " <> show t

showGameState :: Map.Map (Int, Int) Int -> IO ()
showGameState tiles = do
  putStr $ showTiles tiles
  putStrLn $ "Score: " <> show score
 where
  score = fromJust $ Map.lookup (-1, 0) tiles

readJoystick :: IO Char
readJoystick = runInputT defaultSettings $ fromJust <$> getInputChar "Move: "

parseMove :: Char -> Int64
parseMove = \case
  'h' -> -1 -- move left
  'l' -> 1  -- move right
  _ -> 0    -- any other keystroke is a neutral move

runInteractive :: ProgStM ()
runInteractive = go Map.empty []
 where
  go :: Map.Map (Int, Int) Int -> [Int64] -> ProgStM ()
  go tiles inputs = do
    (outs, endState) <- runToEnd inputs
    let newTiles = foldl' (\ts (p, t) -> Map.insert p t ts) tiles (parseTiles outs)
    liftIO $ showGameState newTiles
    case endState of
      Halted -> return ()
      AwaitingInput -> do
        c <- liftIO readJoystick
        go newTiles [parseMove c]

runWholeInteractive :: [Int64] -> IO ()
runWholeInteractive prog = do
  ps0 <- initMem memoryLimit prog
  evalStateT runInteractive ps0
