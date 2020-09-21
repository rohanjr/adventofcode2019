{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Char
import Data.Maybe
import Data.List
import Data.List.Index
import Data.Array
import Data.Ord
import qualified Data.Set as Set
import qualified Data.PQueue.Min as Q

main :: IO ()
main = do
  input <- getContents
  let maze = makeMaze input
  putStr $ show $ search maze
  return ()

data Maze = Maze
  { grid :: Array (Int, Int) Char
  , numKeys :: Int
  , entrance :: (Int, Int)
  }
  deriving Show

makeMaze :: String -> Maze
makeMaze input = case lines input of
  [] -> error "No maze input"
  ls@(l1:_) ->
    let grid = array ((1, 1), (length l1, length ls)) $
                concat $ flip imap ls $ \y row ->
                  flip imap row $ \x val -> ((x+1, y+1), val)
        numKeys = length $ filter isLower input
        entrance = fst $ fromJust $ find (\(_i, v) -> v == '@') $ assocs grid
    in Maze{..}

data MazeState = MazeState
  { loc :: (Int, Int)
  , keys :: Set.Set Char
  }
  deriving (Eq, Ord, Show)

data MazeStateWithSteps = MazeStateWithSteps
  { state :: MazeState
  , steps :: Int
  }
  deriving (Eq, Show)

instance Ord MazeStateWithSteps where
  compare = comparing $
    \MazeStateWithSteps{..} -> (steps, Down $ Set.size $ keys state)

search :: Maze -> Maybe Int
search Maze{..} = go Set.empty (Q.singleton initState)
 where
  initState :: MazeStateWithSteps
  initState = MazeStateWithSteps{ state = MazeState entrance Set.empty, steps = 0 }
  go :: Set.Set MazeState -> Q.MinQueue MazeStateWithSteps -> Maybe Int
  go visited qq = case Q.minView qq of
    Nothing -> Nothing
    Just (MazeStateWithSteps state@MazeState{..} steps, q)
      | Set.size keys == numKeys
      -> Just steps
      | otherwise
      -> go (Set.insert state visited) $
          foldl' (flip Q.insert) q
            [ MazeStateWithSteps newState (steps + 1) | newState <- getSuccs visited state ]
  getSuccs :: Set.Set MazeState -> MazeState -> [MazeState]
  getSuccs visited MazeState{..} =
    flip mapMaybe dirs $ \dir ->
      let newLoc = addPos loc dir
          v = grid ! newLoc
          newState = MazeState{ loc = newLoc, keys }
          isWall = v == '#' || isUpper v && Set.notMember (toLower v) keys
          isNewKey = isLower v && Set.notMember v keys
      in if | isWall || Set.member newState visited
            -> Nothing
            | isNewKey
            -> Just newState{ keys = Set.insert v keys }
            | otherwise
            -> Just newState

dirs :: [(Int, Int)]
dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
