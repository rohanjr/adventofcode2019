{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  , steps :: Int
  }
  deriving (Eq, Show)

instance Ord MazeState where
  compare = comparing steps

search :: Maze -> Int
search Maze{..} = go (Q.singleton initState)
 where
  initState :: MazeState
  initState = MazeState entrance Set.empty 0
  go :: Q.MinQueue MazeState -> Int
  go q = case Q.minView q of
    Nothing -> error "No solution found"
    Just (s@MazeState{..}, q)
      | Set.size keys == numKeys
      -> steps
      | otherwise
      -> go $ foldl' (flip Q.insert) q $ getSuccs s
  getSuccs :: MazeState -> [MazeState]
  getSuccs s@MazeState{..} =
    flip mapMaybe dirs $ \dir ->
      let nextState = s{ loc = addPos loc dir, steps = steps + 1 }
      in if isWall nextState then Nothing else moveToNextBranch nextState loc
  moveToNextBranch :: MazeState -> (Int, Int) -> Maybe MazeState
  moveToNextBranch s@MazeState{..} prevLoc =
    case grid ! loc of
      v | isLower v && Set.notMember v keys
        -- v is a key, and you didn't have it before
        -> Just MazeState{ loc, keys = Set.insert v keys, steps }
        | otherwise
        -> let isValidLoc l = l /= prevLoc && not (isWall s{ loc = l })
              --  isValidState newS@(MazeState newLoc _ _) =
              --    newLoc /= prevLoc && not (isWall newS)
               newLocs = filter isValidLoc $ map (addPos loc) dirs
           in case newLocs of
             -- Dead end
             [] -> Nothing
             -- Only one direction to move in, keep going until a branch point
             [newLoc] ->
               moveToNextBranch s{ loc = newLoc, steps = steps + 1 } loc
             -- Many options now, so we are at a branch point
             _ -> Just s
  -- Current location is a either a wall or a door without the key
  isWall :: MazeState -> Bool
  isWall MazeState{..} =
    let v = grid ! loc
    in v == '#' || isUpper v && Set.notMember (toLower v) keys

dirs :: [(Int, Int)]
dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
