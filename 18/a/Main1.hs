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
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

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

search :: Maze -> Maybe Int
search Maze{..} = go Set.empty (Seq.singleton initState)
 where
  initState :: (MazeState, Int)
  initState = (MazeState entrance Set.empty, 0)
  go :: Set.Set MazeState -> Seq.Seq (MazeState, Int) -> Maybe Int
  go _ Seq.Empty = Nothing
  go visited ((s@MazeState{..}, steps) Seq.:<| q)
    | Set.size keys == numKeys
    = Just steps
    | otherwise
    = go (Set.insert s visited) $
        foldl' (Seq.|>) q $ map (, steps+1) $ getSuccs visited s
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
