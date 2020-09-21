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
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
--import Debug.Trace

main :: IO ()
main = do
  input <- getContents
  let maze = makeMaze input
  putStr $ show $ distancesToEnd maze
  return ()

data Maze = Maze
  { grid :: Array (Int, Int) Char
  , numKeys :: Int
  , keyLocs :: Map.Map Char (Int, Int)
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
        keyLocs = Map.fromList [(k, loc) | (loc, k) <- assocs grid, isLower k]
        numKeys = Map.size keyLocs
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

-- Find distances until we have collected all keys.
-- This builds a map from states (current key location and keys collected so far)
-- to minimal distances to a state with all keys.
distancesToEnd :: Maze -> Maybe Int
distancesToEnd m@Maze{..} =
  let allDists = foldl' go Map.empty subsets
  in Map.lookup ('@', Set.empty) allDists
 where
  subsets :: [[Set.Set Char]]
  subsets = reverse $
    groupBy (\s t -> Set.size s == Set.size t) $
      sortBy (comparing Set.size) $ Set.toList $
        Set.powerSet $ Map.keysSet keyLocs
  keys :: Set.Set Char
  keys = Map.keysSet keyLocs
  allPaths :: Map.Map Char (Map.Map Char MazeStateWithSteps)
  allPaths = allKeyPaths m
  distsFromKeySet :: Map.Map (Char, Set.Set Char) Int -> Set.Set Char -> Map.Map (Char, Set.Set Char) Int
  distsFromKeySet ds s =
    let ks = if Set.null s then Set.singleton '@' else s
    in Set.foldl' (\dds k -> case distFromKeyAndKeySet ds k s of Nothing -> dds; Just d -> Map.insert (k, s) d dds) Map.empty ks
  distFromKeyAndKeySet :: Map.Map (Char, Set.Set Char) Int -> Char -> Set.Set Char -> Maybe Int
  distFromKeyAndKeySet ds k s =
    let missingKeys = Set.toList $ keys Set.\\ s
    in if null missingKeys then Just 0 else
        let pathsFromK = {-trace ("Getting all paths from " <> show k) $-} allPaths Map.! k
        in (\case [] -> Nothing; l -> Just $ minimum l) $
            flip mapMaybe missingKeys $ \kk -> case pathsFromK Map.!? kk of
              Just (MazeStateWithSteps (MazeState _ keysRequired) distToNewKey)
                    --trace ("Getting path from " <> show k <> " to " <> show kk) $
                | keysRequired `Set.isSubsetOf` s ->
                    let distFromNewKey = ds Map.!? (kk, Set.insert kk s) --trace ("Getting distance from " <> show kk <> " to the end")
                    in fmap (distToNewKey +) distFromNewKey
              _ -> Nothing --trace ("Required " <> show keysRequired <> "\nHave " <> show s)
  go :: Map.Map (Char, Set.Set Char) Int -> [Set.Set Char] -> Map.Map (Char, Set.Set Char) Int
  go distances ss = foldl' Map.union Map.empty $ map (distsFromKeySet distances) ss
  -- go 0 [s] _ = Map.fromList [((k, s), 0) | k <- Map.keys keyLocs]
    -- | keysMissing == numKeys
    -- = let distFromEntrance = minimum
    --         [d + fromJust (Map.lookup (k, Set.singleton k) distances)
    --         | (k, MazeStateWithSteps (MazeState _ ks) d) <- Map.toList (keyPaths m '@')
    --         , Set.null ks]
    --   in Map.insert ('@', Set.empty) distFromEntrance distances
    -- | otherwise

-- powerset :: [a] -> [Set.Set a]
-- powerset [] = [Set.empty]
-- powerset (x : xs) =
--   let subsets = powerset xs
--   in subsets ++ map (Set.insert x) subsets

-- Get all subsets (as a list) of a set with size k, not including the given element.
-- subsets :: Int -> Set.Set a -> a -> [a]
-- subsets k s x =


allKeyPaths :: Maze -> Map.Map Char (Map.Map Char MazeStateWithSteps)
allKeyPaths m = Map.fromList [(k, keyPaths m k) | k <- '@' : Map.keys (keyLocs m)]

-- Find the paths from key `k` to all other keys.
-- This path can go through doors but not walls.
-- We record for each path the length and keys required.
-- Overloaded to find paths from the entrance '@' as well.
keyPaths :: Maze -> Char -> Map.Map Char MazeStateWithSteps
keyPaths Maze{..} k = go Set.empty Map.empty (Seq.singleton initState)
 where
  -- To get from key `k` to itself takes no keys and 0 steps
  initState :: MazeStateWithSteps
  initState =
    let keyLoc = if k == '@' then entrance else fromJust $ Map.lookup k keyLocs
    in MazeStateWithSteps (MazeState keyLoc Set.empty) 0
  go :: Set.Set (Int, Int) -> Map.Map Char MazeStateWithSteps -> Seq.Seq MazeStateWithSteps -> Map.Map Char MazeStateWithSteps
  go _ paths Seq.Empty = paths
  go visited paths (ss@(MazeStateWithSteps MazeState{..} steps) Seq.:<| q)
    | Map.size paths == numKeys
    = paths
    | otherwise
    = let v = grid ! loc
          newPaths = if isLower v then Map.insert v ss paths else paths
          newKeys = if isUpper v then Set.insert (toLower v) keys else keys
          nextStates =
            [MazeStateWithSteps (MazeState newLoc newKeys) (steps+1)
            | newLoc <- getSuccs visited loc]
          newQ = foldl' (Seq.|>) q nextStates
      in go (Set.insert loc visited) newPaths newQ
  getSuccs :: Set.Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
  getSuccs visited loc =
    flip mapMaybe dirs $ \dir ->
      let newLoc = addPos loc dir
          isWall = grid ! newLoc == '#'
      in if isWall || newLoc `Set.member` visited then Nothing else Just newLoc

dirs :: [(Int, Int)]
dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
