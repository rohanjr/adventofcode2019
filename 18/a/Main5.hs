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
import Data.Bits
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap

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
  , keys :: Int
  }
  deriving (Eq, Ord, Show)

data MazeStateWithSteps = MazeStateWithSteps
  { state :: MazeState
  , steps :: Int
  }
  deriving (Eq, Show)

intToKeys :: Int -> Int -> [Char]
intToKeys numKeys i = go 0
 where
  go :: Int -> [Char]
  go b
    | b == numKeys
    = []
    | testBit i b
    = bitPosToChar b : go (b + 1)
    | otherwise
    = go (b + 1)
  bitPosToChar :: Int -> Char
  bitPosToChar b = chr $ ord 'a' + b

charToBitPos :: Char -> Int
charToBitPos c = ord c - ord 'a'

charSetToInt :: Set.Set Char -> Int
charSetToInt s =
  let bs = map charToBitPos (Set.toAscList s)
  in foldl' setBit 0 bs

type KeySetKeyMap = IntMap.IntMap (Map.Map Char Int)

-- Find distances until we have collected all keys.
-- This builds a map from states (current key location and keys collected so far)
-- to minimal distances to a state with all keys.
distancesToEnd :: Maze -> Maybe Int
distancesToEnd m@Maze{..} =
  let allDists = foldl' go IntMap.empty subsets
  in (allDists IntMap.!? 0) >>= (Map.!? '@')
 where
  subsets :: [[Int]]
  subsets =
    groupBy (\i j -> popCount i == popCount j) $
      sortBy (comparing $ Down . popCount) [0 .. allKeys]
  allKeys :: Int
  allKeys = bit numKeys - 1
  allPaths :: Map.Map Char (Map.Map Char MazeStateWithSteps)
  allPaths = allKeyPaths m
  distFromKeyAndKeySet :: KeySetKeyMap -> Char -> Int -> Maybe Int
  distFromKeyAndKeySet ds k s =
    let missingKeys = xor allKeys s
    in if missingKeys == 0 then Just 0 else
        let missingKeyList = intToKeys numKeys missingKeys
            pathsFromK = allPaths Map.! k
        in (\case [] -> Nothing; l -> Just $ minimum l) $
            flip mapMaybe missingKeyList $ \kk -> case pathsFromK Map.!? kk of
              Just (MazeStateWithSteps (MazeState _ keysRequired) distToNewKey)
                | keysRequired .&. s == keysRequired ->
                    let newKeySet = setBit s (charToBitPos kk)
                        distFromNewKey = (ds IntMap.!? newKeySet) >>= (Map.!? kk)
                    in fmap (distToNewKey +) distFromNewKey
              _ -> Nothing
  distsFromKeySet :: KeySetKeyMap -> Int -> KeySetKeyMap
  distsFromKeySet ds s =
    let ks = if s == 0 then ['@'] else intToKeys numKeys s
        insertInKeySetMap dds k =
          case distFromKeyAndKeySet ds k s of
            Nothing -> dds
            Just d -> IntMap.insertWith Map.union s (Map.singleton k d) dds
    in foldl' insertInKeySetMap IntMap.empty ks
  go :: KeySetKeyMap -> [Int] -> KeySetKeyMap
  go distances ss = IntMap.unions $ map (distsFromKeySet distances) ss

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
    in MazeStateWithSteps (MazeState keyLoc 0) 0
  go :: Set.Set (Int, Int) -> Map.Map Char MazeStateWithSteps -> Seq.Seq MazeStateWithSteps -> Map.Map Char MazeStateWithSteps
  go _ paths Seq.Empty = paths
  go visited paths (ss@(MazeStateWithSteps MazeState{..} steps) Seq.:<| q)
    | Map.size paths == numKeys
    = paths
    | otherwise
    = let v = grid ! loc
          newPaths = if isLower v then Map.insert v ss paths else paths
          newKeys = if isUpper v then setBit keys (charToBitPos $ toLower v) else keys
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
