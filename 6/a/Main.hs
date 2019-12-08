{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Graph
import Data.Foldable
import qualified Data.Map.Strict as Map
import Control.Monad

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
      splitEdge e =
        let (v1, v2) = break (== ')') e
        in (v1, tail v2)
      edges = map splitEdge ls
  putStrLn $ show $ numOrbits edges

numOrbits :: [(String, String)] -> Int
numOrbits edges =
  let -- Group edges by source vertex for constructing the graph
      edgeMap = Map.fromListWith (++) $
                  map (\(v1, v2) -> (v1, [v2])) edges ++
                  zip (map snd edges) (repeat []) -- include missing leaf nodes
      adjList = map (\(v, vs) -> (v, v, vs)) $ Map.toList edgeMap
      (_graph, nodeFromVertex, vertexFromKey) = graphFromEdges adjList
      (orbits, numNodes) = go nodeFromVertex vertexFromKey "COM"
  in orbits
 where
  -- Number of orbits and number of nodes in the subtree rooted at the given vertex
  go :: (Vertex -> (String, String, [String])) -> (String -> Maybe Vertex) -> String -> (Int, Int)
  go vMap keyMap start =
    case keyMap start of
      Nothing -> (0, 0)
      Just v ->
        let (_, _, children) = vMap v
        in foldl'
            (\(os, vs) (cos, cvs) -> (os + cos + cvs, vs + cvs))
            (0, 1)
            (map (go vMap keyMap) children)
