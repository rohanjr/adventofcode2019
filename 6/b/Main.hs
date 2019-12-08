{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Tuple
import Data.Graph
import Data.Foldable
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Control.Monad

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
      splitEdge e =
        let (v1, v2) = break (== ')') e
        in (v1, tail v2)
      edges = map splitEdge ls
  putStrLn $
    case distToSanta edges of
      Nothing -> "Santa not found!"
      Just d -> show d

distToSanta :: [(String, String)] -> Maybe Int
distToSanta edges =
  let -- Group edges by source vertex for constructing the graph
      -- Include reverse edges to create undirected graph
      edgeMap = Map.fromListWith (++) $
                  map (\(v1, v2) -> (v1, [v2])) $
                    edges ++ map swap edges
      adjList = map (\(v, vs) -> (v, v, vs)) $ Map.toList edgeMap
      (_graph, nodeFromVertex, vertexFromKey) = graphFromEdges adjList
      dist = go nodeFromVertex vertexFromKey "SAN" Set.empty (Seq.singleton ("YOU", 0))
  in fmap (\d -> d - 2) dist -- don't include distance from YOU/SANTA to own planet
 where
  -- Length of the path to the target node
  go :: (Vertex -> (String, String, [String])) -> (String -> Maybe Vertex) -> String -> Set.Set String -> Seq.Seq (String, Int) -> Maybe Int
  go vMap keyMap target visited (Seq.viewl -> Seq.EmptyL) = Nothing
  go vMap keyMap target visited (Seq.viewl -> (curr, cost) Seq.:< toVisit) =
    case keyMap curr of
      Nothing -> error "Node is not present in graph"
      Just v ->
        let (_, _, children) = vMap v
        in if curr == target
            then Just cost
            else go vMap keyMap target (Set.insert curr visited) $
                  toVisit Seq.>< (Seq.fromList $
                    map (\child -> (child, cost + 1)) $
                      filter (`Set.notMember` visited) children)
