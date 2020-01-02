module Main where

import System.Environment
import Data.Int
import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import Data.List
import Data.List.Split
import Data.Graph
import qualified Data.Map.Strict as Map
import Numeric.Search

main :: IO ()
main = do
  [arg] <- getArgs
  input <- getContents
  let oreProvided = read arg
      reactions = map parseReaction (lines input)
  putStrLn $ show $ maxFuelWithOre oreProvided reactions

type ChemQuantity = (String, Int64)
type Reaction = ([ChemQuantity], ChemQuantity)

-- Return inputs and outputs from a reaction.
parseReaction :: String -> Reaction
parseReaction s =
  let [lhs, rhs] = splitOn "=>" $ filter (not . isSpace) s
  in (parseChemicals lhs, parseChemical rhs)

parseChemicals :: String -> [ChemQuantity]
parseChemicals s = map parseChemical $ splitOn "," s

parseChemical :: String -> ChemQuantity
parseChemical s =
  let (num, chem) = break isAlpha s
  in (chem, read num)

type ReactionMap = Map.Map String (Int64, [ChemQuantity])

-- Map each chemical on the right hand side of a reaction to the required
-- chemicals on the left hand side of the reaction, along with the proportions.
-- No chemical appears on the right hand side of two different reactions,
-- so we need not worry about duplicate keys in our map.
reactionMapAndChemOrder :: [Reaction] -> (ReactionMap, [String])
reactionMapAndChemOrder reactions =
  let toMapping (lhses, (cRhs, nRhs)) = (cRhs, (nRhs, lhses))
      mappings = map toMapping reactions
  in (Map.fromList mappings, topSortChems mappings)

-- Order the chemicals in a topological (dependency) order.
-- That is, chem1 should appear before chem2 in the output list
-- if chem1 is on the right of a reaction with chem2 on the left.
-- "ORE" does not appear on the right hand side of an equation, so we need to
-- add it as a vertex without outgoing edges in the graph.
topSortChems :: [(String, (Int64, [ChemQuantity]))] -> [String]
topSortChems mappings =
  let toEdge (cRhs, (_nRhs, lhses)) = (cRhs, cRhs, map fst lhses)
      adjList = ("ORE", "ORE", []) : map toEdge mappings
      (graph, nodeFromVertex, _vertexFromKey) = graphFromEdges adjList
  in map (fst3 . nodeFromVertex) (topSort graph)

computeAmountsNeeded :: Int64 -> ReactionMap -> [String] -> Map.Map String Int64 -> Map.Map String Int64
computeAmountsNeeded _ _ [] amountsNeeded = amountsNeeded
computeAmountsNeeded fuelNeeded reactions (cRhs : cs) amountsNeeded =
  case Map.lookup cRhs reactions of
    Nothing ->
      -- This should only happen for "ORE", since it does not appear on the right hand side
      -- of any reaction. There is nothing to compute for it, so move on to the remaining chemicals.
      computeAmountsNeeded fuelNeeded reactions cs amountsNeeded
    Just (nRhs, lhses) ->
      let cRhsNeeded = case Map.lookup cRhs amountsNeeded of
              Nothing -> if cRhs == "FUEL" then fuelNeeded else 0
              Just aRhs -> aRhs
          numReactions = cRhsNeeded `divCeiling` nRhs
          addLhsNeeded needed (cLhs, nLhs) =
                      Map.insertWith (+) cLhs (numReactions * nLhs) needed
          amountsNeededNow = foldl' addLhsNeeded amountsNeeded lhses
      in computeAmountsNeeded fuelNeeded reactions cs amountsNeededNow

divCeiling :: Integral a => a -> a -> a
divCeiling n d =
  let (q, r) = n `divMod` d
  in if r == 0 then q else q + 1

oreNeededForFuel :: ReactionMap -> [String] -> Int64 -> Int64
oreNeededForFuel reactionMap chems fuelNeeded =
  let amountsNeeded = computeAmountsNeeded fuelNeeded reactionMap chems Map.empty
  in case Map.lookup "ORE" amountsNeeded of
      Nothing -> error "Could not find amount of ORE needed."
      Just a -> a

-- Find the largest amount of fuel we can produce with a given amount of ore.
-- We do this by binary search using `oreNeededForFuel` which is monotonically
-- increasing with the amount of fuel.
maxFuelWithOre :: Int64 -> [Reaction] -> Int64
maxFuelWithOre oreProvided reactions =
  let oreNeededFor = uncurry oreNeededForFuel (reactionMapAndChemOrder reactions)
      fuelRange = fromTo (oreProvided `div` oreNeededFor 1) oreProvided
  in fromJust $ largest True $ search fuelRange divForever $
        \f -> oreNeededFor f <= oreProvided
