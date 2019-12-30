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

main :: IO ()
main = do
  [arg] <- getArgs
  input <- getContents
  let initialOre = read arg
      reactions = map parseReaction (lines input)
  putStrLn $ show $ maxFuel initialOre reactions

type ChemQuantity = (String, Int)
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

type ReactionMap = Map.Map String (Int, [ChemQuantity])

-- Map each chemical on the right hand side of a reaction to the required
-- chemicals on the left hand side of the reaction, along with the proportions.
-- Only "ORE" does not appear on the right hand side of an equation, so we
-- add a mapping for it to the empty list of chemicals.
-- Also no chemical appears on the right hand side of two different reactions,
-- so we need not worry about duplicate keys in our map.
reactionMapAndChemOrder :: [Reaction] -> (ReactionMap, [String])
reactionMapAndChemOrder reactions =
  let toMapping (lhses, (cRhs, nRhs)) = (cRhs, (nRhs, lhses))
      mappings = map toMapping reactions
      withOre = ("ORE", (0, [])) : mappings
  in (Map.fromList mappings, topSortChems withOre)

-- Order the chemicals in a topological (dependency) order.
-- That is, chem1 should appear before chem2 in the output list
-- if chem1 is the right hand side of a reaction with chem2 on the left hand side.
topSortChems :: [(String, (Int, [ChemQuantity]))] -> [String]
topSortChems mappings =
  let adjList = map (\(cRhs, (_nRhs, lhses)) -> (cRhs, cRhs, map fst lhses)) mappings
      (graph, nodeFromVertex, _vertexFromKey) = graphFromEdges adjList
  in map (fst3 . nodeFromVertex) (topSort graph)

type NeededAndLeftOver = (Map.Map String Int, Map.Map String Int)

computeAmountsNeeded :: ReactionMap -> [String] -> NeededAndLeftOver -> NeededAndLeftOver
computeAmountsNeeded _ [] neededLeftover = neededLeftover
computeAmountsNeeded reactions (cRhs : cs) (amountsNeeded, amountsLeftover) =
  case Map.lookup cRhs reactions of
    Nothing -> (amountsNeeded, amountsLeftover)
    Just (nRhs, lhses) ->
      let cRhsNeededGross = case Map.lookup cRhs amountsNeeded of
              Nothing -> if cRhs == "FUEL" then 1 else 0
              Just aRhs -> aRhs
          cRhsLeftover = fromMaybe 0 $ Map.lookup cRhs amountsLeftover
          (amountsNeededNow, cRhsLeftoverNow) =
            if cRhsLeftover >= cRhsNeededGross
              then (amountsNeeded, cRhsLeftover - cRhsNeededGross)
              else
                let cRhsNeededNet = cRhsNeededGross - cRhsLeftover
                    numReactions = ceiling (fromIntegral cRhsNeededNet / fromIntegral nRhs)
                    addLhsNeeded needed (cLhs, nLhs) =
                      Map.insertWith (+) cLhs (numReactions * nLhs) needed
                in (foldl' addLhsNeeded amountsNeeded lhses, numReactions * nRhs - cRhsNeededNet)
          amountsLeftoverNow = Map.insert cRhs cRhsLeftoverNow amountsLeftover
      in computeAmountsNeeded reactions cs (amountsNeededNow, amountsLeftoverNow)

maxFuel :: Int64 -> [Reaction] -> Int64
maxFuel initialOre reactions =
  let (reactionMap, chems) = reactionMapAndChemOrder reactions
  in exhaustOre initialOre 0 reactionMap chems Map.empty

exhaustOre :: Int64 -> Int64 -> ReactionMap -> [String] -> Map.Map String Int -> Int64
exhaustOre oreLeft fuelCollected reactions chems amountsLeftover =
  let (amountsNeeded, amountsLeftoverNow) =
        computeAmountsNeeded reactions chems (Map.empty, amountsLeftover)
  in case Map.lookup "ORE" amountsNeeded of
      Nothing -> error "Minimum amount of ore not found"
      Just minOre ->
        let diffFromMin = oreLeft - fromIntegral minOre
        in if diffFromMin < 0
            then fuelCollected
            else exhaustOre diffFromMin (fuelCollected + 1) reactions chems amountsLeftoverNow
