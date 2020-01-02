module Main where

import Data.Char
import Data.List.Split
import Data.Tuple.Extra
import Data.Graph
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- getContents
  let reactions = map parseReaction (lines input)
  putStrLn $ show $ minOre reactions

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

minOre :: [Reaction] -> Int
minOre reactions =
  let edgeMap = reactionMap reactions
      chems = topSortChemicals edgeMap
      amounts = computeAmounts edgeMap Map.empty chems
  in case Map.lookup "ORE" amounts of
      Nothing -> error "No amount computed for ORE"
      Just a -> a

-- Map each chemical to all chemicals on the right hand side of a reaction,
-- with the required proportions. We must also include chemicals that do not
-- appear on the left hand side of a reaction (i.e. mapping to `[]`).
reactionMap :: [Reaction] -> Map.Map String [(String, Int, Int)]
reactionMap reactions =
  let flattenReaction (ins, (cRhs, nRhs)) =
        (cRhs, []) : map (\(cLhs, nLhs) -> (cLhs, [(cRhs, nLhs, nRhs)])) ins
      edgeList = concatMap flattenReaction reactions
  in Map.fromListWith (++) edgeList

topSortChemicals :: Map.Map String [(String, Int, Int)] -> [String]
topSortChemicals edgeMap =
  let adjList = map (\(v, vs) -> (v, v, map fst3 vs)) (Map.toList edgeMap)
      (graph, nodeFromVertex, _vertexFromKey) = graphFromEdges adjList
  in map (fst3 . nodeFromVertex) $ topSort $ transposeG graph

-- Compute the minimum amounts of each chemical needed to produce 1 FUEL.
-- Pass in the map of reaction information and the map of amounts computed so far.
computeAmounts :: Map.Map String [(String, Int, Int)] -> Map.Map String Int -> [String] -> Map.Map String Int
computeAmounts _reactions amounts [] = amounts
computeAmounts reactions amounts (chem : chems) =
  computeAmounts reactions (Map.insert chem chemAmount amounts) chems
 where
  chemAmount = case Map.lookup chem reactions of
    Nothing -> error "Reaction map does not include all chemicals"
    Just [] -> if chem == "FUEL" then 1 else 0
      -- ^ we need exactly 1 FUEL but otherwise the chemical is not needed
    Just rhses ->
      sum $ flip map rhses $ \(cRhs, nLhs, nRhs) ->
        case Map.lookup cRhs amounts of
          Nothing -> error "Should have already computed amount of right hand chemical"
          Just a -> nLhs * divCeiling a nRhs

divCeiling :: Integral a => a -> a -> a
divCeiling n d =
  let (q, r) = n `divMod` d
  in if r == 0 then q else q + 1
