module Day21 where

import qualified Data.HashMap.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Parser (between, until)
import Util (replace, splitOn, trim)

-- input :: String
-- input =
--   "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
--   \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
--   \sqjhc fvjkl (contains soy)\n\
--   \sqjhc mxmxvkd sbzzf (contains fish)"

type Allergen = String

type Ingredient = String

parseLine :: String -> Maybe ([Ingredient], [Allergen])
parseLine input = do
  (ingredients, rest) <- Parser.until '(' input
  (allergens, rest) <- Parser.until ')' rest
  let ingredients' = filter (not . null) $ splitOn " " ingredients
  let allergens' = map trim $ splitOn "," $ replace "contains" "" allergens
  return (ingredients', allergens')

parseInput :: String -> [([Ingredient], [Allergen])]
parseInput input = mapMaybe parseLine $ lines input

findPossibilities :: [([Ingredient], [Allergen])] -> M.HashMap Allergen (Set.Set Ingredient)
findPossibilities = foldl f M.empty
  where
    f acc (ingredients, allergens) = foldl (\acc' allergent -> M.insertWith Set.intersection allergent (Set.fromList ingredients) acc') acc allergens

solveA :: String -> String
solveA s = show $ sum ff
  where
    foods = parseInput s
    ingredients = concatMap fst foods
    possibilities = findPossibilities foods
    nonAllergicIngredients = Set.difference (Set.fromList ingredients) (foldl Set.union Set.empty $ map snd $ M.toList possibilities)
    count x l = (length . filter (== x)) l
    ff = map (`count` ingredients) (Set.toList nonAllergicIngredients)

solveB :: String -> String
solveB s = s
