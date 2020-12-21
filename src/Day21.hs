module Day21 where

import qualified Data.HashMap.Strict as M
import Data.List (intercalate, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Parser (between, until)
import Util (replace, splitOn, trim)

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

matchAllergenToIngredient :: M.HashMap Allergen (Set.Set Ingredient) -> [(Allergen, Ingredient)] -> [(Allergen, Ingredient)]
matchAllergenToIngredient possibilities acc = if null possibilities then acc else f
  where
    found = M.foldrWithKey (\k v acc' -> if Set.size v == 1 then acc' ++ [(k, Set.elemAt 0 v)] else acc') acc possibilities
    mm = foldr (\(allergen, _) acc' -> M.delete allergen acc') possibilities found
    foundIngredients = Set.fromList $ map snd found
    nn = M.map (`Set.difference` foundIngredients) mm
    f = matchAllergenToIngredient nn found

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
solveB s = intercalate "," $ map snd $ sortBy (comparing fst) matches
  where
    foods = parseInput s
    ingredients = concatMap fst foods
    possibilities = findPossibilities foods
    matches = matchAllergenToIngredient possibilities []
