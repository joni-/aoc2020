module Day7 where

import qualified Data.HashMap.Strict as M
import Data.Hashable
import Util (splitOn, trim)

data Bag = Bag {count :: Int, color :: String} deriving (Show)

instance Eq Bag where
  (==) b1 b2 = color b1 == color b2

instance Ord Bag where
  compare b1 b2 = compare (color b1) (color b2)

instance Hashable Bag where
  hashWithSalt salt bag = hashWithSalt salt $ color bag

-- todo: unsafe indexing
createBag :: [String] -> Bag
createBag ["no", "other", "bags."] = Bag {count = 0, color = "empty"}
createBag (countS : adjective : color : _) = Bag {count = read countS, color = adjective ++ " " ++ color}
createBag [adjective, color] = Bag {count = 0, color = adjective ++ " " ++ color}

parseContent :: String -> [Bag]
parseContent s =
  map
    ((createBag . words) . trim)
    (splitOn "," $ trim $ last $ splitOn "contain" s)

parseRow :: String -> (Bag, [Bag])
parseRow s = (bag, contents)
  where
    bag = createBag $ take 2 $ words s -- unsafe
    contents = parseContent s

isValidOutermostBag :: Bag -> M.HashMap Bag [Bag] -> Bag -> Bool
isValidOutermostBag target bags bag = target /= bag && isNeighbor
  where
    neighbors = M.lookupDefault [] bag bags
    -- is target is direct neighbor of bag?
    isNeighbor = target `elem` neighbors || any (isValidOutermostBag target bags) neighbors

findBagCount :: M.HashMap Bag [Bag] -> Bag -> Int -> Int
-- if neighbors is empty, then return accumulator
findBagCount bags currentBag accumulator = if null contents then 1 else f
  where
    contents = M.lookupDefault [] currentBag bags
    sumOfContents = sum $ map (\bag -> count bag * findBagCount bags bag accumulator) contents
    -- count bag count for each of the bags inside this bag and then return their sum + accumulator
    f = accumulator + sumOfContents

solveA :: String -> String
solveA s = show $ length $ filter (isValidOutermostBag Bag {count = 0, color = "shiny gold"} bags) $ M.keys bags
  where
    bags = M.fromList $ map parseRow $ lines s

solveB :: String -> String
solveB s = show $ (\v -> v - 1) $ findBagCount bags shinyGold 1
  where
    bags = M.fromList $ map parseRow $ lines s
    shinyGold = Bag {count = 0, color = "shiny gold"}
