module Day7 where

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Util (splitOn, trim)

type Bag = String

-- input =
--   "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
--   \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
--   \bright white bags contain 1 shiny gold bag.\n\
--   \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
--   \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
--   \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
--   \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
--   \faded blue bags contain no other bags.\n\
--   \dotted black bags contain no other bags."

parseContent :: String -> [Bag]
parseContent s =
  map
    (parseSingleBag . words)
    (filter (/= "no other bags.") $ map trim $ splitOn "," $ trim $ last $ splitOn "contain" s)
  where
    parseSingleBag :: [String] -> Bag
    parseSingleBag (_ : adjective : color : _) = adjective ++ " " ++ color

parseRow :: String -> (Bag, S.Set Bag)
parseRow s = (bag, contents)
  where
    bag = unwords $ take 2 $ words s
    contents = S.fromList $ parseContent s

isValidOutermostBag :: Bag -> M.HashMap Bag (S.Set Bag) -> Bag -> Bool
isValidOutermostBag target bags bag = target /= bag && isNeighbor
  where
    neighbors = M.lookupDefault S.empty bag bags
    -- is target is direct neighbor of bag?
    isNeighbor = target `elem` neighbors || any (isValidOutermostBag target bags) neighbors

solveA :: String -> String
solveA s = show $ length $ filter (isValidOutermostBag "shiny gold" bags) $ M.keys bags
  where
    bags = M.fromList $ map parseRow $ lines s

solveB :: String -> String
solveB s = s
