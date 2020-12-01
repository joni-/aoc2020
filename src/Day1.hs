module Day1 where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

showJust :: Show a => Maybe a -> String
showJust = show . fromJust

-- Find the two elements that sum to given target
findSumElements :: Int -> [Int] -> Maybe (Int, Int)
findSumElements target elements = (\e -> (e, target - e)) <$> find (\e -> Set.member (target - e) elemSet) elements
  where
    elemSet = Set.fromList elements

-- Find the triplets that sum to given target
findTripleSumElements :: Int -> [Int] -> Maybe (Int, Int, Int)
findTripleSumElements target elements = fmap (\(a, b) -> (a, b, target - a - b)) $ fromMaybe Nothing $ find isJust newTargets
  where
    -- For each element e in the elements list, find a pair that sums to (target - e) (if any).
    -- We can then use that pair (a, b) to deduce the third element (i.e. target - a - b)
    newTargets = map ((`findSumElements` elements) . (target -)) elements

solveA :: String -> String
solveA s = showJust result
  where
    result = uncurry (*) <$> findSumElements 2020 (map (\v -> read v :: Int) $ words s)

solveB :: String -> String
solveB s = showJust result
  where
    result = (\(a, b, c) -> a * b * c) <$> findTripleSumElements 2020 (map (\v -> read v :: Int) $ words s)
