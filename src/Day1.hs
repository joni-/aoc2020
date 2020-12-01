module Day1 where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

findSumElements :: Int -> [Int] -> Maybe (Int, Int)
findSumElements target elements = (\e -> (e, target - e)) <$> find (\e -> Set.member (target - e) elemSet) elements
  where
    elemSet = Set.fromList elements

findTripleSumElements :: Int -> [Int] -> Maybe (Int, Int, Int)
findTripleSumElements target elements = fmap (\(a, b) -> (a, b, target - a - b)) $ fromMaybe Nothing $ find isJust newTargets'
  where
    newTargets' = map (\t -> findSumElements t elements) $ map (target -) elements

solveA :: String -> String
solveA s = show $ fromMaybe (-1) result
  where
    result = uncurry (*) <$> findSumElements 2020 (map (\v -> read v :: Int) $ words s)

solveB :: String -> String
solveB s = show $ fromMaybe (-1) result
  where
    result = (\(a, b, c) -> a * b * c) <$> findTripleSumElements 2020 (map (\v -> read v :: Int) $ words s)
