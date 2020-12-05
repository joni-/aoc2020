module Day5 where

import Data.Foldable (maximumBy)
import Data.List (sort)
import Data.Ord (comparing)

search :: Char -> [Int] -> String -> Int
search headChar haystack s = head $ foldr f haystack (reverse s)
  where
    f :: Char -> [Int] -> [Int]
    f c acc =
      if c == headChar
        then take (length acc `div` 2) acc
        else drop (length acc `div` 2) acc

parseRowNumber :: String -> Int
parseRowNumber = search 'F' [0 .. 127]

parseColumnNumber :: String -> Int
parseColumnNumber = search 'L' [0 .. 7]

getBoardingPassId :: String -> Int
getBoardingPassId s = (\(r, c) -> parseRowNumber r * 8 + parseColumnNumber c) $ splitAt 7 s

findMySeatId :: [Int] -> Int
findMySeatId ids = (+ 1) $ fst $ head $ filter (\(a, b) -> b - a /= 1) $ zip sortedIds (tail sortedIds)
  where
    sortedIds = sort ids

solveA :: String -> String
solveA s = show $ snd $ maximumBy (comparing snd) $ map (\v -> (v, getBoardingPassId v)) $ lines s

solveB :: String -> String
solveB s = show $ findMySeatId $ map getBoardingPassId $ lines s
