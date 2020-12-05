module Day5 where

import Data.Foldable (maximumBy)
import Data.Ord (comparing)

data Direction = UP | DOWN

parseRow :: String -> [Int]
parseRow s = foldr f [0 .. 127] (reverse s)
  where
    f :: Char -> [Int] -> [Int]
    f c acc =
      if c == 'F'
        then take ((length acc) `div` 2) acc
        else drop ((length acc) `div` 2) acc

parseRowNumber :: String -> Int
parseRowNumber s = case parseRow s of
  [x] -> x
  _ -> -1

parseColumn :: String -> [Int]
parseColumn s = foldr f [0 .. 7] (reverse s)
  where
    f :: Char -> [Int] -> [Int]
    f c acc =
      if c == 'L'
        then take ((length acc) `div` 2) acc
        else drop ((length acc) `div` 2) acc

parseColumnNumber :: String -> Int
parseColumnNumber s = case parseColumn s of
  [x] -> x
  _ -> -1

getBoardingPassId :: String -> Int
getBoardingPassId s = (\(r, c) -> parseRowNumber r * 8 + parseColumnNumber c) $ splitAt 7 s

solveA :: String -> String
solveA s = show $ snd $ maximumBy (comparing snd) $ map (\v -> (v, getBoardingPassId v)) $ lines s

solveB :: String -> String
solveB s = s
