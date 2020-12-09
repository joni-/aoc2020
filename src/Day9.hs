module Day9 where

import Data.List (find)
import qualified Data.Set as S
import Util (showJust)

isSum :: S.Set Int -> Int -> Bool
isSum previous target = any (\v -> target - v `elem` S.delete v previous) previous

findEncodingError :: Int -> [Int] -> Int
findEncodingError preamble numbers = fst $ head $ filter (not . snd) results
  where
    results = map f [preamble .. (length numbers - 1)]
    f index = (numbers !! index, isSum (S.fromList $ take preamble $ drop (index - preamble) numbers) (numbers !! index))

chunkify :: Int -> [Int] -> [[Int]]
chunkify chunkSize numbers = map f [0 .. length numbers - chunkSize]
  where
    f startIndex = take chunkSize $ drop startIndex numbers

multiChunkify :: Int -> Int -> [Int] -> [[Int]]
multiChunkify minChunkSize maxChunkSize numbers = foldl f [] [minChunkSize .. maxChunkSize]
  where
    f acc chunkSize = acc ++ chunkify chunkSize numbers

toNumbers :: String -> [Int]
toNumbers s = map (\v -> read v :: Int) $ lines s

solveA' :: Int -> String -> String
solveA' chunkSize s = show $ findEncodingError chunkSize $ toNumbers s

solveA :: String -> String
solveA = solveA' 25

solveB' :: Int -> String -> String
solveB' chunkSize s = showJust result
  where
    numbers = toNumbers s
    invalidNumber = findEncodingError chunkSize numbers
    chunks = multiChunkify 2 (length numbers) numbers
    match = find (\chunk -> sum chunk == invalidNumber) chunks
    result = case match of
      Just values -> Just (minimum values + maximum values)
      _ -> Nothing

solveB :: String -> String
solveB = solveB' 25
