module Day13 where

import Data.List (find, minimumBy)
import Data.Ord (comparing)
import Util (showJust, splitOn)

input' =
  "939\n\
  \7,13,x,x,59,x,31,19"

parseTarget :: String -> Int
parseTarget input = read $ head $ lines input

parseBusIds :: String -> [Int]
parseBusIds input = map read $ filter (/= "x") $ splitOn "," $ last $ lines input

parseBusIdsB :: String -> [(Int, Int)]
parseBusIdsB input = map (\(v, i) -> (read v :: Int, i)) $ filter (\(v, _) -> v /= "x") $ zip (splitOn "," $ last $ lines input) [0 ..]

earliestDeparts :: Int -> [Int] -> [(Int, Int)]
earliestDeparts target busIds = map f busIds
  where
    f id = (id, lowest)
      where
        multiplier = target `div` id
        lowest = if multiplier * id < target then multiplier * id + id else multiplier * id

earliestDepart :: [(Int, Int)] -> (Int, Int)
earliestDepart = minimumBy (comparing snd)

solveA :: String -> String
solveA s = show $ busId * wait
  where
    target = parseTarget s
    busIds = parseBusIds s
    (busId, depart) = earliestDepart $ earliestDeparts target busIds
    wait = depart - target

matches :: [(Integer, Integer)] -> Integer -> Bool
matches busIds tick = all (== 0) (map (\(busId, index) -> (tick + index) `mod` busId) busIds)

findTicks :: (Integer, Integer) -> (Integer, Integer) -> [Integer]
findTicks (firstId, _) (lastId, lastIndex) = [tick - lastIndex | tick <- [1 ..], tick `mod` firstId == 0 && tick `mod` lastId == 0]

-- findTicks (firstId, firstIndex) (lastId, lastIndex) currentTick ticks = if firstMatch && lastMatch then findTicks (firstId, firstIndex) (lastId, lastIndex) (currentTick + 1) (currentTick : ticks) else findTicks (firstId, firstIndex) (lastId, lastIndex) (currentTick + 1) ticks
--   where
--     lastMatch = currentTick `mod` lastId == 0
--     firstMatch = currentTick `mod` firstId == 0

solveB :: String -> String
solveB s = showJust match
  where
    busIds = map (\(a, b) -> (toInteger a, toInteger b)) $ parseBusIdsB s
    match = find (matches busIds) $ findTicks (head busIds) (last busIds)
