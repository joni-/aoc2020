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

-- thanks
-- https://www.reddit.com/r/adventofcode/comments/kc4njx/2020_day_13_solutions/ghf18ej/?utm_source=reddit&utm_medium=web2x&context=3
iter :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
iter time step offset bid =
  if (time + offset) `mod` bid == 0
    then (time, step * bid)
    else iter (time + step) step offset bid

solveB :: String -> String
solveB s = show $ fst $ foldl f (1, 1) busIds
  where
    busIds = map (\(a, b) -> (toInteger a, toInteger b)) $ parseBusIdsB s
    f (t, step) (bid, offset) = iter t step offset bid
