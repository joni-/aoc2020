module Day13 where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Util (splitOn)

input' =
  "939\n\
  \7,13,x,x,59,x,31,19"

parseTarget :: String -> Int
parseTarget input = read $ head $ lines input

parseBusIds :: String -> [Int]
parseBusIds input = map read $ filter (/= "x") $ splitOn "," $ last $ lines input

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

solveB :: String -> String
solveB s = s
