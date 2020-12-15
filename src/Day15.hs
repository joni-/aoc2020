module Day15 where

import qualified Data.HashMap.Strict as M
import Debug.Trace (trace)
import Util (splitOn)

type SpokenNumbers = M.HashMap Int Int

insert :: Int -> Int -> Int -> SpokenNumbers -> SpokenNumbers
insert t k v s = trace ("Turn " ++ show t ++ ": Insert (" ++ show k ++ ", " ++ show v ++ ")") f
  where
    f = M.insert k v s

play :: Int -> SpokenNumbers -> Int -> Int -> Int
play target spokenNumbers number turn =
  if turn == target then nextNumber else f
  where
    previousTurn = M.lookup number spokenNumbers
    nextNumber = case previousTurn of
      Just turn' -> turn - 1 - turn'
      Nothing -> 0
    f = play target (M.insert number (turn - 1) spokenNumbers) nextNumber (turn + 1)

solveA :: String -> String
solveA s = show $ play 2020 initialSpokenNumbers (last startingNumbers) (length startingNumbers + 1)
  where
    startingNumbers = map (\v -> read v :: Int) $ splitOn "," $ head $ lines s
    initialSpokenNumbers = M.fromList $ zip startingNumbers [1 .. (length startingNumbers - 1)]

solveB :: String -> String
solveB s = s
