module Day3 where

type Position = (Int, Int)

type Grid = [String]

getPositions :: Position -> Grid -> [Position]
getPositions (deltaX, deltaY) grid = positions
  where
    positions = [(i * deltaX, i * deltaY) | i <- [1 .. (length grid)]]

hasTree :: Grid -> Position -> Bool
hasTree grid (x, y) = y < length grid && hasTree'
  where
    row = grid !! y
    x' = x `mod` length row
    hasTree' = row !! x' == '#'

countTrees :: Position -> Grid -> Int
countTrees slope grid = length $ filter (== True) $ map (hasTree grid) positions
  where
    positions = getPositions slope grid

solveA :: String -> String
solveA s = show $ countTrees (3, 1) $ lines s

solveB :: String -> String
solveB s = show $ product $ fmap (`countTrees` grid) slopes
  where
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    grid = lines s
