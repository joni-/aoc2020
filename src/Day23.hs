module Day23 where

import Data.Array
import Data.Char (digitToInt)
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust)

parseInput :: String -> Array Int Int
parseInput input = listArray (0, length input - 1) (map digitToInt input)

take3 :: Array Int Int -> Int -> [Int]
take3 arr i = map (arr !) indices
  where
    indices = map (\v -> v `mod` length arr) [i + 1 .. i + 3]

drop3 :: Array Int Int -> Int -> Array Int Int
drop3 arr i = listArray (0, length arr - 4) $ map snd $ filter (\(i, _) -> i `notElem` indices) $ assocs arr
  where
    indices = map (\v -> v `mod` length arr) [i + 1 .. i + 3]

findDestination :: Int -> Int -> Array Int Int -> Int -> Int
findDestination lo hi state target
  | target < lo = findDestination lo hi state hi
  | target `elem` state = fromJust $ elemIndex target $ elems state
  | otherwise = findDestination lo hi state (target - 1)

rotateUntil :: Array Int Int -> (Int, Int) -> Array Int Int
rotateUntil arr (i, e) = if arr ! i == e then arr else rotateUntil arr' (i, e)
  where
    arr' = ixmap (0, length arr - 1) (\i -> (i + 1) `mod` length arr) arr

play :: Int -> Array Int Int -> Array Int Int
play n state = state'
  where
    n' = n `mod` length state
    currentCup = state ! n'
    threeCups = take3 state n
    newCircle = drop3 state n
    lo = minimum $ elems newCircle
    hi = maximum $ elems newCircle
    destination = findDestination lo hi newCircle (currentCup - 1)
    (xs, ys) = splitAt (destination + 1) $ elems newCircle
    updated = xs ++ threeCups ++ ys
    state' = rotateUntil (listArray (0, length state - 1) updated) (n', currentCup)

iter :: Int -> Int -> Array Int Int -> Array Int Int
iter target acc state =
  if acc == target
    then state
    else iter target (acc + 1) (play acc state)

solveA :: String -> String
solveA s = intercalate "" $ map show ff
  where
    initialState = parseInput s
    finalState = elems $ iter 100 0 initialState
    i = fromJust $ elemIndex 1 finalState
    (xs, ys) = splitAt i finalState
    ff = drop 1 ys ++ xs

solveB :: String -> String
solveB s = s
