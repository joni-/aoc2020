module Day11 where

import qualified Data.HashMap.Strict as Map
import Data.Maybe (catMaybes)

type Coord = (Int, Int)

data State = Empty | Occupied | Floor deriving (Show, Eq)

toState :: Char -> State
toState 'L' = Empty
toState '#' = Occupied
toState '.' = Floor
toState c = error ("Invalid char " ++ show c)

parseLayout :: String -> Map.HashMap Coord State
parseLayout input = Map.fromList vs
  where
    parseLine row s = map (\(char, col) -> ((row, col), toState char)) $ zip s [0 ..]
    vs = concat $ map (\(line, row) -> parseLine row line) $ zip (lines input) [0 ..]

goUp :: Coord -> Coord
goUp (x, y) = (x, y - 1)

goTopRight :: Coord -> Coord
goTopRight (x, y) = (x + 1, y - 1)

goRight :: Coord -> Coord
goRight (x, y) = (x + 1, y)

goBottomRight :: Coord -> Coord
goBottomRight (x, y) = (x + 1, y + 1)

goBottom :: Coord -> Coord
goBottom (x, y) = (x, y + 1)

goBottomLeft :: Coord -> Coord
goBottomLeft (x, y) = (x - 1, y + 1)

goLeft :: Coord -> Coord
goLeft (x, y) = (x - 1, y)

goTopLeft :: Coord -> Coord
goTopLeft (x, y) = (x - 1, y - 1)

adjacentSeats :: Map.HashMap Coord State -> Coord -> [State]
adjacentSeats layout coord = catMaybes [up, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft]
  where
    up = Map.lookup (goUp coord) layout
    topRight = Map.lookup (goTopRight coord) layout
    right = Map.lookup (goRight coord) layout
    bottomRight = Map.lookup (goBottomRight coord) layout
    bottom = Map.lookup (goBottom coord) layout
    bottomLeft = Map.lookup (goBottomLeft coord) layout
    left = Map.lookup (goLeft coord) layout
    topLeft = Map.lookup (goTopLeft coord) layout

firstSeat :: Map.HashMap Coord State -> (Coord -> Coord) -> Coord -> Maybe State
firstSeat layout updateCoord coordinate = case Map.lookup (updateCoord coordinate) layout of
  Just Floor -> firstSeat layout updateCoord (updateCoord coordinate)
  value -> value

visibleSeats :: Map.HashMap Coord State -> Coord -> [State]
visibleSeats layout coord = catMaybes [up, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft]
  where
    up = firstSeat layout goUp coord
    topRight = firstSeat layout goTopRight coord
    right = firstSeat layout goRight coord
    bottomRight = firstSeat layout goBottomRight coord
    bottom = firstSeat layout goBottom coord
    bottomLeft = firstSeat layout goBottomLeft coord
    left = firstSeat layout goLeft coord
    topLeft = firstSeat layout goTopLeft coord

newState :: Int -> State -> [State] -> State
newState _ Empty adjacent = if Occupied `notElem` adjacent then Occupied else Empty
newState tolerance Occupied adjacent = if length (filter (== Occupied) adjacent) >= tolerance then Empty else Occupied
newState _ state _ = state

updateLayout :: Int -> Map.HashMap Coord State -> (Map.HashMap Coord State -> Coord -> [State]) -> Map.HashMap Coord State
updateLayout tolerance layout updateFn = f
  where
    seats = Map.keys layout
    f = foldl (\layout' seat -> Map.insert seat (newState tolerance (Map.lookupDefault Floor seat layout) (updateFn layout seat)) layout') layout seats

iter :: Int -> Map.HashMap Coord State -> (Map.HashMap Coord State -> Coord -> [State]) -> Map.HashMap Coord State
iter tolerance layout updateFn = if layout == nextLayout then layout else iter tolerance nextLayout updateFn
  where
    nextLayout = updateLayout tolerance layout updateFn

solveA :: String -> String
solveA s = show $ length $ filter (== Occupied) $ map snd $ Map.toList finalState
  where
    finalState = iter 4 (parseLayout s) adjacentSeats

solveB :: String -> String
solveB s = show $ length $ filter (== Occupied) $ map snd $ Map.toList finalState
  where
    finalState = iter 5 (parseLayout s) visibleSeats
