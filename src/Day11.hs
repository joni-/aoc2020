module Day11 where

import qualified Data.HashMap.Strict as Map
import Data.Maybe (catMaybes)

type Coord = (Int, Int)

data State = Empty | Occupied | Floor deriving (Show, Eq)

input' =
  "L.LL.LL.LL\n\
  \LLLLLLL.LL\n\
  \L.L.L..L..\n\
  \LLLL.LL.LL\n\
  \L.LL.LL.LL\n\
  \L.LLLLL.LL\n\
  \..L.L.....\n\
  \LLLLLLLLLL\n\
  \L.LLLLLL.L\n\
  \L.LLLLL.LL"

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

adjacentSeats :: Map.HashMap Coord State -> Coord -> [State]
adjacentSeats layout (x, y) = catMaybes [up, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft]
  where
    up = Map.lookup (x, y - 1) layout
    topRight = Map.lookup (x + 1, y - 1) layout
    right = Map.lookup (x + 1, y) layout
    bottomRight = Map.lookup (x + 1, y + 1) layout
    bottom = Map.lookup (x, y + 1) layout
    bottomLeft = Map.lookup (x - 1, y + 1) layout
    left = Map.lookup (x - 1, y) layout
    topLeft = Map.lookup (x - 1, y - 1) layout

newState :: State -> [State] -> State
newState Empty adjacent = if Occupied `notElem` adjacent then Occupied else Empty
newState Occupied adjacent = if length (filter (== Occupied) adjacent) >= 4 then Empty else Occupied
newState state _ = state

updateLayout :: Map.HashMap Coord State -> Map.HashMap Coord State
updateLayout layout = f
  where
    seats = Map.keys layout
    f = foldl (\layout' seat -> Map.insert seat (newState (Map.lookupDefault Floor seat layout) (adjacentSeats layout seat)) layout') layout seats

iter :: Map.HashMap Coord State -> Map.HashMap Coord State
iter layout = if layout == nextLayout then layout else iter nextLayout
  where
    nextLayout = updateLayout layout

solveA :: String -> String
solveA s = show $ length $ filter (== Occupied) $ map snd $ Map.toList finalState
  where
    finalState = iter (parseLayout s)

solveB :: String -> String
solveB s = s
