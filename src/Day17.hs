module Day17 where

import Control.Monad (replicateM)
import qualified Data.HashMap.Strict as M

type Coordinate3D = (Int, Int, Int)

data CubeState = Active | Inactive deriving (Show, Eq)

type PocketDimension = M.HashMap Coordinate3D CubeState

input =
  ".#.\n\
  \..#\n\
  \###"

parseInitialState :: String -> PocketDimension
parseInitialState input = M.fromList $ concatMap (\(line, row) -> map (\(state, col) -> ((row, col, 0), if state == '#' then Active else Inactive)) $ zip line [0 ..]) $ zip (lines input) [0 ..]

neighborCoordinates :: Coordinate3D -> [Coordinate3D]
neighborCoordinates (x, y, z) = map (\[x', y', z'] -> (x + x', y + y', z + z')) relativeIndices
  where
    relativeIndices = filter (/= [0, 0, 0]) $ replicateM 3 [-1, 0, 1]

getNewState :: PocketDimension -> Coordinate3D -> CubeState
getNewState state coordinate = case currentState of
  Active -> if activeNeighbors == 2 || activeNeighbors == 3 then Active else Inactive
  Inactive -> if activeNeighbors == 3 then Active else Inactive
  where
    currentState = M.lookupDefault Inactive coordinate state
    neighbors = neighborCoordinates coordinate
    activeNeighbors = length $ filter (\c -> M.lookupDefault Inactive c state == Active) neighbors

nextCycle :: PocketDimension -> PocketDimension
nextCycle state = M.fromList $ map (\c -> (c, getNewState state c)) allCoordinates
  where
    coordinates = M.keys state
    allCoordinates = concatMap neighborCoordinates coordinates

cycleTimes :: Int -> Int -> PocketDimension -> PocketDimension
cycleTimes times acc state = if times == acc then state else cycleTimes times (acc + 1) (nextCycle state)

solveA :: String -> String
solveA s = show $ length $ M.filter (== Active) $ cycleTimes 6 0 initialState
  where
    initialState = parseInitialState s

solveB :: String -> String
solveB s = s
