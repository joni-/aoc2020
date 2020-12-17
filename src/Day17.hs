module Day17 where

import Control.Monad (replicateM)
import qualified Data.HashMap.Strict as M
import Data.Hashable

data PDCoordinate = Coordinate3D (Int, Int, Int) | Coordinate4D (Int, Int, Int, Int) deriving (Show, Eq)

data CubeState = Active | Inactive deriving (Show, Eq)

type PocketDimension = M.HashMap PDCoordinate CubeState

instance Hashable PDCoordinate where
  hashWithSalt salt (Coordinate3D t) = hashWithSalt salt t
  hashWithSalt salt (Coordinate4D t) = hashWithSalt salt t

parseInitialState :: String -> PocketDimension
parseInitialState input = M.fromList $ concatMap (\(line, row) -> map (\(state, col) -> (Coordinate3D (row, col, 0), if state == '#' then Active else Inactive)) $ zip line [0 ..]) $ zip (lines input) [0 ..]

parseInitialState4D :: String -> PocketDimension
parseInitialState4D input = M.fromList $ concatMap (\(line, row) -> map (\(state, col) -> (Coordinate4D (row, col, 0, 0), if state == '#' then Active else Inactive)) $ zip line [0 ..]) $ zip (lines input) [0 ..]

neighborCoordinates :: PDCoordinate -> [PDCoordinate]
neighborCoordinates c = case c of
  Coordinate3D (x, y, z) -> map (Coordinate3D . (\[x', y', z'] -> (x + x', y + y', z + z'))) (relativeIndices 3)
  Coordinate4D (x, y, z, v) -> map (Coordinate4D . (\[x', y', z', v'] -> (x + x', y + y', z + z', v + v'))) (relativeIndices 4)
  where
    relativeIndices n = filter (not . all (== 0)) $ replicateM n [-1, 0, 1]

getNewState :: PocketDimension -> PDCoordinate -> CubeState
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
solveB s = show $ length $ M.filter (== Active) $ cycleTimes 6 0 initialState
  where
    initialState = parseInitialState4D s
