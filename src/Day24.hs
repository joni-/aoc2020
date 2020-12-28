module Day24 where

import qualified Data.HashMap.Strict as M

type Coordinate = (Int, Int)

data Step = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Show)

data Color = White | Black deriving (Show, Eq)

type FloorState = M.HashMap Coordinate Color

parseSteps :: String -> [Step] -> [Step]
parseSteps "" acc = acc
parseSteps ('e' : rest) acc = parseSteps rest (acc ++ [East])
parseSteps ('s' : 'e' : rest) acc = parseSteps rest (acc ++ [SouthEast])
parseSteps ('s' : 'w' : rest) acc = parseSteps rest (acc ++ [SouthWest])
parseSteps ('w' : rest) acc = parseSteps rest (acc ++ [West])
parseSteps ('n' : 'w' : rest) acc = parseSteps rest (acc ++ [NorthWest])
parseSteps ('n' : 'e' : rest) acc = parseSteps rest (acc ++ [NorthEast])
parseSteps _ _ = undefined

parseCoordinate :: [Step] -> Coordinate -> Coordinate
parseCoordinate [] coordinate = coordinate
parseCoordinate (step : rest) (x, y) = parseCoordinate rest (x', y')
  where
    x' = case step of
      East -> x + 2
      West -> x - 2
      NorthWest -> x - 1
      SouthWest -> x - 1
      NorthEast -> x + 1
      SouthEast -> x + 1
    y' = case step of
      NorthWest -> y - 1
      SouthWest -> y + 1
      NorthEast -> y - 1
      SouthEast -> y + 1
      _ -> y

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) = [east, west, northWest, southWest, northEast, southEast]
  where
    east = (x + 2, y)
    west = (x - 2, y)
    northWest = (x - 1, y - 1)
    southWest = (x - 1, y + 1)
    northEast = (x + 1, y - 1)
    southEast = (x + 1, y + 1)

updateState :: FloorState -> FloorState
updateState state = M.fromList newState
  where
    newState =
      map
        ( \c -> case M.lookupDefault White c state of
            White -> if length (filter (== Black) $ neighborColors c) == 2 then (c, Black) else (c, White)
            Black -> if numOfBlacks == 0 || numOfBlacks > 2 then (c, White) else (c, Black)
              where
                numOfBlacks = length (filter (== Black) $ neighborColors c)
        )
        $ M.keys state

    neighborColors :: Coordinate -> [Color]
    neighborColors coordinate = map (\c -> M.lookupDefault White c state) $ neighbors coordinate

getNewState :: FloorState -> Coordinate -> Color
getNewState state coordinate = case currentState of
  White -> if blacks == 2 then Black else White
  Black -> if blacks == 0 || blacks > 2 then White else Black
  where
    currentState = M.lookupDefault White coordinate state
    neighborColors = map (\c -> M.lookupDefault White c state) $ neighbors coordinate
    blacks = length $ filter (== Black) neighborColors

nextCycle :: FloorState -> FloorState
nextCycle state = M.fromList $ map (\c -> (c, getNewState state c)) allCoordinates
  where
    coordinates = M.keys state
    allCoordinates = concatMap neighbors coordinates

cycleTimes :: Int -> Int -> FloorState -> FloorState
cycleTimes times acc state = if times == acc then state else cycleTimes times (acc + 1) (nextCycle state)

solveA :: String -> String
solveA s = show $ length $ filter (\(_, value) -> value == Black) $ M.toList state
  where
    coordinates = map ((`parseCoordinate` (0, 0)) . (`parseSteps` [])) $ lines s
    state = foldl f M.empty coordinates
    f state coordinate = M.insert coordinate newValue state
      where
        newValue = case M.lookupDefault White coordinate state of
          White -> Black
          Black -> White

solveB :: String -> String
solveB s = show $ length $ filter (\(_, value) -> value == Black) $ M.toList $ cycleTimes 100 0 initialState
  where
    coordinates = map ((`parseCoordinate` (0, 0)) . (`parseSteps` [])) $ lines s
    initialState = foldl f M.empty coordinates
    f state coordinate = M.insert coordinate newValue state
      where
        newValue = case M.lookupDefault White coordinate state of
          White -> Black
          Black -> White
