module Day12 where

type Coordinate = (Int, Int)

data Orientation = North | South | East | West deriving (Show)

data Direction = Left | Right | Forward deriving (Show)

data Action = DirectionAction Direction Int | OrientationAction Orientation Int deriving (Show)

capDegrees :: Int -> Int
capDegrees v = v `mod` 360

next :: Orientation -> Orientation
next North = East
next East = South
next South = West
next West = North

clockwiseSteps :: Int -> Int
clockwiseSteps degrees = round (fromInteger (toInteger degrees) / 360 * 4) `mod` 4

counterClockwiseSteps :: Int -> Int
counterClockwiseSteps degrees = 4 - clockwiseSteps degrees

turnCounterClockwise :: Int -> Orientation -> Orientation
turnCounterClockwise degrees orientation = take steps (drop 1 $ iterate next orientation) !! max 0 (steps - 1)
  where
    steps = counterClockwiseSteps degrees

turnClockwise :: Int -> Orientation -> Orientation
turnClockwise degrees orientation = take steps (drop 1 $ iterate next orientation) !! max 0 (steps - 1)
  where
    steps = clockwiseSteps degrees

turn :: Orientation -> Action -> Orientation
turn orientation (DirectionAction Day12.Left degrees) = turnCounterClockwise degrees orientation
turn orientation (DirectionAction Day12.Right degrees) = turnClockwise degrees orientation
turn orientation _ = orientation

move :: Orientation -> Action -> Coordinate -> (Coordinate, Orientation)
move North (DirectionAction Forward value) (x, y) = ((x, y - value), North)
move East (DirectionAction Forward value) (x, y) = ((x + value, y), East)
move South (DirectionAction Forward value) (x, y) = ((x, y + value), South)
move West (DirectionAction Forward value) (x, y) = ((x - value, y), West)
move orientation (OrientationAction North value) (x, y) = ((x, y - value), orientation)
move orientation (OrientationAction East value) (x, y) = ((x + value, y), orientation)
move orientation (OrientationAction South value) (x, y) = ((x, y + value), orientation)
move orientation (OrientationAction West value) (x, y) = ((x - value, y), orientation)
move orientation (DirectionAction action value) coordinate = (coordinate, turn orientation (DirectionAction action value))

rotateClockwise :: Int -> Coordinate -> Coordinate
rotateClockwise 90 (x, y) = (negate y, x)
rotateClockwise 180 (x, y) = (negate x, negate y)
rotateClockwise 270 (x, y) = (y, negate x)
rotateClockwise 370 (x, y) = (x, y)

rotateCounterClockwise :: Int -> Coordinate -> Coordinate
rotateCounterClockwise 90 (x, y) = (y, negate x)
rotateCounterClockwise 180 (x, y) = (negate x, negate y)
rotateCounterClockwise 270 (x, y) = (negate y, x)
rotateCounterClockwise 370 (x, y) = (x, y)

move' :: Action -> Coordinate -> Coordinate -> (Coordinate, Coordinate)
move' (DirectionAction Forward value) (shipX, shipY) (waypointX, waypointY) = ((shipX', shipY'), (waypointX, waypointY))
  where
    shipX' = shipX + value * waypointX
    shipY' = shipY + value * waypointY
move' (OrientationAction North value) (shipX, shipY) (waypointX, waypointY) = ((shipX, shipY), (waypointX, waypointY - value))
move' (OrientationAction East value) (shipX, shipY) (waypointX, waypointY) = ((shipX, shipY), (waypointX + value, waypointY))
move' (OrientationAction South value) (shipX, shipY) (waypointX, waypointY) = ((shipX, shipY), (waypointX, waypointY + value))
move' (OrientationAction West value) (shipX, shipY) (waypointX, waypointY) = ((shipX, shipY), (waypointX - value, waypointY))
move' (DirectionAction Day12.Left degrees) shipCoord waypointCoord = (shipCoord, rotateCounterClockwise degrees waypointCoord)
move' (DirectionAction Day12.Right degrees) shipCoord waypointCoord = (shipCoord, rotateClockwise degrees waypointCoord)

parseAction :: String -> Action
parseAction ('N' : value) = OrientationAction North (read value)
parseAction ('S' : value) = OrientationAction South (read value)
parseAction ('E' : value) = OrientationAction East (read value)
parseAction ('W' : value) = OrientationAction West (read value)
parseAction ('L' : value) = DirectionAction Day12.Left (capDegrees $ read value)
parseAction ('R' : value) = DirectionAction Day12.Right (capDegrees $ read value)
parseAction ('F' : value) = DirectionAction Forward (read value)

solveA :: String -> String
solveA s = show $ abs (x + y)
  where
    actions = map parseAction $ lines s
    ((x, y), _) = foldl (\(coordinate, orientation) action -> move orientation action coordinate) ((0, 0), East) actions

solveB :: String -> String
solveB s = show $ abs (x + y)
  where
    actions = map parseAction $ lines s
    ((x, y), _) = foldl (\(shipCoord, waypointCoord) action -> move' action shipCoord waypointCoord) ((0, 0), (10, -1)) actions
