module Day12 where

type Coordinate = (Int, Int)

data Orientation = North | South | East | West deriving (Show)

data Direction = Left | Right | Forward deriving (Show)

data Action = DirectionAction Direction Int | OrientationAction Orientation Int deriving (Show)

capDegrees :: Int -> Int
capDegrees v = v `mod` 360

turnDegrees :: Orientation -> Direction -> Int -> Orientation
turnDegrees North Day12.Left 90 = West
turnDegrees North Day12.Left 180 = South
turnDegrees North Day12.Left 270 = East
turnDegrees East Day12.Left 90 = North
turnDegrees East Day12.Left 180 = West
turnDegrees East Day12.Left 270 = South
turnDegrees South Day12.Left 90 = East
turnDegrees South Day12.Left 180 = North
turnDegrees South Day12.Left 270 = West
turnDegrees West Day12.Left 90 = South
turnDegrees West Day12.Left 180 = East
turnDegrees West Day12.Left 270 = North
turnDegrees North Day12.Right 90 = East
turnDegrees North Day12.Right 180 = South
turnDegrees North Day12.Right 270 = West
turnDegrees East Day12.Right 90 = South
turnDegrees East Day12.Right 180 = West
turnDegrees East Day12.Right 270 = North
turnDegrees South Day12.Right 90 = West
turnDegrees South Day12.Right 180 = North
turnDegrees South Day12.Right 270 = East
turnDegrees West Day12.Right 90 = North
turnDegrees West Day12.Right 180 = East
turnDegrees West Day12.Right 270 = South

turn :: Orientation -> Action -> Orientation
turn North (DirectionAction Day12.Left value) = turnDegrees North Day12.Left value
turn North (DirectionAction Day12.Right value) = turnDegrees North Day12.Right value
turn East (DirectionAction Day12.Left value) = turnDegrees East Day12.Left value
turn East (DirectionAction Day12.Right value) = turnDegrees East Day12.Right value
turn South (DirectionAction Day12.Left value) = turnDegrees South Day12.Left value
turn South (DirectionAction Day12.Right value) = turnDegrees South Day12.Right value
turn West (DirectionAction Day12.Left value) = turnDegrees West Day12.Left value
turn West (DirectionAction Day12.Right value) = turnDegrees West Day12.Right value
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

parseAction :: String -> Action
parseAction ('N' : value) = OrientationAction North (read value)
parseAction ('S' : value) = OrientationAction South (read value)
parseAction ('E' : value) = OrientationAction East (read value)
parseAction ('W' : value) = OrientationAction West (read value)
parseAction ('L' : value) = DirectionAction Day12.Left (capDegrees $ read value)
parseAction ('R' : value) = DirectionAction Day12.Right (capDegrees $ read value)
parseAction ('F' : value) = DirectionAction Forward (read value)

input =
  "F10\n\
  \N3\n\
  \F7\n\
  \R90\n\
  \F11"

solveA :: String -> String
solveA s = show $ abs (x + y)
  where
    actions = map parseAction $ lines s
    ((x, y), _) = foldl (\(coordinate, orientation) action -> move orientation action coordinate) ((0, 0), East) actions

solveB :: String -> String
solveB s = s
