module Day24 where

import qualified Data.HashMap.Strict as M

type Coordinate = (Int, Int)

data Step = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Show)

data Color = White | Black deriving (Show, Eq)

input' =
  "sesenwnenenewseeswwswswwnenewsewsw\n\
  \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
  \seswneswswsenwwnwse\n\
  \nwnwneseeswswnenewneswwnewseswneseene\n\
  \swweswneswnenwsewnwneneseenw\n\
  \eesenwseswswnenwswnwnwsewwnwsene\n\
  \sewnenenenesenwsewnenwwwse\n\
  \wenwwweseeeweswwwnwwe\n\
  \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
  \neeswseenwwswnwswswnw\n\
  \nenwswwsewswnenenewsenwsenwnesesenew\n\
  \enewnwewneswsewnwswenweswnenwsenwsw\n\
  \sweneswneswneneenwnewenewwneswswnese\n\
  \swwesenesewenwneswnwwneseswwne\n\
  \enesenwswwswneneswsenwnewswseenwsese\n\
  \wnwnesenesenenwwnenwsewesewsesesew\n\
  \nenewswnwewswnenesenwnesewesw\n\
  \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
  \neswnwewnwnwseenwseesewsenwsweewe\n\
  \wseweeenwnesenwwwswnew"

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
solveB s = s
