module Day20 where

import Data.Bifunctor (second)
import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Parser (parseInt, parseString, until)
import Util (splitOn)

data State = On | Off deriving (Show, Eq)

type Tile = [[State]]

type TileId = Int

parseTile :: String -> Maybe (TileId, Tile)
parseTile input = fmap (\id -> (id, parseImageContent (tail parts))) (parseTileNumber $ head parts)
  where
    parts = lines input

    parseTileNumber :: String -> Maybe Int
    parseTileNumber input = do
      (_, rest) <- parseString "Tile " input
      (value, _) <- Parser.until ':' rest
      (iValue, _) <- parseInt value
      return iValue

    parseImageContent :: [String] -> Tile
    parseImageContent rows = map (map (\c -> if c == '#' then On else Off)) rows

parseInput :: String -> [(TileId, Tile)]
parseInput input = mapMaybe parseTile $ splitOn "\n\n" input

getBorders :: Tile -> [[State]]
getBorders tile = ar ++ map reverse ar
  where
    ar = [topRow, bottomRow, leftCol, rightCol]
    topRow = head tile
    bottomRow = last tile
    transposed = transpose tile
    leftCol = head transposed
    rightCol = last transposed

tilesWithoutMatchingBorder :: [(TileId, Tile)] -> [TileId]
tilesWithoutMatchingBorder tiles = cornerTileIds
  where
    borders = map (second getBorders) tiles
    cornerTileIds = map fst $ filter (uncurry isCorner) borders
    isCorner id tileBorders = matches == 4
      where
        candidates = concatMap snd $ filter (\(id', _) -> id' /= id) borders
        matches = sum $ map (\v -> length $ filter (== v) candidates) tileBorders

solveA :: String -> String
solveA s = show $ product $ tilesWithoutMatchingBorder $ parseInput s

solveB :: String -> String
solveB s = s

input' =
  "Tile 2311:\n\
  \..##.#..#.\n\
  \##..#.....\n\
  \#...##..#.\n\
  \####.#...#\n\
  \##.##.###.\n\
  \##...#.###\n\
  \.#.#.#..##\n\
  \..#....#..\n\
  \###...#.#.\n\
  \..###..###\n\
  \\n\
  \Tile 1951:\n\
  \#.##...##.\n\
  \#.####...#\n\
  \.....#..##\n\
  \#...######\n\
  \.##.#....#\n\
  \.###.#####\n\
  \###.##.##.\n\
  \.###....#.\n\
  \..#.#..#.#\n\
  \#...##.#..\n\
  \\n\
  \Tile 1171:\n\
  \####...##.\n\
  \#..##.#..#\n\
  \##.#..#.#.\n\
  \.###.####.\n\
  \..###.####\n\
  \.##....##.\n\
  \.#...####.\n\
  \#.##.####.\n\
  \####..#...\n\
  \.....##...\n\
  \\n\
  \Tile 1427:\n\
  \###.##.#..\n\
  \.#..#.##..\n\
  \.#.##.#..#\n\
  \#.#.#.##.#\n\
  \....#...##\n\
  \...##..##.\n\
  \...#.#####\n\
  \.#.####.#.\n\
  \..#..###.#\n\
  \..##.#..#.\n\
  \\n\
  \Tile 1489:\n\
  \##.#.#....\n\
  \..##...#..\n\
  \.##..##...\n\
  \..#...#...\n\
  \#####...#.\n\
  \#..#.#.#.#\n\
  \...#.#.#..\n\
  \##.#...##.\n\
  \..##.##.##\n\
  \###.##.#..\n\
  \\n\
  \Tile 2473:\n\
  \#....####.\n\
  \#..#.##...\n\
  \#.##..#...\n\
  \######.#.#\n\
  \.#...#.#.#\n\
  \.#########\n\
  \.###.#..#.\n\
  \########.#\n\
  \##...##.#.\n\
  \..###.#.#.\n\
  \\n\
  \Tile 2971:\n\
  \..#.#....#\n\
  \#...###...\n\
  \#.#.###...\n\
  \##.##..#..\n\
  \.#####..##\n\
  \.#..####.#\n\
  \#..#.#..#.\n\
  \..####.###\n\
  \..#.#.###.\n\
  \...#.#.#.#\n\
  \\n\
  \Tile 2729:\n\
  \...#.#.#.#\n\
  \####.#....\n\
  \..#.#.....\n\
  \....#..#.#\n\
  \.##..##.#.\n\
  \.#.####...\n\
  \####.#.#..\n\
  \##.####...\n\
  \##..#.##..\n\
  \#.##...##.\n\
  \\n\
  \Tile 3079:\n\
  \#.#.#####.\n\
  \.#..######\n\
  \..#.......\n\
  \######....\n\
  \####.#..#.\n\
  \.#...#.##.\n\
  \#.#####.##\n\
  \..#.###...\n\
  \..#.......\n\
  \..#.###..."
