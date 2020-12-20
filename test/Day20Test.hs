module Day20Test (tests) where

import Day20
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
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

tests =
  testGroup
    "Day20"
    [ testCase "solveA" $ solveA input @?= "20899048083289",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]