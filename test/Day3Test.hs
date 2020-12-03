module Day3Test (tests) where

import Day3
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input :: String
input =
  "..##.......\n\
  \#...#...#..\n\
  \.#....#..#.\n\
  \..#.#...#.#\n\
  \.#...##..#.\n\
  \..#.##.....\n\
  \.#.#.#....#\n\
  \.#........#\n\
  \#.##...#...\n\
  \#...##....#\n\
  \.#..#...#.#"

tests =
  testGroup
    "Day3"
    [ testCase "solveA" $ solveA input @?= "7",
      testCase "solveB" $ solveB input @?= "336"
    ]