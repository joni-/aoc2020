module Day12Test (tests) where

import Day12
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "F10\n\
  \N3\n\
  \F7\n\
  \R90\n\
  \F11"

tests =
  testGroup
    "Day12"
    [ testCase "solveA" $ solveA input @?= "25",
      testCase "solveB" $ solveB input @?= "286"
    ]
