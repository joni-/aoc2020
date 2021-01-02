module Day18Test (tests) where

import Day18
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "2 * 3 + (4 * 5)\n\
  \5 + (8 * 3 + 9 + 3 * 4 * 3)\n\
  \5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))\n\
  \((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

tests =
  testGroup
    "Day18"
    [ testCase "solveA" $ solveA input @?= "26335",
      testCase "solveB" $ solveB input @?= "693891"
    ]
