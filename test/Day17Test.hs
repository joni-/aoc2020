module Day17Test (tests) where

import Day17
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  ".#.\n\
  \..#\n\
  \###"

tests =
  testGroup
    "Day17"
    [ testCase "solveA" $ solveA input @?= "112"
    ]
