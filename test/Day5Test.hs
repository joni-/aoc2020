module Day5Test (tests) where

import Day5
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input :: String
input =
  "BFFFBBFRRR\n\
  \FFFBBBFRRR\n\
  \BBFFBBFRLL"

tests =
  testGroup
    "Day5"
    [ testCase "solveA" $ solveA input @?= "820",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]
