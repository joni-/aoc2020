module Day1Test (tests) where

import Day1
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Day 1"
    [ testCase "solveA" $
        solveA
          "1721 \
          \979 \
          \366 \
          \299 \
          \675 \
          \1456"
          @?= "514579",
      testCase "solveB" $
        solveB
          "1721 \
          \979 \
          \366 \
          \299 \
          \675 \
          \1456"
          @?= "241861950"
    ]
