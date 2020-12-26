module Day25Test (tests) where

import Day25
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "5764801\n\
  \17807724"

tests =
  testGroup
    "Day25"
    [ testCase "solveA" $ solveA input @?= "14897079",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]
