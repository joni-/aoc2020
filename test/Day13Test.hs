module Day13Test (tests) where

import Day13
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "939\n\
  \7,13,x,x,59,x,31,19"

tests =
  testGroup
    "Day13"
    [ testCase "solveA" $ solveA input @?= "295",
      testCase "solveB" $ solveB input @?= "1068781"
    ]
