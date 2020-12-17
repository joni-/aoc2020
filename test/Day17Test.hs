module Day17Test (tests) where

import Day17
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Day17"
    [ testCase "solveA" $ solveA "hello" @?= "hello",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]