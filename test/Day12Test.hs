module Day12Test (tests) where

import Day12
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Day12"
    [ testCase "solveA" $ solveA "hello" @?= "hello",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]