module Day14Test (tests) where

import Day14
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Day14"
    [ testCase "solveA" $ solveA "hello" @?= "hello",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]