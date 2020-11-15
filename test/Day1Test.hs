module Day1Test (tests) where

import Day1
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Day 1"
    [ testCase "solveA" $ solveA "hello" @?= "hello",
      testCase "solveB" $ solveA "hello" @?= "hello"
    ]
