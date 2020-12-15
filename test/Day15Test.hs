module Day15Test (tests) where

import Day15
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Day15"
    [ testCase "solveA 0,3,6" $ solveA "0,3,6" @?= "436",
      testCase "solveA 1,3,2" $ solveA "1,3,2" @?= "1",
      testCase "solveA 2,1,3" $ solveA "2,1,3" @?= "10",
      testCase "solveA 1,2,3" $ solveA "1,2,3" @?= "27",
      testCase "solveA 2,3,1" $ solveA "2,3,1" @?= "78",
      testCase "solveA 3,2,1" $ solveA "3,2,1" @?= "438",
      testCase "solveA 3,1,2" $ solveA "3,1,2" @?= "1836",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]
