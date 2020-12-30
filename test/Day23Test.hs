module Day23Test (tests) where

import Day23
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Day23"
    [ testCase "solveA" $ solveA "389125467" @?= "67384529",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]
