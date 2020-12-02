module Day2Test (tests) where

import Day2
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Day2"
    [ testCase "solveA" $
        solveA
          "1-3 a: abcde\n\
          \1-3 b: cdefg\n\
          \2-9 c: ccccccccc"
          @?= "2",
      testCase "solveB" $
        solveB
          "1-3 a: abcde\n\
          \1-3 b: cdefg\n\
          \2-9 c: ccccccccc"
          @?= "1"
    ]