module Day14Test (tests) where

import Day14
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input1 =
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
  \mem[8] = 11\n\
  \mem[7] = 101\n\
  \mem[8] = 0\n"

input2 =
  "mask = 000000000000000000000000000000X1001X\n\
  \mem[42] = 100\n\
  \mask = 00000000000000000000000000000000X0XX\n\
  \mem[26] = 1"

tests =
  testGroup
    "Day14"
    [ testCase "solveA" $ solveA input1 @?= "165",
      testCase "solveB" $ solveB input2 @?= "208"
    ]
