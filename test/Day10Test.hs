module Day10Test (tests) where

import Day10
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input1 =
  "16\n\
  \10\n\
  \15\n\
  \5\n\
  \1\n\
  \11\n\
  \7\n\
  \19\n\
  \6\n\
  \12\n\
  \4"

input2 =
  "28\n\
  \33\n\
  \18\n\
  \42\n\
  \31\n\
  \14\n\
  \46\n\
  \20\n\
  \48\n\
  \47\n\
  \24\n\
  \23\n\
  \49\n\
  \45\n\
  \19\n\
  \38\n\
  \39\n\
  \11\n\
  \1\n\
  \32\n\
  \25\n\
  \35\n\
  \8\n\
  \17\n\
  \7\n\
  \9\n\
  \4\n\
  \2\n\
  \34\n\
  \10\n\
  \3"

tests =
  testGroup
    "Day10"
    [ testCase "solveA 1" $ solveA input1 @?= "35",
      testCase "solveA 2" $ solveA input2 @?= "220",
      testCase "solveB" $ solveB input1 @?= "8",
      testCase "solveB" $ solveB input2 @?= "19208"
    ]
