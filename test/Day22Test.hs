module Day22Test (tests) where

import Day22
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "Player 1:\n\
  \9\n\
  \2\n\
  \6\n\
  \3\n\
  \1\n\
  \\n\
  \Player 2:\n\
  \5\n\
  \8\n\
  \4\n\
  \7\n\
  \10"

tests =
  testGroup
    "Day22"
    [ testCase "solveA" $ solveA input @?= "306",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]
