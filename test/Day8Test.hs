module Day8Test (tests) where

import Day8
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "nop +0\n\
  \acc +1\n\
  \jmp +4\n\
  \acc +3\n\
  \jmp -3\n\
  \acc -99\n\
  \acc +1\n\
  \jmp -4\n\
  \acc +6"

tests =
  testGroup
    "Day8"
    [ testCase "solveA" $ solveA input @?= "5",
      testCase "solveB" $ solveB input @?= "8"
    ]
