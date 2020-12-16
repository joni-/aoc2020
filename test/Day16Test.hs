module Day16Test (tests) where

import Day16
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "class: 1-3 or 5-7\n\
  \row: 6-11 or 33-44\n\
  \seat: 13-40 or 45-50\n\
  \\n\
  \your ticket:\n\
  \7,1,14\n\
  \\n\
  \nearby tickets:\n\
  \7,3,47\n\
  \40,4,50\n\
  \55,2,20\n\
  \38,6,12"

tests =
  testGroup
    "Day16"
    [ testCase "solveA" $ solveA input @?= "71",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]
