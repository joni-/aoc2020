module Day11Test (tests) where

import Day11
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "L.LL.LL.LL\n\
  \LLLLLLL.LL\n\
  \L.L.L..L..\n\
  \LLLL.LL.LL\n\
  \L.LL.LL.LL\n\
  \L.LLLLL.LL\n\
  \..L.L.....\n\
  \LLLLLLLLLL\n\
  \L.LLLLLL.L\n\
  \L.LLLLL.LL"

tests =
  testGroup
    "Day11"
    [ testCase "solveA" $ solveA input @?= "37",
      testCase "solveB" $ solveB input @?= "26"
    ]
