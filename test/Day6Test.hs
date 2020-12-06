module Day6Test (tests) where

import Day6
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input :: String
input =
  "abc\n\
  \\n\
  \a\n\
  \b\n\
  \c\n\
  \\n\
  \ab\n\
  \ac\n\
  \\n\
  \a\n\
  \a\n\
  \a\n\
  \a\n\
  \\n\
  \b"

tests =
  testGroup
    "Day6"
    [ testCase "solveA" $ solveA input @?= "11",
      testCase "solveB" $ solveB input @?= "6"
    ]
