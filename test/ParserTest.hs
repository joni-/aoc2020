module ParserTest where

import Parser
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "Parser"
    [ testCase "parseString" $ parseString "abc" "abcd" @?= Just ("abc", "d"),
      testCase "between" $ between '[' ']' "[null]" @?= Just ("null", ""),
      testCase "until" $ Parser.until ':' "beep:boop" @?= Just ("beep", "boop")
    ]
