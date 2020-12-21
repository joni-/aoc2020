module Day21Test (tests) where

import Day21
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
  \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
  \sqjhc fvjkl (contains soy)\n\
  \sqjhc mxmxvkd sbzzf (contains fish)"

tests =
  testGroup
    "Day21"
    [ testCase "solveA" $ solveA input @?= "5",
      testCase "solveB" $ solveB input @?= "mxmxvkd,sqjhc,fvjkl"
    ]
