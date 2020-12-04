import qualified Day1Test
import qualified Day2Test
import qualified Day3Test
import qualified Day4Test
-- start.py import placeholder
import Test.Tasty

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "AOC 2020 tests"
    [ Day1Test.tests
    , Day2Test.tests
    , Day3Test.tests
    , Day4Test.tests
    -- start.py test placeholder
    ]
