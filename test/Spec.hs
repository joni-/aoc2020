import qualified Day1Test
import qualified Day2Test
import qualified Day3Test
import qualified Day4Test
import qualified Day5Test
import qualified ParserTest
import qualified Day6Test
import qualified Day7Test
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
    , Day5Test.tests
    , ParserTest.tests
    , Day6Test.tests
    , Day7Test.tests
    -- start.py test placeholder
    ]
