import qualified Day1Test
import qualified Day2Test
import qualified Day3Test
import qualified Day4Test
import qualified Day5Test
import qualified ParserTest
import qualified Day6Test
import qualified Day7Test
import qualified Day8Test
import qualified Day9Test
import qualified Day10Test
import qualified Day11Test
import qualified Day12Test
import qualified Day13Test
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
    , Day8Test.tests
    , Day9Test.tests
    , Day10Test.tests
    , Day11Test.tests
    , Day12Test.tests
    , Day13Test.tests
    -- start.py test placeholder
    ]
