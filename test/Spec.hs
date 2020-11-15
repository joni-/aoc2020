import qualified Day1Test
import Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "AOC 2020 tests" [Day1Test.tests]
