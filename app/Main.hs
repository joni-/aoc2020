module Main where

import Data.Char
import qualified Data.HashMap.Strict as HashMap
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day20
import qualified Day21
-- start.py import placeholder
import System.Environment

solveFns :: HashMap.HashMap (String, String) (String -> String)
solveFns =
  HashMap.fromList
    [ (("1", "A"), Day1.solveA)
    , (("1", "B"), Day1.solveB)
    , (("2", "A"), Day2.solveA)
    , (("2", "B"), Day2.solveB)
    , (("3", "A"), Day3.solveA)
    , (("3", "B"), Day3.solveB)
    , (("4", "A"), Day4.solveA)
    , (("4", "B"), Day4.solveB)
    , (("5", "A"), Day5.solveA)
    , (("5", "B"), Day5.solveB)
    , (("6", "A"), Day6.solveA)
    , (("6", "B"), Day6.solveB)
    , (("7", "A"), Day7.solveA)
    , (("7", "B"), Day7.solveB)
    , (("8", "A"), Day8.solveA)
    , (("8", "B"), Day8.solveB)
    , (("9", "A"), Day9.solveA)
    , (("9", "B"), Day9.solveB)
    , (("10", "A"), Day10.solveA)
    , (("10", "B"), Day10.solveB)
    , (("11", "A"), Day11.solveA)
    , (("11", "B"), Day11.solveB)
    , (("12", "A"), Day12.solveA)
    , (("12", "B"), Day12.solveB)
    , (("13", "A"), Day13.solveA)
    , (("13", "B"), Day13.solveB)
    , (("14", "A"), Day14.solveA)
    , (("14", "B"), Day14.solveB)
    , (("15", "A"), Day15.solveA)
    , (("15", "B"), Day15.solveB)
    , (("16", "A"), Day16.solveA)
    , (("16", "B"), Day16.solveB)
    , (("17", "A"), Day17.solveA)
    , (("17", "B"), Day17.solveB)
    , (("20", "A"), Day20.solveA)
    , (("20", "B"), Day20.solveB)
    , (("21", "A"), Day21.solveA)
    , (("21", "B"), Day21.solveB)
    -- start.py placeholder
    ]

main :: IO ()
main = do
  [day, part] <- getArgs
  let solverFn = HashMap.lookup (day, map toUpper part) solveFns
  case solverFn of
    Just fn -> do
      interact fn
    Nothing -> error "Invalid params"
