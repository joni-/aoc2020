module Main where

import Data.Char
import qualified Data.HashMap.Strict as HashMap
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
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
