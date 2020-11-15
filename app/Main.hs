module Main where

import Data.Char
import qualified Data.HashMap.Strict as HashMap
import qualified Day1
-- start.py import placeholder
import System.Environment

solveFns :: HashMap.HashMap (String, String) (String -> String)
solveFns =
  HashMap.fromList
    [ (("1", "A"), Day1.solveA)
    , (("1", "B"), Day1.solveB)
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
