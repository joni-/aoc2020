module Main where

import qualified Day1

main :: IO ()
main = do
  input <- readFile "inputs/day1.input"
  putStrLn $ Day1.solveA input
