module Day18 where

import Data.Char (digitToInt)

data Token = ValueToken Int | AddT | MultT | SubT [Token] deriving (Show)

parseT :: String -> [Token] -> ([Token], String)
parseT "" acc = (acc, "")
parseT (' ' : xs) acc = parseT xs acc
parseT ('(' : xs) acc = parseT xs' (acc ++ [SubT subt])
  where
    (subt, xs') = parseT xs []
parseT (')' : xs) acc = (acc, xs)
parseT ('*' : xs) acc = parseT xs (acc ++ [MultT])
parseT ('+' : xs) acc = parseT xs (acc ++ [AddT])
parseT (x : xs) acc = parseT xs (acc ++ [ValueToken $ digitToInt x])

parseT' :: String -> [Token]
parseT' input = fst $ parseT input []

evalT :: [Token] -> [Token]
evalT [x] = [x]
evalT (x : y : z : xs) = evalT (ValueToken zz : xs)
  where
    x' = case x of
      SubT tokens -> head $ evalT tokens
      ValueToken v -> ValueToken v
    z' = case z of
      SubT tokens -> head $ evalT tokens
      ValueToken v -> ValueToken v

    x'' = case x' of
      ValueToken v -> v
    z'' = case z' of
      ValueToken v -> v

    zz = case y of
      AddT -> x'' + z''
      MultT -> x'' * z''

evalT' :: [Token] -> Int
evalT' tokens = case head $ evalT tokens of
  ValueToken v -> v

solveA :: String -> String
solveA s = show $ sum $ map (evalT' . parseT') $ lines s

solveB :: String -> String
solveB s = s
