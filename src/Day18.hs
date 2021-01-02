module Day18 where

import Data.Char (digitToInt)

data Token = ValueToken Integer | AddT | MultT | SubT [Token] deriving (Show)

parseT :: String -> [Token] -> ([Token], String)
parseT "" acc = (acc, "")
parseT (' ' : xs) acc = parseT xs acc
parseT ('(' : xs) acc = parseT xs' (acc ++ [SubT subt])
  where
    (subt, xs') = parseT xs []
parseT (')' : xs) acc = (acc, xs)
parseT ('*' : xs) acc = parseT xs (acc ++ [MultT])
parseT ('+' : xs) acc = parseT xs (acc ++ [AddT])
parseT (x : xs) acc = parseT xs (acc ++ [ValueToken $ toInteger $ digitToInt x])

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

evalT' :: [Token] -> Integer
evalT' tokens = case head $ evalT tokens of
  ValueToken v -> v

solveA :: String -> String
solveA s = show $ sum $ map (evalT' . parseT') $ lines s

evalTAddT :: [Token] -> [Token] -> [Token]
evalTAddT acc [x] = case x of
  ValueToken v -> acc ++ [ValueToken v]
  SubT tokens -> acc ++ [fullEvalB tokens]
evalTAddT acc (x : y : z : xs) = evalTAddT acc' (zz ++ xs)
  where
    x' = case x of
      SubT tokens -> fullEvalB tokens
      ValueToken v -> ValueToken v
    z' = case z of
      SubT tokens -> fullEvalB tokens
      ValueToken v -> ValueToken v

    x'' = case x' of
      ValueToken v -> v
    z'' = case z' of
      ValueToken v -> v

    zz = case y of
      AddT -> [ValueToken $ x'' + z'']
      MultT -> [z']

    acc' = case y of
      AddT -> acc
      MultT -> acc ++ [x', y]

fullEvalB :: [Token] -> Token
fullEvalB tokens = head $ evalT $ evalTAddT [] tokens

fullEvalB' :: [Token] -> Integer
fullEvalB' tokens = case fullEvalB tokens of
  ValueToken v -> v

solveB :: String -> String
solveB s = show $ sum $ map (fullEvalB' . parseT') $ lines s
