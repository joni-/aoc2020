module Day19 where

import Control.Applicative (empty, (<|>))
import qualified Data.HashMap.Strict as M
import Data.Maybe (mapMaybe)
import Parser
import Util

data Rule = Ref [[Int]] | Val String deriving (Show)

type RuleMap = M.HashMap Int Rule

type ParserMap = M.HashMap Int (Parser String)

createParser :: RuleMap -> Rule -> Parser String
createParser _ (Val s) = stringP s
createParser deps (Ref refs) = m
  where
    m = concat <$> foldl (\acc b -> p b <|> acc) empty refs
    p refs' = traverse (createParser deps) $ mapMaybe (`M.lookup` deps) refs'

createParsers :: RuleMap -> ParserMap
createParsers deps = M.fromList $ map (\(k, v) -> (k, createParser deps v)) $ M.toList deps

evalRule :: ParserMap -> Int -> String -> Maybe (String, String)
evalRule deps rule s = case M.lookup rule deps of
  Just v -> case runParser v s of
    Just (v, input') -> if null input' then Just (v, input') else Nothing
    Nothing -> Nothing
  Nothing -> Nothing

parse :: String -> RuleMap
parse input = M.fromList $ map parseLine $ lines input
  where
    parseLine :: String -> (Int, Rule)
    parseLine input = (n', rules')
      where
        [n, rules] = splitOn ": " input
        n' = read n :: Int
        rules' = case between '"' '"' rules of
          Just (v, _) -> Val v
          Nothing -> Ref $ map (map (\v -> read v :: Int) . splitOn " ") $ splitOn " | " rules

solveA :: String -> String
solveA s = show $ length results
  where
    [rules, inputs] = splitOn "\n\n" s
    parsers = createParsers $ parse rules
    results = mapMaybe (evalRule parsers 0) $ lines inputs

solveB :: String -> String
solveB s = s
