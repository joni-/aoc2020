module Day2 where

import Data.Maybe (catMaybes, mapMaybe)
import Parser
import Util (splitOn, trim)

data Policy = Policy
  { policyMin :: Int,
    policyMax :: Int,
    policyChar :: Char
  }
  deriving (Show)

type Password = String

parsePolicyAndPassword :: Parser (Policy, Password)
parsePolicyAndPassword = do
  min <- head <$> many1 intP
  charP '-'
  max <- head <$> many1 intP
  whitespace
  char <- anyChar
  charP ':'
  whitespace
  password <- many anyChar
  return (Policy {policyMin = min, policyMax = max, policyChar = char}, password)

isValidA :: (Policy, Password) -> Bool
isValidA (policy, password) = count >= policyMin policy && count <= policyMax policy
  where
    count = length $ filter (\c -> c == policyChar policy) password

isValidB :: (Policy, Password) -> Bool
isValidB (policy, password) = (== 1) $ length $ filter (== policyChar policy) [charAtFirst, charAtSecond]
  where
    -- todo: unsafe index usage
    charAtFirst = password !! (policyMin policy - 1)
    charAtSecond = password !! (policyMax policy - 1)

solveA :: String -> String
solveA s = show $ length $ filter isValidA $ mapMaybe (fmap fst . runParser parsePolicyAndPassword) $ lines s

solveB :: String -> String
solveB s = show $ length $ filter isValidB $ mapMaybe (fmap fst . runParser parsePolicyAndPassword) $ lines s
