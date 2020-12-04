module Day2 where

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Util (splitOn)

data Policy = Policy
  { policyMin :: Int,
    policyMax :: Int,
    policyChar :: Char
  }
  deriving (Show)

type Password = String

trim :: String -> Password
trim = T.unpack . T.strip . T.pack

parseRange :: String -> Maybe (Int, Int)
parseRange s = case splitOn " " s of
  (range : _) -> case splitOn "-" range of
    (x : y : _) -> Just (read x :: Int, read y :: Int)
    _ -> Nothing
  _ -> Nothing

parseChar :: String -> Maybe Char
parseChar s = case splitOn " " s of
  (_ : rest : _) -> case rest of
    (c : _) -> Just c
    _ -> Nothing

parsePassword :: String -> Maybe String
parsePassword s = case splitOn ":" s of
  (_ : password : _) -> Just (trim password)
  _ -> Nothing

parsePolicyAndPassword :: String -> Maybe (Policy, Password)
parsePolicyAndPassword s = result
  where
    range = parseRange s
    char = parseChar s
    password = parsePassword s
    result = case range of
      Just (min, max) -> case char of
        Just c -> case password of
          Just p -> Just (Policy {policyMin = min, policyMax = max, policyChar = c}, p)
          _ -> Nothing
        Nothing -> Nothing

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
solveA s = show $ length $ filter isValidA $ catMaybes $ parsePolicyAndPassword <$> lines s

solveB :: String -> String
solveB s = show $ length $ filter isValidB $ catMaybes $ parsePolicyAndPassword <$> lines s
