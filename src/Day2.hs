module Day2 where

import qualified Data.Text as T

data Policy = Policy
  { getMin :: Int,
    getMax :: Int,
    getC :: Char
  }
  deriving (Show)

type Password = String

splitOn :: String -> String -> [String]
splitOn c s = map T.unpack $ T.splitOn (T.pack c) (T.pack s)

parsePassword :: String -> Password
parsePassword = T.unpack . T.strip . T.pack

parsePolicy :: String -> Policy
-- todo: unsafe head & head . tail
parsePolicy s = Policy {getMin = head result, getMax = (head . tail) result, getC = c}
  where
    -- todo: ensure there are 2 elements
    result = map (\v -> read v :: Int) $ splitOn "-" $ head $ splitOn " " s
    c = head $ last $ splitOn " " s

parseRow :: String -> (Policy, Password)
parseRow s = (parsePolicy policyString, parsePassword password)
  where
    policyString = head $ splitOn ":" s
    password = last $ splitOn ":" s

isValidA :: (Policy, Password) -> Bool
isValidA (policy, password) = numOfC >= getMin policy && numOfC <= getMax policy
  where
    numOfC = length $ filter (\c -> c == getC policy) password

isValidB :: (Policy, Password) -> Bool
isValidB (policy, password) = (== 1) $ length $ filter (== getC policy) [charAtFirst, charAtSecond]
  where
    -- todo: unsafe index usage
    charAtFirst = password !! (getMin policy - 1)
    charAtSecond = password !! (getMax policy - 1)

solveA :: String -> String
solveA s = show $ length $ filter isValidA $ map parseRow $ lines s

solveB :: String -> String
solveB s = show $ length $ filter isValidB $ map parseRow $ lines s