module Day14 where

import Data.Char (digitToInt, intToDigit)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Numeric (readInt, showIntAtBase)
import Parser (between, parseInt)

input' =
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
  \mem[8] = 11\n\
  \mem[7] = 101\n\
  \mem[8] = 0\n"

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

lpad m c xs = replicate (m - length ys) c ++ ys
  where
    ys = take m xs

type Mask = String

type Memory = M.Map Integer Integer

type State = (Maybe Mask, Memory)

data Command = SetMask Mask | UpdateMemory Integer Integer deriving (Show)

applyMask :: Mask -> Integer -> Maybe Integer
applyMask mask value = fmap toInteger $ readBin $ zipWith (curry f) mask valueS
  where
    valueS = lpad 36 '0' $ showIntAtBase 2 intToDigit value ""
    f (m, c) = if m == 'X' then c else m

parseCommand :: String -> Maybe Command
parseCommand command = case command of
  ('m' : 'a' : 's' : 'k' : ' ' : '=' : ' ' : xs) -> Just (SetMask xs)
  ('m' : 'e' : 'm' : xs) -> case between '[' ']' xs of
    Just (memory, rest) -> case parseInt (drop 3 rest) of
      Just (value, _) -> Just $ UpdateMemory (read memory) (toInteger value)
      Nothing -> Nothing
    Nothing -> Nothing

updateState :: State -> Command -> State
updateState (mask, memory) command = case command of
  SetMask mask' -> (Just mask', memory)
  UpdateMemory address value -> case mask of
    Just mask' -> case applyMask mask' value of
      Just value' -> (mask, M.insert address value' memory)
      Nothing -> (mask, M.insert address value memory)
    Nothing -> (mask, M.insert address value memory)

solveA :: String -> String
solveA s = show $ sum $ map snd $ M.toList memory
  where
    commands = mapMaybe parseCommand $ lines s
    (_, memory) = foldl updateState (Nothing, M.empty) commands

solveB :: String -> String
solveB s = s
