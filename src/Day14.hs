module Day14 where

import Control.Monad (replicateM)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (toList)
import Data.List (findIndices)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import Numeric (readInt, showIntAtBase)
import Parser (between, parseInt)

-- stolen from stack overflow
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

lpad m c xs = replicate (m - length ys) c ++ ys
  where
    ys = take m xs

type Mask = String

type Memory = M.Map Integer Integer

type State = (Maybe Mask, Memory)

data Command = SetMask Mask | UpdateMemory Integer Integer deriving (Show)

applyMaskV1 :: Mask -> Integer -> Maybe Integer
applyMaskV1 mask value = fmap toInteger $ readBin $ zipWith (curry f) mask valueS
  where
    valueS = lpad 36 '0' $ showIntAtBase 2 intToDigit value ""
    f (m, c) = if m == 'X' then c else m

replaceXs :: String -> [String]
replaceXs input = map applyReplacements allReplacements
  where
    -- find indices of Xs to replace in the input
    indices = findIndices (`elem` "X") input
    count = length indices
    -- all permutations of length "count" of 0s and 1s
    permutations = replicateM count "01"
    -- build a list of list of replacements where the inner list tells the
    -- replacement pattern for one permutation
    allReplacements = map (zip indices) permutations
    applyReplacements replacements = toList $ foldl (\acc (i, c) -> Seq.update i c acc) (Seq.fromList input) replacements

applyMaskV2 :: Mask -> Integer -> [Integer]
applyMaskV2 mask value = mapMaybe readBin (replaceXs $ zipWith (curry f) mask valueS)
  where
    valueS = lpad 36 '0' $ showIntAtBase 2 intToDigit value ""
    f ('0', c) = c
    f ('1', _) = '1'
    f (m, _) = m

parseCommand :: String -> Maybe Command
parseCommand command = case command of
  ('m' : 'a' : 's' : 'k' : ' ' : '=' : ' ' : xs) -> Just (SetMask xs)
  ('m' : 'e' : 'm' : xs) -> case between '[' ']' xs of
    Just (memory, rest) -> case parseInt (drop 3 rest) of
      Just (value, _) -> Just $ UpdateMemory (read memory) (toInteger value)
      Nothing -> Nothing
    Nothing -> Nothing

updateStateV1 :: State -> Integer -> Integer -> State
updateStateV1 (mask, memory) address value = case mask of
  Just mask' -> case applyMaskV1 mask' value of
    Just value' -> (mask, M.insert address value' memory)
    Nothing -> (mask, M.insert address value memory)
  Nothing -> (mask, memory)

updateStateV2 :: State -> Integer -> Integer -> State
updateStateV2 (mask, memory) address value = case mask of
  Just mask' -> (mask, foldl (\mem addr -> M.insert addr value mem) memory addresses)
    where
      addresses = applyMaskV2 mask' address
  Nothing -> (mask, memory)

runCommand :: (State -> Integer -> Integer -> State) -> State -> Command -> State
runCommand updateFn (mask, memory) command = case command of
  SetMask mask' -> (Just mask', memory)
  UpdateMemory address value -> updateFn (mask, memory) address value

solve' :: (State -> Command -> State) -> String -> String
solve' updateFn s = show $ sum $ map snd $ M.toList memory
  where
    commands = mapMaybe parseCommand $ lines s
    (_, memory) = foldl updateFn (Nothing, M.empty) commands

solveA :: String -> String
solveA = solve' (runCommand updateStateV1)

solveB :: String -> String
solveB = solve' (runCommand updateStateV2)
