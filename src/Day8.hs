module Day8 where

import Data.Array
import qualified Data.Set as S

data Instruction = Acc Int | NoOp | Jmp Int deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s = parse $ words s
  where
    parseValue ('+' : xs) = read xs :: Int
    parseValue xs = read xs :: Int

    parse ["acc", value] = Acc (parseValue value)
    parse ["nop", _] = NoOp
    parse ["jmp", value] = Jmp (parseValue value)

parseInstructions :: [String] -> Array Int Instruction
parseInstructions lines = array (0, length lines - 1) indexed
  where
    indexed = map (\(a, b) -> (b, a)) $ zip (map parseInstruction lines) [0 ..]

findLoop :: Array Int Instruction -> S.Set Int -> Int -> Int -> Int
findLoop instructions visited index accumulator = if index `elem` visited then accumulator else advance
  where
    currentInstruction = instructions ! index
    advance = case currentInstruction of
      Acc value -> findLoop instructions (S.insert index visited) (index + 1) (accumulator + value)
      NoOp -> findLoop instructions (S.insert index visited) (index + 1) accumulator
      Jmp value -> findLoop instructions (S.insert index visited) (index + value) accumulator

solveA :: String -> String
solveA s = show $ findLoop (parseInstructions (lines s)) S.empty 0 0

solveB :: String -> String
solveB s = s
