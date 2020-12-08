module Day8 where

import Data.Array
import qualified Data.Set as S

data Instruction = Acc Int | NoOp Int | Jmp Int deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s = parse $ words s
  where
    parseValue ('+' : xs) = read xs :: Int
    parseValue xs = read xs :: Int

    parse ["acc", value] = Acc (parseValue value)
    parse ["nop", value] = NoOp (parseValue value)
    parse ["jmp", value] = Jmp (parseValue value)

parseInstructions :: [String] -> Array Int Instruction
parseInstructions lines = array (0, length lines - 1) indexed
  where
    indexed = map (\(a, b) -> (b, a)) $ zip (map parseInstruction lines) [0 ..]

-- return value is (accumulator, infinite loop or not)
execute :: Array Int Instruction -> S.Set Int -> Int -> Int -> (Int, Bool)
execute instructions visited index accumulator
  | index `elem` visited = (accumulator, True)
  | index >= (snd $ bounds instructions) = case currentInstruction of
    Acc value -> ((accumulator + value), False)
    NoOp _ -> (accumulator, False)
    Jmp _ -> (accumulator, False) -- should actually jump
  | otherwise = advance
  where
    currentInstruction = instructions ! index
    advance = case currentInstruction of
      Acc value -> execute instructions (S.insert index visited) (index + 1) (accumulator + value)
      NoOp _ -> execute instructions (S.insert index visited) (index + 1) accumulator
      Jmp value -> execute instructions (S.insert index visited) (index + value) accumulator

findBrokenInstruction :: Array Int Instruction -> Int -> (Int, Bool)
findBrokenInstruction instructions index = case execute newInstructions S.empty 0 0 of
  (accumulator, False) -> (accumulator, False)
  (_, True) -> findBrokenInstruction instructions (index + 1)
  where
    currentInstruction = instructions ! index
    newInstructions = case currentInstruction of
      Acc _ -> instructions
      NoOp value -> instructions // [(index, Jmp value)]
      Jmp value -> instructions // [(index, NoOp value)]

solveA :: String -> String
solveA s = show $ fst $ execute (parseInstructions (lines s)) S.empty 0 0

solveB :: String -> String
solveB s = show $ fst $ findBrokenInstruction (parseInstructions (lines s)) 0
