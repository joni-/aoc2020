module Day25 where

import qualified Data.HashMap.Strict as M
import Debug.Trace (trace)

type SubjectNumber = Integer

type LoopSize = Integer

type PublicKey = Integer

type EncryptionKey = Integer

input' =
  "5764801\n\
  \17807724"

input'' =
  "12092626\n\
  \4707356"

loop :: SubjectNumber -> LoopSize -> Integer -> Integer -> Integer
loop subject loopSize value acc
  | loopSize == acc = value
  | otherwise = loop subject loopSize ((value * subject) `mod` 20201227) (acc + 1)

findLoopSize :: PublicKey -> LoopSize -> Integer -> Integer
findLoopSize target currentLoopSize previousValue
  | value == target = currentLoopSize + 1
  | otherwise = findLoopSize target (currentLoopSize + 1) value
  where
    value = loop 7 currentLoopSize previousValue (currentLoopSize - 1)

-- causes stack overflow with actual input..
solveA :: String -> String
solveA s = show $ loop pkDoor cardLoopSize 1 0
  where
    [pkCard, pkDoor] = map (\v -> read v :: Integer) $ lines s
    cardLoopSize = findLoopSize pkCard 0 1

solveB :: String -> String
solveB s = s
