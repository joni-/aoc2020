module Day22 where

import Util (splitOn)

input' =
  "Player 1:\n\
  \9\n\
  \2\n\
  \6\n\
  \3\n\
  \1\n\
  \\n\
  \Player 2:\n\
  \5\n\
  \8\n\
  \4\n\
  \7\n\
  \10"

parseDecks :: String -> ([Int], [Int])
parseDecks input = (p1', p2')
  where
    [p1, p2] = splitOn "\n\n" input
    p1' = map read $ tail $ lines p1
    p2' = map read $ tail $ lines p2

play :: ([Int], [Int]) -> ([Int], [Int])
play (deck1, deck2) = if null deck1 || null deck2 then (deck1, deck2) else play (deck1', deck2')
  where
    c1 = head deck1
    c2 = head deck2
    deck1' = if c1 > c2 then tail deck1 ++ [c1, c2] else tail deck1
    deck2' = if c2 > c1 then tail deck2 ++ [c2, c1] else tail deck2

solveA :: String -> String
solveA s = show $ sum $ map (\(a, b) -> a * b) $ zip (reverse winningDeck) [1 ..]
  where
    winningDeck = case play $ parseDecks s of
      (deck, []) -> deck
      ([], deck) -> deck

solveB :: String -> String
solveB s = s
