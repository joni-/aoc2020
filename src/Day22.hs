module Day22 where

import qualified Data.Set as Set
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

playGame :: ([Int], [Int]) -> ([Int], [Int])
playGame decks@(deck1, deck2) = if null deck1 || null deck2 then decks else playRound Set.empty decks

playRound :: Set.Set ([Int], [Int]) -> ([Int], [Int]) -> ([Int], [Int])
playRound previousStartingDecks decks@(deck1, deck2) = if null deck1 || null deck2 || decks `Set.member` previousStartingDecks then decks else f decks
  where
    f (c1 : deck1', c2 : deck2') =
      if length deck1' >= c1 && length deck2' >= c2
        then case playGame (take c1 deck1', take c2 deck2') of
          (_, []) -> playRound (Set.insert decks previousStartingDecks) (deck1' ++ [c1, c2], deck2')
          ([], p2) -> playRound (Set.insert decks previousStartingDecks) (deck1', deck2' ++ [c2, c1])
          -- found duplicate starting hands -> player 1 won
          _ -> playRound (Set.insert decks previousStartingDecks) (deck1' ++ [c1, c2], deck2')
        else -- (a, b) -> (a, b)
          playRound (Set.insert decks previousStartingDecks) (deck1'', deck2'')
      where
        deck1'' = if c1 > c2 then deck1' ++ [c1, c2] else deck1'
        deck2'' = if c2 > c1 then deck2' ++ [c2, c1] else deck2'

solveA :: String -> String
solveA s = show $ sum $ map (\(a, b) -> a * b) $ zip (reverse winningDeck) [1 ..]
  where
    winningDeck = case play $ parseDecks s of
      (deck, []) -> deck
      ([], deck) -> deck

solveB :: String -> String
solveB s = show $ sum $ map (\(a, b) -> a * b) $ zip (reverse winningDeck) [1 ..]
  where
    winningDeck = case playGame $ parseDecks s of
      (deck, []) -> deck
      ([], deck) -> deck
