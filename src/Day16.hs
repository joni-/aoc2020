module Day16 where

import qualified Data.Set as Set
import Util (splitOn, trim)

type Field = (String, Set.Set Int)

type Ticket = [Int]

parseField :: String -> Field
parseField s = (name, Set.fromList ranges)
  where
    parts = splitOn ":" s
    name = head parts
    ranges = concat $ map (\pair -> [head pair .. (head $ tail pair)]) $ map (\pair -> map (\v -> read v :: Int) pair) $ map (\pair -> map trim pair) $ map (splitOn "-") $ splitOn "or" $ head $ tail parts

parseFields :: String -> [Field]
parseFields s = map parseField $ filter (not . null) $ map trim $ lines $ head $ splitOn "your ticket:\n" s

parseMyTicket :: String -> Ticket
parseMyTicket s = map (\v -> read v :: Int) $ splitOn "," $ head $ lines $ (head . tail) $ splitOn "your ticket:\n" s

parseNearbyTickets :: String -> [Ticket]
parseNearbyTickets s = map (\l -> map (\v -> read v :: Int) l) $ map (splitOn ",") $ lines $ head $ tail $ splitOn "nearby tickets:\n" s

findInvalidValues :: [Field] -> [Ticket] -> [Int]
findInvalidValues fields tickets = invalidValues
  where
    validFieldValues = foldl (\acc (_, values) -> Set.union acc values) Set.empty fields
    invalidTickets = filter (\ticket -> not $ all (\v -> v `elem` validFieldValues) ticket) tickets
    invalidValues = filter (\v -> not $ v `elem` validFieldValues) $ concat invalidTickets

input' =
  "class: 1-3 or 5-7\n\
  \row: 6-11 or 33-44\n\
  \seat: 13-40 or 45-50\n\
  \\n\
  \your ticket:\n\
  \7,1,14\n\
  \\n\
  \nearby tickets:\n\
  \7,3,47\n\
  \40,4,50\n\
  \55,2,20\n\
  \38,6,12"

solveA :: String -> String
solveA s = show $ sum $ findInvalidValues fields nearbyTickets
  where
    fields = parseFields s
    nearbyTickets = parseNearbyTickets s

solveB :: String -> String
solveB s = s
