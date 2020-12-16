module Day16 where

import qualified Data.HashMap.Strict as M
import Data.List (isPrefixOf, transpose)
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

isValidTicket :: [Field] -> Ticket -> Bool
isValidTicket fields = all (`elem` validFieldValues)
  where
    validFieldValues = foldl (\acc (_, values) -> Set.union acc values) Set.empty fields

findInvalidValues :: [Field] -> [Ticket] -> [Int]
findInvalidValues fields tickets = invalidValues
  where
    validFieldValues = foldl (\acc (_, values) -> Set.union acc values) Set.empty fields
    invalidTickets = filter (not . isValidTicket fields) tickets
    invalidValues = filter (`notElem` validFieldValues) $ concat invalidTickets

filterInvalidTickets :: [Field] -> [Ticket] -> [Ticket]
filterInvalidTickets fields = filter (isValidTicket fields)

findFields :: [Field] -> [Ticket] -> [(Int, String)] -> [(Int, String)]
findFields fields tickets acc = if null fields then acc else findFields fieldsLeft tickets ff
  where
    columnized = transpose tickets
    f = map (\values -> map fst $ filter (\(field, allowedValues) -> all (`elem` allowedValues) values) fields) columnized
    ff = foldl (\acc' (value, index) -> if length value == 1 then acc' ++ [(index, head value)] else acc') acc (zip f [0 ..])
    fieldsFound = Set.fromList $ map snd ff
    fieldsLeft = filter (\(field, _) -> field `notElem` fieldsFound) fields

mapFields :: M.HashMap Int String -> Ticket -> M.HashMap String Int
mapFields mapping ticket = M.fromList $ map (\(value, index) -> (M.lookupDefault "unknown" index mapping, value)) $ zip ticket [0 ..]

solveA :: String -> String
solveA s = show $ sum $ findInvalidValues fields nearbyTickets
  where
    fields = parseFields s
    nearbyTickets = parseNearbyTickets s

solveB :: String -> String
solveB s = show $ product $ map snd $ M.toList $ M.filterWithKey (\k _ -> "departure" `isPrefixOf` k) $ mapFields fieldMapping myTicket
  where
    fields = parseFields s
    myTicket = parseMyTicket s
    nearbyTickets = parseNearbyTickets s ++ [myTicket]
    validTickets = filterInvalidTickets fields nearbyTickets
    columnized = transpose validTickets
    f = map (\values -> map fst $ filter (\(field, allowedValues) -> all (`elem` allowedValues) values) fields) columnized
    fieldMapping = M.fromList $ findFields fields validTickets []
