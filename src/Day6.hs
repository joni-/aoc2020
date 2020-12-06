module Day6 where

import Data.List (intercalate, intersect)
import qualified Data.Set as Set
import Util (splitOn)

type GroupAnswers = [String]

parseGroups :: String -> [GroupAnswers]
parseGroups s = map words $ splitOn "\n\n" s

anyoneAnsweredYes :: [GroupAnswers] -> Int
anyoneAnsweredYes anwers = sum $ map ((length . Set.fromList) . intercalate "") anwers

everyoneAnsweredYes :: [GroupAnswers] -> Int
everyoneAnsweredYes anwers = sum $ map (length . foldl1 intersect) anwers

solveA :: String -> String
solveA s = show $ anyoneAnsweredYes $ parseGroups s

solveB :: String -> String
solveB s = show $ everyoneAnsweredYes $ parseGroups s
