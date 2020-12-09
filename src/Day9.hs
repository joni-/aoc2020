module Day9 where

import qualified Data.Set as S

input2 =
  "35\n\
  \20\n\
  \15\n\
  \25\n\
  \47\n\
  \40\n\
  \62\n\
  \55\n\
  \65\n\
  \95\n\
  \102\n\
  \117\n\
  \150\n\
  \182\n\
  \127\n\
  \219\n\
  \299\n\
  \277\n\
  \309\n\
  \576\n"

isSum :: S.Set Int -> Int -> Bool
isSum previous target = any (\v -> target - v `elem` S.delete v previous) previous

findEncodingError :: Int -> [Int] -> Int
findEncodingError preamble numbers = fst $ head $ filter (not . snd) results
  where
    results = map f [preamble .. (length numbers - 1)]
    f index = (numbers !! index, isSum (S.fromList $ take preamble $ drop (index - preamble) numbers) (numbers !! index))

solveA :: String -> String
solveA s = show $ findEncodingError 25 $ map (\v -> read v :: Int) $ lines s

solveB :: String -> String
solveB s = s
