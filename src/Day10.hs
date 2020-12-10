module Day10 where

import qualified Data.Set as S

input1' =
  "16\n\
  \10\n\
  \15\n\
  \5\n\
  \1\n\
  \11\n\
  \7\n\
  \19\n\
  \6\n\
  \12\n\
  \4"

input2' =
  "28\n\
  \33\n\
  \18\n\
  \42\n\
  \31\n\
  \14\n\
  \46\n\
  \20\n\
  \48\n\
  \47\n\
  \24\n\
  \23\n\
  \49\n\
  \45\n\
  \19\n\
  \38\n\
  \39\n\
  \11\n\
  \1\n\
  \32\n\
  \25\n\
  \35\n\
  \8\n\
  \17\n\
  \7\n\
  \9\n\
  \4\n\
  \2\n\
  \34\n\
  \10\n\
  \3"

type Adapter = Int

nextAdapter :: Int -> S.Set Adapter -> Maybe (Adapter, S.Set Adapter)
nextAdapter currentJolt adapters
  | S.member (currentJolt + 1) adapters = Just (currentJolt + 1, S.delete (currentJolt + 1) adapters)
  | S.member (currentJolt + 2) adapters = Just (currentJolt + 2, S.delete (currentJolt + 2) adapters)
  | S.member (currentJolt + 3) adapters = Just (currentJolt + 3, S.delete (currentJolt + 3) adapters)
  | otherwise = Nothing

adapterChain :: S.Set Adapter -> [Adapter] -> Adapter -> [Adapter]
adapterChain adapters chain currentAdapter = if null adapters then chain ++ [currentAdapter + 3] else f
  where
    f = case nextAdapter currentAdapter adapters of
      Just (adapter, adapters') -> adapterChain adapters' (chain ++ [adapter]) adapter
      Nothing -> []

toNumbers :: String -> [Int]
toNumbers s = map (\v -> read v :: Int) $ lines s

solveA :: String -> String
solveA s = show $ ones * threes
  where
    chain = adapterChain (S.fromList $ toNumbers s) [0] 0
    diffs = map abs $ zipWith (-) chain (tail chain)
    ones = length $ filter (== 1) diffs
    threes = length $ filter (== 3) diffs

solveB :: String -> String
solveB s = s
