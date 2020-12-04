module Day4 where

import Data.Char (isDigit, isHexDigit)
import qualified Data.HashMap.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Text.Read (readMaybe)
import Util (splitOn)

type Passport = M.HashMap String String

data EyeColor = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Show)

data ValidPassport = ValidPassport
  { byr :: Int,
    iyr :: Int,
    eyr :: Int,
    hgt :: Int,
    hcl :: String,
    ecl :: EyeColor,
    pid :: String
  }

parseKeyValue :: String -> Maybe (String, String)
parseKeyValue v = case splitOn ":" v of
  [k, v] -> Just (k, v)
  _ -> Nothing

parsePassports :: String -> [Passport]
parsePassports input = raw
  where
    raw = map (M.fromList . (mapMaybe parseKeyValue . words)) (splitOn "\n\n" input)

parseNumber :: Int -> Int -> String -> Maybe Int
parseNumber min max s = case (readMaybe s :: Maybe Int) of
  Just x -> if x >= min && x <= max then Just x else Nothing
  Nothing -> Nothing

parseHeight :: String -> Maybe Int
parseHeight [a, b, c, 'c', 'm'] = parseNumber 150 193 [a, b, c]
parseHeight [a, b, 'i', 'n'] = parseNumber 59 76 [a, b]
parseHeight _ = Nothing

parseHex :: String -> Maybe String
parseHex s = if all ((== True) . isHexDigit) s then Just s else Nothing

parseColor :: String -> Maybe String
parseColor ['#', a, b, c, d, e, f] = parseHex [a, b, c, d, e, f]
parseColor _ = Nothing

parseEyeColor :: String -> Maybe EyeColor
parseEyeColor "amb" = Just AMB
parseEyeColor "blu" = Just BLU
parseEyeColor "brn" = Just BRN
parseEyeColor "gry" = Just GRY
parseEyeColor "grn" = Just GRN
parseEyeColor "hzl" = Just HZL
parseEyeColor "oth" = Just OTH
parseEyeColor _ = Nothing

parsePassportId :: String -> Maybe String
parsePassportId s = if length s == 9 && all ((== True) . isDigit) s then Just s else Nothing

isValidA :: Passport -> Bool
isValidA passport = all ((== True) . (`elem` foundKeys)) requiredKeys
  where
    requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    foundKeys = S.fromList $ M.keys passport

parseValidPassport :: Passport -> Maybe ValidPassport
parseValidPassport s = do
  byr <- parseNumber 1920 2002 =<< M.lookup "byr" s
  iyr <- parseNumber 2010 2020 =<< M.lookup "iyr" s
  eyr <- parseNumber 2020 2030 =<< M.lookup "eyr" s
  hgt <- parseHeight =<< M.lookup "hgt" s
  hcl <- parseColor =<< M.lookup "hcl" s
  ecl <- parseEyeColor =<< M.lookup "ecl" s
  pid <- parsePassportId =<< M.lookup "pid" s
  Just
    ValidPassport
      { byr = byr,
        iyr = iyr,
        eyr = eyr,
        hgt = hgt,
        hcl = hcl,
        ecl = ecl,
        pid = pid
      }

solveA :: String -> String
solveA s = show $ length $ filter (== True) $ map isValidA $ parsePassports s

solveB :: String -> String
solveB s = show $ length $ mapMaybe parseValidPassport (parsePassports s)
