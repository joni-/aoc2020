module Day4 where

import Data.Char (isDigit, isHexDigit)
import qualified Data.HashMap.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Parser (intP, runParser)
import Util (splitOn)

type PassportData = M.HashMap String String

data EyeColor = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Show)

data Passport = Passport
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

parsePassportData :: String -> [PassportData]
parsePassportData input = map (M.fromList . (mapMaybe parseKeyValue . words)) (splitOn "\n\n" input)

parseBoundedInt :: Int -> Int -> String -> Maybe Int
parseBoundedInt min max s = do
  (value, _) <- runParser intP s
  if value >= min && value <= max then Just value else Nothing

parseHeight :: String -> Maybe Int
parseHeight [a, b, c, 'c', 'm'] = parseBoundedInt 150 193 [a, b, c]
parseHeight [a, b, 'i', 'n'] = parseBoundedInt 59 76 [a, b]
parseHeight _ = Nothing

parseHexString :: String -> Maybe String
parseHexString s = if all ((== True) . isHexDigit) s then Just s else Nothing

parseColor :: String -> Maybe String
parseColor ['#', a, b, c, d, e, f] = parseHexString [a, b, c, d, e, f]
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

hasRequiredFields :: PassportData -> Bool
hasRequiredFields passport = all ((== True) . (`elem` foundKeys)) requiredKeys
  where
    requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    foundKeys = S.fromList $ M.keys passport

parsePassport :: PassportData -> Maybe Passport
parsePassport s = do
  byr <- parseBoundedInt 1920 2002 =<< M.lookup "byr" s
  iyr <- parseBoundedInt 2010 2020 =<< M.lookup "iyr" s
  eyr <- parseBoundedInt 2020 2030 =<< M.lookup "eyr" s
  hgt <- parseHeight =<< M.lookup "hgt" s
  hcl <- parseColor =<< M.lookup "hcl" s
  ecl <- parseEyeColor =<< M.lookup "ecl" s
  pid <- parsePassportId =<< M.lookup "pid" s
  Just
    Passport
      { byr = byr,
        iyr = iyr,
        eyr = eyr,
        hgt = hgt,
        hcl = hcl,
        ecl = ecl,
        pid = pid
      }

solveA :: String -> String
solveA s = show $ length $ filter (== True) $ map hasRequiredFields $ parsePassportData s

solveB :: String -> String
solveB s = show $ length $ mapMaybe parsePassport (parsePassportData s)
