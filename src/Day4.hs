module Day4 where

import Data.Char (isDigit, isHexDigit)
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Read (readMaybe)

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

splitOn :: String -> String -> [String]
splitOn c s = map T.unpack $ T.splitOn (T.pack c) (T.pack s)

parseKeyValue :: String -> Maybe (String, String)
parseKeyValue v = case splitOn ":" v of
  [k, v] -> Just (k, v)
  _ -> Nothing

parsePassports :: String -> [Passport]
parsePassports input = raw
  where
    raw = map M.fromList $ map (\v -> catMaybes $ map parseKeyValue $ words v) $ splitOn "\n\n" input

parseNumber :: Int -> Int -> String -> Maybe Int
parseNumber min max s = case (readMaybe s :: Maybe Int) of
  Just x -> if x >= min && x <= max then Just x else Nothing
  Nothing -> Nothing

parseHeight :: String -> Maybe Int
parseHeight [a, b, c, 'c', 'm'] = parseNumber 150 193 [a, b, c]
parseHeight [a, b, 'i', 'n'] = parseNumber 59 76 [a, b]
parseHeight _ = Nothing

parseHex :: String -> Maybe String
parseHex s = if all (== True) $ map isHexDigit s then Just s else Nothing

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
parsePassportId s = if length s == 9 && all (== True) (map isDigit s) then Just s else Nothing

isValidA :: Passport -> Bool
isValidA passport = all (== True) $ map (\k -> k `elem` foundKeys) requiredKeys
  where
    requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    foundKeys = S.fromList $ M.keys passport

parseValidPassport :: Passport -> Maybe ValidPassport
parseValidPassport s = result
  where
    byr = fromMaybe Nothing $ parseNumber 1920 2002 <$> M.lookup "byr" s
    iyr = fromMaybe Nothing $ parseNumber 2010 2020 <$> M.lookup "iyr" s
    eyr = fromMaybe Nothing $ parseNumber 2020 2030 <$> M.lookup "eyr" s
    hgt = fromMaybe Nothing $ parseHeight <$> M.lookup "hgt" s
    hcl = fromMaybe Nothing $ parseColor <$> M.lookup "hcl" s
    ecl = fromMaybe Nothing $ parseEyeColor <$> M.lookup "ecl" s
    pid = fromMaybe Nothing $ parsePassportId <$> M.lookup "pid" s
    result = case byr of
      Just byr' -> case iyr of
        Just iyr' -> case eyr of
          Just eyr' -> case hgt of
            Just hgt' -> case hcl of
              Just hcl' -> case ecl of
                Just ecl' -> case pid of
                  Just pid' ->
                    Just
                      ValidPassport
                        { byr = byr',
                          iyr = iyr',
                          eyr = eyr',
                          hgt = hgt',
                          hcl = hcl',
                          ecl = ecl',
                          pid = pid'
                        }
                  Nothing -> Nothing
                Nothing -> Nothing
              Nothing -> Nothing
            Nothing -> Nothing
          Nothing -> Nothing
        Nothing -> Nothing
      Nothing -> Nothing

solveA :: String -> String
solveA s = show $ length $ filter (== True) $ map isValidA $ parsePassports s

solveB :: String -> String
solveB s = show $ length $ catMaybes $ map parseValidPassport $ parsePassports s
