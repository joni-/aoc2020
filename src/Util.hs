module Util where

import Data.Maybe (fromJust)
import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn c s = map T.unpack $ T.splitOn (T.pack c) (T.pack s)

trim :: String -> String
trim = T.unpack . T.strip . T.pack

replace :: String -> String -> String -> String
replace needle replacement haystack = T.unpack $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)

showJust :: Show a => Maybe a -> String
showJust = show . fromJust
