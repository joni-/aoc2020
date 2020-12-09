module Util where

import Data.Maybe (fromJust)
import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn c s = map T.unpack $ T.splitOn (T.pack c) (T.pack s)

trim :: String -> String
trim = T.unpack . T.strip . T.pack

showJust :: Show a => Maybe a -> String
showJust = show . fromJust
