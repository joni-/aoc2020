module Util where

import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn c s = map T.unpack $ T.splitOn (T.pack c) (T.pack s)
