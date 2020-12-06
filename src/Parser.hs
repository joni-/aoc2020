module Parser where

import Data.Char (isDigit)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser ff
    where
      ff input = do
        (match, input') <- p input
        Just (f match, input')

instance Applicative Parser where
  pure x = Parser (\input -> Just (x, input))
  (<*>) (Parser p1) (Parser p2) = Parser ff
    where
      ff input = do
        (f, input') <- p1 input
        (match, input'') <- p2 input'
        Just (f match, input'')

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x : xs) = if x == c then Just (c, xs) else Nothing
    f _ = Nothing

stringP :: String -> Parser String
stringP = traverse charP

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser f
  where
    f input = do
      (match, input') <- p input
      if null match then Nothing else Just (match, input')

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser f
  where
    f input = Just (span pred input)

intP :: Parser Int
intP = fmap read (notNull (spanP isDigit))

parseString :: String -> String -> Maybe (String, String)
parseString target = runParser $ stringP target

between :: Char -> Char -> String -> Maybe (String, String)
between start end = runParser $ charP start *> spanP (/= end) <* charP end

until :: Char -> String -> Maybe (String, String)
until target = runParser $ spanP (/= target) <* charP target
