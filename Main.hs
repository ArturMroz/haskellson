module Main where

import Control.Applicative
import Data.Char (isDigit, isSpace)
import Data.Map (Map)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- TODO: support floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  -- | JsonObject (Map String JsonValue)
  deriving (Show, Eq)

-- TODO: proper error reporting, err msg, line, column
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Control.Applicative.Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input Control.Applicative.<|> p2 input

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" Control.Applicative.<|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    f _ = undefined -- this should never happen

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where
    f ds = JsonNumber $ read ds

-- TODO: support escape characters
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

whitespace :: Parser String
whitespace = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = charP '[' *> whitespace *> elements <* whitespace <* charP ']'
  where
    elements = JsonArray <$> sepBy separator jsonValue
    separator = whitespace *> charP ',' <* whitespace

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$>
  (charP '{' *> whitespace *> sepBy (whitespace *> charP ',' <* whitespace) pair <* whitespace <* charP '}')
  where
    pair =
      (\key _ value -> (key, value)) <$>
      stringLiteral <*>
      (whitespace *> charP ':' *> whitespace) <*>
      jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull Control.Applicative.<|> jsonBool Control.Applicative.<|> jsonNumber Control.Applicative.<|> jsonString

main :: IO ()
main = undefined