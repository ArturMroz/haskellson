module Main where

import Data.Map

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Integer -- TODO: support floats
    | JsonString String
    | JsonArray [JsonValue]
    -- | JsonObject [(String, JsonValue)]
    | JsonObject (Map String JsonValue) 
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

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined