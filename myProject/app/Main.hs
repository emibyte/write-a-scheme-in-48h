module Main (main) where

import Control.Monad (liftM)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!#$%%|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\""
  return x

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\"\\")
  -- x <- many $ many1 (noneOf "\"\\") <|> escapedChars
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- exercise 1.1
parseNumber' :: Parser LispVal
parseNumber' = do
  digits <- many1 digit
  (return . Number . read) digits

-- exercise 1.2
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
