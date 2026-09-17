module Main (main) where

import Control.Monad (liftM)
import Data.Array
import Data.Complex
import Data.Ratio
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Array Int LispVal)
  | Number Integer
  | Float Double
  | Rational Rational
  | Complex (Complex Double)
  | String String
  | Character Char
  | Bool Bool
  deriving (Show)

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '"' -> x
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True -- TODO(emi): can't happen anymore
    "#f" -> Bool False -- TODO(emi): can't happen anymore
    _ -> Atom atom

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseDecimalWithTag <|> parseBinary <|> parseOctal <|> parseHexadecimal

parseDecimal :: Parser LispVal
parseDecimal = parseNumber'

parseDecimalWithTag :: Parser LispVal
parseDecimalWithTag = try $ char '#' >> char 'd' >> parseDecimal

parseBinary :: Parser LispVal
parseBinary = do
  try $ string "#b"
  x <- many1 $ oneOf "10"
  return $ Number (bin2dig x)

bin2dig :: [Char] -> Integer
bin2dig =
  let digitToInt x = if x == '0' then 0 else 1
   in foldl' (\acc x -> acc * 2 + digitToInt x) 0

parseOctal :: Parser LispVal
parseOctal = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dig x)

oct2dig :: [Char] -> Integer
oct2dig x = fst (readOct x !! 0)

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dig x)

hex2dig :: [Char] -> Integer
hex2dig x = fst (readHex x !! 0)

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space") <|> do x <- anyChar; notFollowedBy alphaNum; return [x]
  return $ case value of
    "space" -> Character ' '
    "newline" -> Character '\n'
    otherwise -> Character (value !! 0)

-- exercise 1.1
parseNumber' :: Parser LispVal
parseNumber' = do
  digits <- many1 digit
  (return . Number . read) digits

-- exercise 1.2
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . Number . read

-- TODO(emi): #e and #i for exactness
parseFloat :: Parser LispVal
parseFloat = do
  first <- many1 digit
  char '.'
  after <- many1 digit
  let (parsed, _) = (readFloat $ first ++ "." ++ after) !! 0
  return $ Float parsed

parseRational :: Parser LispVal
parseRational = do
  numerator <- many1 digit
  char '/'
  denominator <- many1 digit
  return $ Rational $ (read numerator) % (read denominator)

parseComplex :: Parser LispVal
parseComplex = do
  real <- (try parseFloat <|> parseDecimal)
  char '+'
  imaginary <- (try parseFloat <|> parseDecimal)
  char 'i'
  return $ Complex (toDouble real :+ toDouble imaginary)

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  string ",@"
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do
  values <- sepBy parseExpr spaces
  return $ Vector $ listArray (0, length values - 1) values

-- TODO(emi): group the numerical stuff into a separate function so it doesnt clog this one so much
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseRational
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> parseQuasiquoted
    <|> parseUnquoteSplicing
    <|> parseUnquote
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x
    <|> do
      char '#'
      char '('
      x <- try parseVector
      char ')'
      return x
