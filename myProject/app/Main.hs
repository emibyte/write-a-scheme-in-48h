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

main :: IO ()
main = do
  (expr : _) <- getArgs
  (print . eval . readExpr) expr

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

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

parseAnyList :: Parser LispVal
parseAnyList = do
  char '('
  first <- sepEndBy parseExpr spaces
  maybeSecond <- optionMaybe (char '.' >> spaces >> parseExpr)
  char ')'
  return $ case maybeSecond of
    Just second -> DottedList first second
    Nothing -> List first

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
    <|> parseAnyList
    <|> do
      char '#'
      char '('
      x <- try parseVector
      char ')'
      return x

-- Evaluation Part 1 Chapter:
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Rational contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Rational _) = val
eval val@(Complex _) = val
eval (List [Atom "quote", val]) = val -- NOTE(emi): quote -> dont eval
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("string?", unaryOp stringp),
    ("symbol?", unaryOp symbolp),
    ("number?", unaryOp numberp),
    ("complex?", unaryOp complexp),
    ("real?", unaryOp realp),
    ("rational?", unaryOp rationalp),
    ("integer?", unaryOp integerp),
    -- ("exact?", _),     -- TODO(emi): havent implemented exactness
    -- ("inexact?", _),   -- TODO(emi): havent implemented exactness
    ("boolean?", unaryOp booleanp),
    ("pair?", unaryOp pairp), -- TODO(emi): is this correct? i think so tbh, but its also still kinda buggy mmh
    ("list?", unaryOp listp),
    -- ("procedure?", _), -- TODO(emi): we do not have procedures yet
    ("vector?", unaryOp vectorp),
    ("string->symbol", unaryOp stringToSymbol),
    ("symbol->string", unaryOp symbolToString)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- NOTE(emi): kinda whacky but since we defined primitives with [LispVal] -> LispVal we kinda have to do it like this
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op [param] = op param
unaryOp op (param : _) = op param

stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False

symbolp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False

booleanp :: LispVal -> LispVal
booleanp (Bool _) = Bool True
booleanp _ = Bool False

vectorp :: LispVal -> LispVal
vectorp (Vector _) = Bool True
vectorp _ = Bool False

listp :: LispVal -> LispVal
listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp (Complex _) = Bool True
numberp (Float _) = Bool True
numberp (Rational _) = Bool True
numberp _ = Bool False

complexp :: LispVal -> LispVal
complexp (Complex _) = Bool True
complexp _ = Bool False

realp :: LispVal -> LispVal
realp (Float _) = Bool True
realp _ = Bool False

rationalp :: LispVal -> LispVal
rationalp (Rational _) = Bool True
rationalp _ = Bool False

integerp :: LispVal -> LispVal
integerp (Number _) = Bool True
integerp _ = Bool False

pairp :: LispVal -> LispVal
pairp (List (_ : _)) = Bool True
pairp (List []) = Bool False
pairp (DottedList _ _) = Bool True
pairp _ = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
-- unpackNum (String s) =
--   let parsed = reads s :: [(Integer, String)]
--    in if null parsed
--         then 0
--         else fst $ parsed !! 0
-- unpackNum (List [n]) = unpackNum n
-- unpackNum _ = 0

-- TODO(emi): what do when non good input?
symbolToString :: LispVal -> LispVal
symbolToString (Atom name) = String name
symbolToString _ = String ""

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s
stringToSymbol _ = Atom ""
