module LispValParsers
  ( module Data.Array
  , module NumberParsers
  , module LispValParsers
  , module Text.Parsec
  ) where

import Control.Monad
import Data.Array
import Text.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec (Parser(..))

import GeneralParsers
import NumberParsers

-- LispVal's parsers are all left factored *except* for those that employ LispNum parsers,
-- because NumberParsers is full of try expressions.
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Array Integer LispVal)
  | Number LispNum
  | String String
  | Char Char
  | Bool Bool
  deriving (Eq)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal v =
  case v of
    String contents -> "\"" ++ contents ++ "\""
    Char ' ' -> "#\\space"
    Char '\n' -> "#\\newline"
    Char c -> "#\\" ++ show c
    Atom name -> name
    Number contents -> show contents
    Bool True -> "#t"
    Bool False -> "#f"
    List contents -> "(" ++ unwordsList contents ++ ")"
    DottedList head tail -> "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
    Vector contents -> "#" ++ show (List $ elems contents)
  where
    unwordsList = unwords . map showVal

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseChar
    <|> parseNumber
    <|> parseQuoted
    <|> parseList
    <|> parseVector

-- | Parses numbers according to the R5RS specification.
parseNumber :: Parser LispVal
parseNumber = do
  num <- parseLispNum
  return . Number $ num

-- | Parses strings "string", permitting escape characters and escaped quotes.
parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many $ noneOf "\\\"" <|> parseEscaped
  char '"'
  return . String $ str

-- | parses the first char and then the rest
-- | this left factors the grammar and reduces the worst case complexity
-- | LL(1)
parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  head <- anyChar
  rest <- string "ewline" <|> string "pace" <|> return ""
  return . Char
    $ case rest of
        "pace" -> ' '
        "ewline" -> '\n'
        _ -> head

-- LL(1)
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return
    $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseList :: Parser LispVal
parseList = do
  char '('
  expr <- parseExpr
  (rest, dot) <- parseRest
  return
    $ case dot of
        Just x -> DottedList (expr : rest) x
        Nothing -> List (expr : rest)
  where
    parseRest = do
      char ')'
      return ([], Nothing)
      <|> (spaces >> (parseDottedListEnd <|> parseRecList))
    parseDottedListEnd = do
      char '.'
      spaces
      expr <- parseExpr
      return ([], Just expr)
    parseRecList = do
      expr <- parseExpr
      (rest, dot) <- parseRest
      return (expr : rest, dot)

parseVector :: Parser LispVal
parseVector = do
  char '#'
  List listRep <- parseList
  let vecRep = listArray (0, toInteger $ length listRep - 1) listRep
  return . Vector $ vecRep

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
