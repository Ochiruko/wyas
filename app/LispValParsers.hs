module LispValParsers where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import GeneralParsers
import NumberParsers

-- LispVal's parsers are all left factored *except* for those that employ LispNum parsers,
-- because NumberParsers is full of try expressions.

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | String String
             | Char Char
             | Character Char
             | Bool Bool
             deriving (Eq, Show)

parseString :: Parser LispVal
parseString =
  do char '"'
     str <- many $ noneOf "\\\"" <|> parseEscaped
     char '"'
     return . String $ str

-- parses the first char and then the rest
-- this left factors the grammar and reduces the worst case complexity
parseChar :: Parser LispVal
parseChar = 
  do string "#\\"
     head <- anyChar
     rest <- string "ewline" <|> string "pace" <|> return ""
     return . Char $ case rest of
       "pace" -> ' '
       "ewline" -> '\n'
       _ -> head

parseAtom :: Parser LispVal
parseAtom =
  do first <- letter <|> symbol
     rest  <- many (letter <|> digit <|> symbol)
     let atom = first:rest
     return $ case atom of
       "#t" -> Bool True
       "#f" -> Bool False
       _    -> Atom atom

parseList :: Parser LispVal
parseList =
  do shared  <- sepBy parseExpr spaces
     dotExpr <- do { spaces; char '.'; spaces; x <- parseExpr; return (Just x) }
            <|> return Nothing
     return $ case dotExpr of
       Nothing -> List shared
       Just x -> DottedList shared x

-- S = ' '+, E = expr, L = list
-- L ==> [E S]* E

-- parseList :: Parser LispVal
-- parseList = liftM List $ sepBy parseExpr spaces

-- S = ' '+, E = expr, DL = dotted list
-- DL => E S . S E
-- parseDottedList :: Parser LispVal
-- parseDottedList =
--   do head <- endBy parseExpr spaces
--     tail <- char '.' >> spaces >> parseExpr
--     return $ DottedList head tail

-- turns 'x -> (quote x)
parseQuoted :: Parser LispVal
parseQuoted =
  do char '\''
     x <- parseExpr
     return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseExpr 
         <|> do char '('
                x <- parseList
                char ')'
                return x
