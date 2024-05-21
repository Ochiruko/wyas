module RefactoredMain where

import Text.ParserCombinators.Parsec hiding (spaces, hexDigit)
import System.Environment
import Control.Monad
import StringParsers
import NumberParsers

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | String String
             | Character Char
             | Bool Bool
             deriving (Eq, Show)

data LispNum = Integer Integer
             | Rational Rational
             | Real Double
             | Complex (Complex Double)
             deriving (Eq, Show)

parseNumber :: Parser LispVal
parseNumber =  parseComplex
           <|> parseReal 
           <|> parseRational 
           <|> parseInteger

parseString :: Parser LispVal
parseString =
  do char '"'
     str <- many $ noneOf "\\\"" <|> parseEscaped
     char '"'
     return . String $ str

parseChar :: Parser LispVal
parseChar = 
  do string "#\\"
     value <- string "newline"
          <|> string "space"
          <|> anyChar >>= (x -> return [x])
     return . Char $ case value of
       "space" -> ' '
       "newline" -> '\n'
       _ -> head value

parseAtom :: Parser LispVal
parseAtom =
  do first <- letter <|> symbol
     rest  <- many (letter <|> digit <|> symbol)
     let atom = first:rest
     return $ case atom of
       "#t" -> Bool True
       "#f" -> Bool False
       _    -> Atom atom
