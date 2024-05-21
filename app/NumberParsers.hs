module NumberParsers where

import Data.Complex
import Data.Ratio

data LispNum = Complex (Complex Double)
             | Real Double
             | Rational Integer
             | Integer Integer

parseNum :: Parser LispNum
parseNum = 
  do radix <- parseRadix
     let ops = [ parseComplex
               , parseReal
               , parseRational
               , parseInteger ]
     choice (ops <*> [radix])

parseComplex :: Parser LispNum

{-
module StringParsers where

import Text.ParserCombinators.Parsec hiding (spaces, hexDigit)
import Data.Ratio
import Complex

parseUInteger :: Parser Integer
parseUInteger =  parseBin
             <|> parseOct
             <|> parseHex
             <|> parseDec

parseURational :: Parser Rational
parseURational =
  do num <- parseUInteger
     char '/'
     den <- parseUInteger
     return num % den

parseUReal :: Parser Double
parseUReal = 
  do string "#d" <|> return ()
     pre  <- many digit
     char '.'
     post <- many digit
     case (pre ++ '.' ++ post) of
       dec@(x:y:_) -> read dec
       _ -> fail "'.' is not a valid Float."

parseComplex :: Parser (Complex Double)
parseComplex = 
  do rcmp <- parseRational <|> parseReal <|> parseInteger
     isgn <- oneOf "+-"
     icmp <- parseURational <|> parseUReal <|> parseUInteger
     char 'i'
     -- not done
-}
