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
     return rc :+ ic
