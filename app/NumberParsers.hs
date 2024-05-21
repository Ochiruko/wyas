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

parseComplex :: Integer -> Parser LispNum
parseComplex r =
     -- parse a + bi
     do rcmp <- parseReal <|> parseRational <|> parseInteger
        sgn  <- oneOf "+-"
        imcp <- parseUReal <|> parseURational <|> parseUInteger
        char 'i'
        return . Complex $ case sgn of
          '+' -> toDouble rcmp :+ toDouble imcp
          '-' -> toDouble rcmp :+ (-1 * toDouble rcmp)
     -- parse [+|-|empty]bi
 <|> do icmp <- parseReal <|> parseRational <|> parseInteger
        char 'i'
        return . Complex $ icmp

