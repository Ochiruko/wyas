module NumberParsers where

import Data.Complex
import Data.Ratio

{-# LANGUAGE DeriveFunctor #-}

data LispNum = Complex (Complex Double)
             | Real Double
             | Rational Integer
             | Integer Integer
             deriving (Eq, Show)

parseNum :: Parser LispNum
parseNum = 
  do radix <- parseRadix
     let ops = fmap try [ parseComplex
                        , parseReal
                        , parseRational
                        , parseInteger ]
     choice (ops <*> [radix])

parseComplex :: Integer -> Parser LispNum
parseComplex r =
     -- parse a+bi
     do rcmp <- choice . fmap try $ [parseReal, parseRational, parseInteger] <*> [r]
        sgn  <- oneOf "+-"
        icmp <- choice . fmap try $ [parseUReal, parseURational, parseUInteger] <*> [r]
        char 'i'
        return . Complex $ case sgn of
          '+' -> toDouble rcmp :+ toDouble imcp
          '-' -> toDouble rcmp :+ negate (toDouble rcmp)
     -- parse [+|-|empty]bi
 <|> do icmp <- choice . fmap try $ [parseReal, parseRational, parseInteger] <*> [r]
        char 'i'
        return . Complex $ icmp

toDouble :: LispNum -> Double
toDouble x = case x of
  Complex (a:+_) -> a
  Real n -> n
  Rational (n%d) -> n/d
  Integer n -> fromInteger n

parseReal :: Integer -> Parser LispNum
parseReal r = 
  do sgn <- oneOf "+-" <|> return '+'
     unum <- parseUReal 10
     let (Real num) = unum
     return . Real $ case sgn of
       '+' -> num
       '-' -> negate num
       _ -> fail "parsing error: sign not parsed properly"

parseUReal :: Integer -> Parser LispNum
parseUReal r = 
     do whl <- many digit
        char '.'
        dec <- many digit
        let num = read $ whl ++ "." ++ dec
        return . Real $ case (length num) of
          1 -> fail "'.' is an invalid UReal"
          _ -> num

