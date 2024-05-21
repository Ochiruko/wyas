module NumberParsers where

import Data.Complex
import Data.Ratio

{-# LANGUAGE DeriveFunctor #-}

data LispNum = Complex (Complex Double)
             | Real Double
             | Rational Integer
             | Integer Integer
             deriving (Eq, Show, Functor)

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
     -- parse a+bi
     do rcmp <- parseReal r <|> parseRational r <|> parseInteger r
        sgn  <- oneOf "+-"
        imcp <- parseUReal r <|> parseURational r <|> parseUInteger r
        char 'i'
        return . Complex $ case sgn of
          '+' -> (toDouble <$> rcmp) :+ (toDouble <$> imcp)
          '-' -> (toDouble <$> rcmp) :+ (negate <$> (toDouble <$> rcmp))
     -- parse [+|-|empty]bi
 <|> do icmp <- parseReal r <|> parseRational r <|> parseInteger r
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
     dec <- parseUReal 10
     return . Real $ case sgn of
       '+' -> dec
       '-' -> negate <$> dec
       _ -> fail "parsing error: sign not parsed properly"

parseUReal :: Integer -> Parser LispNum
parseUReal r = 
     do 