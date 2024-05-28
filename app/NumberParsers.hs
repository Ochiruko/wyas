module NumberParsers 
  ( LispNum (..)
  , parseLispNum
  , module Data.Complex
  , module Data.Ratio )
  where

import Data.Char (digitToInt, isDigit, isLower)
import Data.Complex
import Data.Ratio
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)

-- prioritizes readability over efficiency, hence the backtracking (try)

data LispNum
  = Complex (Complex Double)
  | Real Double
  | Rational Rational
  | Integer Integer
  deriving (Eq, Show)

parseLispNum :: Parser LispNum
parseLispNum = do
  radix <- parseRadix
  let ops =
        fmap try . (<*> [radix])
          $ [parseComplex, parseReal, parseRational, parseInteger]
  choice ops

parseRadix :: Parser Integer
parseRadix = try parseVerboseRadix <|> return 10
  where
    parseVerboseRadix :: Parser Integer
    parseVerboseRadix = do
      char '#'
      r <- oneOf "bodx"
      return
        $ case r of
            'b' -> 2
            'o' -> 8
            'd' -> 10
            'x' -> 16

parseComplex :: Integer -> Parser LispNum
parseComplex r
 = do
  rcmp <- choice . fmap try $ [parseReal, parseRational, parseInteger] <*> [r]
  sgn <- oneOf "+-"
  icmp <-
    choice . fmap try $ [parseUReal, parseURational, parseUInteger] <*> [r]
  char 'i'
  return . Complex
    $ case sgn of
        '+' -> toDouble rcmp :+ toDouble icmp
        '-' -> toDouble rcmp :+ negate (toDouble rcmp)
 <|> do
  icmp <- choice . fmap try $ [parseReal, parseRational, parseInteger] <*> [r]
  char 'i'
  return . Complex $ 0 :+ toDouble icmp

toDouble :: LispNum -> Double
toDouble x =
  case x of
    Complex (a :+ _) -> a
    Real n -> n
    Rational rat ->
      (fromInteger . numerator) rat / (fromInteger . denominator) rat
    Integer n -> fromInteger n

parseReal :: Integer -> Parser LispNum
parseReal r = do
  sgn <- oneOf "+-" <|> return '+'
  (Real num) <- parseUReal 10
  return . Real
    $ case sgn of
        '+' -> num
        '-' -> negate num

parseUReal :: Integer -> Parser LispNum
parseUReal r = do
  whl <- many digit
  char '.'
  dec <- many digit
  let num :: Double
      num = read $ "0" ++ whl ++ "." ++ dec ++ "0"
  case (length whl + length dec, r) of
    (0, _) -> fail "parsing error: '.' is an invalid UReal"
    (_, 10) -> return . Real $ num
    (_, _) -> fail "parsing error: decimal radices can only be 10"

parseRational :: Integer -> Parser LispNum
parseRational r = do
  sgn <- oneOf "+-" <|> return '+'
  (Rational rat) <- parseURational r
  return . Rational
    $ case sgn of
        '+' -> rat
        '-' -> negate rat

parseURational :: Integer -> Parser LispNum
parseURational r = do
  (Integer num) <- parseUInteger r
  char '/'
  (Integer den) <- parseUInteger r
  return . Rational $ num % den

parseInteger :: Integer -> Parser LispNum
parseInteger r = do
  sgn <- oneOf "+-" <|> return '+'
  (Integer int) <- parseUInteger r
  return . Integer
    $ case sgn of
        '+' -> int
        '-' -> negate int

parseUInteger :: Integer -> Parser LispNum
parseUInteger r =
  case r of
    2 -> parseBinary
    8 -> parseOctal
    10 -> parseDecimal
    16 -> parseHexadecimal
    _ -> fail "parsing error: invalid radix"

parseBinary :: Parser LispNum
parseBinary = do
  bin <- many1 $ oneOf "01"
  return . Integer . foldl f 0 $ bin
  where
    f acc bit = acc * 2 + (toInteger . digitToInt) bit

parseOctal :: Parser LispNum
parseOctal = do
  oct <- many1 $ oneOf "01234567"
  return . Integer . foldl f 0 $ oct
  where
    f acc octit = acc * 8 + (toInteger . digitToInt) octit

parseDecimal :: Parser LispNum
parseDecimal = do
  dec <- many1 digit
  return . Integer . read $ dec

parseHexadecimal :: Parser LispNum
parseHexadecimal = do
  hex <- many1 $ digit <|> oneOf "abcdef"
  return . Integer . foldl f 0 $ hex
  where
    f acc hexit = acc * 16 + readHexit hexit
    readHexit h =
      case h of
        d
          | isDigit d -> toInteger . digitToInt $ d
        l
          | isLower l -> toInteger $ fromEnum l - fromEnum 'a' + 10
