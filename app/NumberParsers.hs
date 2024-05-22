module NumberParsers where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Complex
import Data.Ratio
import Data.Char (isDigit)

data LispNum = Complex (Complex Double)
             | Real Double
             | Rational Rational
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

parseRadix :: Parser Integer
parseRadix = try parseVerboseRadix <|> return 10
  where parseVerboseRadix =
          do char '#'
             r <- oneOf "bodx"
             return $ case r of
               'b' -> 2
               'o' -> 8
               'd' -> 10
               'x' -> 16
               _   -> fail "parsing error: invalid radix"
     

-- "a+bi" | "bi" ==> a :+ b
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
  Rational rat -> numerator rat / denominator rat
  Integer n -> fromInteger n

parseReal :: Integer -> Parser LispNum
parseReal r = 
  do sgn <- oneOf "+-" <|> return '+'
     (Real num) <- parseUReal 10
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
     return . Real $ case (length num, r) of
       (1, _)  -> fail "parsing error: '.' is an invalid UReal"
       (_, 10) -> num
       (_, _)  -> fail "parsing error: decimal radices can only be 10"

parseRational  :: Integer -> Parser LispNum
parseRational r =
  do sgn <- oneOf "+-" <|> return '+'
     (Rational rat) <- parseURational r
     return . Rational $ case sgn of
       '+' -> rat
       '-' -> negate rat     

parseURational :: Integer -> Parser LispNum
parseURational r =
  do num <- parseUInteger r
     char '/'
     den <- parseUInteger r
     return . Rational $ num % den

parseInteger :: Integer -> Parser LispNum
parseInteger r =
  do sgn <- oneOf "+-" <|> return '+'
     (Integer int) <- parseUInteger r
     return . Integer $ case sgn of
       '+' -> int
       '-' -> negate int

parseUInteger :: Integer -> Parser LispNum
parseUInteger r = case r of
  2  -> parseBinary
  8  -> parseOctal
  10 -> parseDecimal
  16 -> parseHexadecimal
  _  -> fail "parsing error: invalid radix"

parseBinary :: Parser LispNum
parseBinary = 
  do bin <- many $ oneOf "01"
     return . Integer . foldl f 0 $ bin
     where f acc bit = acc * 2 + read bit

parseOctal :: Parser LispNum
parseOctal =
  do oct <- many $ oneOf "01234567"
     return . Integer . foldl f 0 $ oct
     where f acc octit = acc * 8 + read octit

parseDecimal :: Parser LispNum
parseDecimal =
  do dec <- many digit
     return . Integer . read $ dec

parseHexadecimal :: Parser LispNum
parseHexadecimal = 
  do hex <- many $ digit <|> oneOf "abcdef"
     return . Integer . foldl f 0 $ hex
     where f acc hexit = acc * 16 + readHexit hexit
           readHexit h = case h of
             d | isDigit d -> read d
             l -> toEnum l - toEnum 'a' + 1