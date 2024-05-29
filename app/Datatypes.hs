module Datatypes 
  ( module Text.Parsec
  , module Data.Ratio
  , module Data.Array
  , module Data.Complex
  , Text.ParserCombinators.Parsec.Parser
  , LispVal (..)
  , LispNum (..)
  , LispError (..)
  ) where

import Text.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec (Parser)
import Data.Ratio
import Data.Array
import Data.Complex

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

instance Show LispVal where show = showVal

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

unwordsList = unwords . map showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError x = case x of
  UnboundVar message varname -> message ++ ": " ++ varname
  BadSpecialForm message form -> message ++ ": " ++ show form
  NotFunction message func -> message ++ ": " ++ show func
  NumArgs expected found -> "Expected " ++ show expected 
                            ++ "args; found values " ++ unwordsList found
  TypeMismatch expected found -> "Invalid type: expected " ++ expected
                                 ++ ", found " ++ show found
  Parser parseErr -> "Parse error at " ++ show parseErr

instance Show LispError where show = showError

data LispNum
  = Complex (Complex Double)
  | Real Double
  | Rational Rational
  | Integer Integer
  deriving Eq

instance Show LispNum where show = showNum

showNum :: LispNum -> String
showNum n = case n of
  Complex (a :+ b) -> "Complex " ++ "(" ++ show a ++ "+" ++ show b ++ "i" ++ ")"
  Real a -> "Real " ++ show (a / 10 ^^ fromInteger (floor (logBase 10 a))) 
            ++ "e" ++ show (floor (logBase 10 a))
  Rational a -> "Rational " ++ "(" ++ show (numerator a) ++ "/" 
                ++ show (denominator a) ++ ")"
  Integer a -> "Integer " ++ show a