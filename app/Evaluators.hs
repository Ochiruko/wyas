module Evaluators where

import Data.List (sort)
import Data.Complex
import Data.Ratio
import Text.Parsec

import LispValParsers
import NumberParsers

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

eval :: LispVal -> LispVal
eval val =
  case val of
    String _ -> val
    Number _ -> val
    Bool _ -> val
    (List [Atom "quote", val]) -> val
    List (Atom func:args) -> apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primOps

data LispType
  = LispInt
  | LispRat
  | LispReal
  | LispComplex
  | LispStr
  | LispChar
  | LispBool
  | LispList
  | LispDotted
  deriving (Eq, Ord, Show)

lispType :: LispVal -> LispType
lispType v =
  case v of
    Number (Integer _) -> LispInt
    Number (Rational _) -> LispRat
    Number (Real _) -> LispReal
    Number (Complex _) -> LispComplex
    String _ -> LispStr
    Char _ -> LispChar
    Bool _ -> LispBool
    List _ -> LispList
    DottedList _ _ -> LispDotted

primOps :: [(String, [LispVal] -> LispVal)]
primOps =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop (/))
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

castComplex :: LispNum -> LispNum
castComplex n =
  case n of
    Complex c -> Complex c
    Real r -> Complex $ r :+ 0
    Rational r -> Complex $ fromRational r :+ 0
    Integer x -> Complex $ fromInteger x :+ 0
    _ -> error "Somehow there are nontower types?"

castReal :: LispNum -> LispNum
castReal n =
  case n of
    Complex _ -> error "Complex numbers are not Real"
    Real r -> Real r
    Rational r -> Real $ fromRational r
    Integer x -> Real $ fromInteger x
    _ -> error "Somehow there are nontower types?"

castRational :: LispNum -> LispNum
castRational n =
  case n of
    Complex _ -> error "Complex numbers are not Rational"
    Real _ -> error "Real numbers are not Rational"
    Rational r -> Rational r
    Integer x -> Rational $ x % 1
    _ -> error "Somehow there are nontower types?"

numericBinop :: Num a => (a -> a -> a) -> [LispVal] -> LispVal
numericBinop op [Number p1, Number p2] = Number $
  case (p1, p2) of
    (Complex n1, _) ->
      let Complex n2 = castComplex p2
       in Complex $ op n1 n2
    (_, Complex n2) ->
      let Complex n1 = castComplex p1
       in Complex $ op n1 n2
    (Real n1, _) ->
      let Real n2 = castReal p2
       in Real $ op n1 n2
    (_, Real n2) ->
      let Real n1 = castReal p1
       in Real $ op n1 n2
    (Rational n1, _) ->
      let Rational n2 = castRational p2
       in Rational $ op n1 n2
    (_, Rational n2) ->
      let Rational n1 = castRational p1
       in Rational $ op n1 n2
    (Integer n1, Integer n2) -> Integer $ op n1 n2
    _ -> error "non numeric arguments passed to numericBinop"