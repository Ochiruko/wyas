module Primitives (primitives) where

import LispValParsers
import NumberParsers
import ErrorHandling

import Data.Functor

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", lispAddition)
  , ("-", lispSubtraction)
  , ("*", lispMultiplication)
  , ("/", lispDivision)
  , ("mod", lispMod)
  , ("quotient", lispQuotient)
  , ("remainder", lispRemainder)
  , ("number?", questionUnop (anyP [isComplex,isReal,isRational,isInteger]))
  , ("complex?", questionUnop (anyP [isComplex,isReal,isRational,isInteger]))
  , ("real?", questionUnop (anyP [isReal,isRational,isInteger]))
  , ("rational?", questionUnop (anyP [isRational,isInteger]))
  , ("integer?", questionUnop isInteger)
  , ("bool?", questionUnop isBool)
  , ("char?", questionUnop isChar)
  , ("string?", questionUnop isString)
  , ("symbol?", questionUnop isSymbol)
  , ("list?", questionUnop isList)
  , ("vector?", questionUnop isVector)
  , ("symbol->string", symbolToString)
  , ("string->symbol", stringToSymbol)
  ]

anyP :: [a -> Bool] -> a -> Bool
anyP ps = or . (ps <*>) . (:[])

allP :: [a -> Bool] -> a -> Bool
allP ps = and . (ps <*>) . (:[])

lispAddition :: [LispVal] -> ThrowsError LispVal
lispAddition args
  | any isComplex args = complexBinop (+) args
  | any isReal args = realBinop (+) args
  | any isRational args = rationalBinop (+) args
  | otherwise = integerBinop (+) args

lispSubtraction :: [LispVal] -> ThrowsError LispVal
lispSubtraction [x]
  | isComplex x = lispSubtraction [Number (Complex 0), x]
  | isReal x = lispSubtraction [Number (Real 0), x]
  | isRational x = lispSubtraction [Number (Rational 0), x]
  | isInteger x = lispSubtraction [Number (Integer 0), x]
lispSubtraction args
  | any isComplex args = complexBinop (-) args
  | any isReal args = realBinop (-) args
  | any isRational args = rationalBinop (-) args
  | otherwise = integerBinop (-) args

lispMultiplication :: [LispVal] -> ThrowsError LispVal
lispMultiplication args
  | any isComplex args = complexBinop (*) args
  | any isReal args = realBinop (*) args
  | any isRational args = rationalBinop (*) args
  | otherwise = integerBinop (*) args

lispDivision :: [LispVal] -> ThrowsError LispVal
lispDivision [x]
  | isComplex x = lispDivision [Number (Complex 1), x]
  | isReal x = lispDivision [Number (Real 1), x]
  | isRational x = lispDivision [Number (Rational 1), x]
lispDivision [] = throwError $ NumArgs 1 []
lispDivision args
  | any isComplex args = complexBinop (/) args
  | any isReal args = realBinop (/) args
  | any isRational args = rationalBinop (/) args
  | any isInteger args = mapM castRational args >>= lispDivision
  | otherwise = throwError $ TypeMismatch "Number" (head args)

lispMod :: [LispVal] -> ThrowsError LispVal
lispMod args
  | all isInteger args = integerBinop mod args
  | otherwise = error "type error: mod takes only integers"

lispQuotient :: [LispVal] -> ThrowsError LispVal
lispQuotient args
  | all isInteger args = integerBinop quot args
  | otherwise = error "type error: quotient takes only integers"

lispRemainder :: [LispVal] -> ThrowsError LispVal
lispRemainder args
  | all isInteger args = integerBinop rem args
  | otherwise = error "type error: remainder takes only integers"

isComplex :: LispVal -> Bool
isComplex (Number (Complex _)) = True
isComplex _ = False

isReal :: LispVal -> Bool
isReal (Number (Real _)) = False
isReal _ = False

isRational :: LispVal -> Bool
isRational (Number (Rational _)) = True
isRational _ = False

isInteger :: LispVal -> Bool
isInteger (Number (Integer _)) = True
isInteger _ = False

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _ = False

isChar :: LispVal -> Bool
isChar (Char _) = True
isChar _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _ = False

isList :: LispVal -> Bool
isList (List _) = True
isList (DottedList _ _) = undefined
isList _ = False

isVector :: LispVal -> Bool
isVector (Vector _) = True
isVector _ = False

castComplex :: LispVal -> ThrowsError LispVal
castComplex (Number n) =
  return . Number
    $ case n of
        Complex c -> Complex c
        Real r -> Complex $ r :+ 0
        Rational r -> Complex $ fromRational r :+ 0
        Integer x -> Complex $ fromInteger x :+ 0
castComplex x = throwError $ TypeMismatch "Number" x

castReal :: LispVal -> ThrowsError LispVal
castReal (Number n) =
  case n of
    Complex x -> throwError $ TypeMismatch "(Real | Rational | Integer)" (Number n)
    Real r -> return . Number . Real $ r
    Rational r -> return . Number . Real $ fromRational r
    Integer x -> return . Number . Real $ fromInteger x
castReal x = throwError $ TypeMismatch "(Real | Rational | Integer)" x

castRational :: LispVal -> ThrowsError LispVal
castRational (Number n) =
  case n of
    Rational r -> return . Number . Rational $ r
    Integer x -> return .Number . Rational $ x % 1
    x -> throwError $ TypeMismatch "(Rational | Integer)" (Number x)
castRational x = throwError $ TypeMismatch "(Rational | Integer)" x

complexBinop :: (Complex Double -> Complex Double -> Complex Double) 
             -> [LispVal] 
             -> ThrowsError LispVal
complexBinop op args =
  let unwrap arg = castComplex arg >>= \(Number (Complex x)) -> return x
   in Number . Complex . foldl1 op <$> mapM unwrap args

realBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
realBinop op args =
  let unwrap arg = castReal arg >>= \(Number (Real x)) -> return x
   in Number . Real . foldl1 op <$> mapM unwrap args

rationalBinop :: (Rational -> Rational -> Rational) -> [LispVal] -> ThrowsError LispVal
rationalBinop op args =
  let unwrap arg = castRational arg >>= \(Number (Rational x)) -> return x
   in Number . Rational . foldl1 op <$> mapM unwrap args

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop op args =
  let unwrap :: LispVal -> ThrowsError Integer
      unwrap (Number (Integer arg)) = return arg
      unwrap x = throwError $ TypeMismatch "Integer" x
   in Number . Integer . foldl1 op <$> mapM unwrap args

questionUnop :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
questionUnop op [arg] = return . Bool $ op arg
questionUnop _ xs = throwError $ NumArgs 1 xs

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom a] = return $ String a
symbolToString [x] = throwError $ TypeMismatch "Symbol" x
symbolToString xs = throwError $ NumArgs 1 xs

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return (Atom s)
stringToSymbol [x] = throwError $ TypeMismatch "String" x
stringToSymbol xs = throwError $ NumArgs 1 xs