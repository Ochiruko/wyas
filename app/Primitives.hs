module Primitives (primitives) where

import LispValParsers
import NumberParsers
import ErrorHandling

import Control.Monad
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
  , ("=", numEq)
  , ("<", lispLT)
  , (">", lispGT)
  , ("/=", numUneq)
  , (">=", lispGTE)
  , ("<=", lispLTE)
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eqv?", eqv)
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

numEq :: [LispVal] -> ThrowsError LispVal
numEq xs | any isComplex xs = complexBoolBinop (==) xs
          | any isReal xs = realBoolBinop (==) xs
          | any isRational xs = rationalBoolBinop (==) xs
          | otherwise = integerBoolBinop (==) xs

numUneq :: [LispVal] -> ThrowsError LispVal
numUneq xs | any isComplex xs = complexBoolBinop (/=) xs
            | any isReal xs = realBoolBinop (/=) xs
            | any isRational xs = rationalBoolBinop (/=) xs
            | otherwise = integerBoolBinop (/=) xs

lispLT :: [LispVal] -> ThrowsError LispVal
lispLT xs | any isComplex xs = complexBoolBinop (\x y -> magnitude x < magnitude y) xs
          | any isReal xs = realBoolBinop (<) xs
          | any isRational xs = rationalBoolBinop (<) xs
          | otherwise = integerBoolBinop (<) xs

lispLTE :: [LispVal] -> ThrowsError LispVal
lispLTE xs | any isComplex xs = complexBoolBinop (\x y -> magnitude x <= magnitude y) xs
           | any isReal xs = realBoolBinop (<=) xs
           | any isRational xs = rationalBoolBinop (<=) xs
           | otherwise = integerBoolBinop (<=) xs

lispGT :: [LispVal] -> ThrowsError LispVal
lispGT xs | any isComplex xs = complexBoolBinop (\x y -> magnitude x > magnitude y) xs
          | any isReal xs = realBoolBinop (>) xs
          | any isRational xs = rationalBoolBinop (>) xs
          | otherwise = integerBoolBinop (>) xs

lispGTE :: [LispVal] -> ThrowsError LispVal
lispGTE xs | any isComplex xs = complexBoolBinop (\x y -> magnitude x >= magnitude y) xs
           | any isReal xs = realBoolBinop (>=) xs
           | any isRational xs = rationalBoolBinop (>=) xs
           | otherwise = integerBoolBinop (>=) xs

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
  let unwrap arg = castComplex arg >>= unpackComplex
   in Number . Complex . foldl1 op <$> mapM unwrap args

realBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
realBinop op args =
  let unwrap arg = castReal arg >>= unpackReal
   in Number . Real . foldl1 op <$> mapM unwrap args

rationalBinop :: (Rational -> Rational -> Rational) -> [LispVal] -> ThrowsError LispVal
rationalBinop op args =
  let unwrap arg = castRational arg >>= unpackRational
   in Number . Rational . foldl1 op <$> mapM unwrap args

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop op args =
  let unwrap :: LispVal -> ThrowsError Integer
      unwrap (Number (Integer arg)) = return arg
      unwrap x = throwError $ TypeMismatch "Integer" x
   in Number . Integer . foldl1 op <$> mapM unwrap args

unpackComplex :: LispVal -> ThrowsError (Complex Double)
unpackComplex (Number (Complex c)) = return c
unpackComplex x = throwError $ TypeMismatch "Complex" x

unpackReal :: LispVal -> ThrowsError Double
unpackReal (Number (Real r)) = return r
unpackReal x = throwError $ TypeMismatch "Real" x

unpackRational :: LispVal -> ThrowsError Rational
unpackRational (Number (Rational r)) = return r
unpackRational x = throwError $ TypeMismatch "Rational" x

unpackInteger :: LispVal -> ThrowsError Integer
unpackInteger (Number (Integer i)) = return i
unpackInteger x = throwError $ TypeMismatch "Integer" x

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool x = throwError $ TypeMismatch "Bool" x

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString x = throwError $ TypeMismatch "String" x

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [a1, a2] = do left <- unpacker a1
                                    right <- unpacker a2
                                    return . Bool $ op left right
boolBinop _ _ xs = throwError $ NumArgs 2 xs

complexBoolBinop :: (Complex Double -> Complex Double -> Bool) -> [LispVal] -> ThrowsError LispVal
complexBoolBinop = boolBinop (castComplex >=> unpackComplex) 

realBoolBinop :: (Double -> Double -> Bool) -> [LispVal] -> ThrowsError LispVal
realBoolBinop = boolBinop (castReal >=> unpackReal)

rationalBoolBinop :: (Rational -> Rational -> Bool) -> [LispVal] -> ThrowsError LispVal
rationalBoolBinop = boolBinop (castRational >=> unpackRational)

integerBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
integerBoolBinop = boolBinop unpackInteger

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackString

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- DottedLists are just improper lists, where the last element is cons-ed to the post-dot part instead of '().
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs d] = return $ DottedList (x:xs) d
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return . Bool $ arg1 == arg2
eqv [arg1@(Number _), arg2@(Number _)] = numEq arg1 arg2
eqv [String arg1, String arg2] = return . Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return . Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = (xs, x) == eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return . Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False -- How the heck would you get this?
                                Right (Bool val) -> val
eqv [_,_] = Return . Bool $ False
eqv badArgList = throwError $ numArgs 2 badArgList