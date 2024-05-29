module Primitives (primitives) where

import LispValParsers
import NumberParsers

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

lispAddition :: [LispVal] -> LispVal
lispAddition args
  | any isComplex args = complexBinop (+) args
  | any isReal args = realBinop (+) args
  | any isRational args = rationalBinop (+) args
  | otherwise = integerBinop (+) args

lispSubtraction :: [LispVal] -> LispVal
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

lispMultiplication :: [LispVal] -> LispVal
lispMultiplication args
  | any isComplex args = complexBinop (*) args
  | any isReal args = realBinop (*) args
  | any isRational args = rationalBinop (*) args
  | otherwise = integerBinop (*) args

lispDivision :: [LispVal] -> LispVal
lispDivision [x]
  | isComplex x = lispDivision [Number (Complex 1), x]
  | isReal x = lispDivision [Number (Real 1), x]
  | isRational x = lispDivision [Number (Rational 1), x]
lispDivision args
  | any isComplex args = complexBinop (/) args
  | any isReal args = realBinop (/) args
  | any isRational args = rationalBinop (/) args
  | otherwise = lispDivision (castRational <$> args)

lispMod :: [LispVal] -> LispVal
lispMod args
  | all isInteger args = integerBinop mod args
  | otherwise = error "type error: mod takes only integers"

lispQuotient :: [LispVal] -> LispVal
lispQuotient args
  | all isInteger args = integerBinop quot args
  | otherwise = error "type error: quotient takes only integers"

lispRemainder :: [LispVal] -> LispVal
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

castComplex :: LispVal -> LispVal
castComplex (Number n) =
  Number
    $ case n of
        Complex c -> Complex c
        Real r -> Complex $ r :+ 0
        Rational r -> Complex $ fromRational r :+ 0
        Integer x -> Complex $ fromInteger x :+ 0
castComplex _ = error "only number types can be cast into complex"

castReal :: LispVal -> LispVal
castReal (Number n) =
  Number
    $ case n of
        Complex _ -> error "Complex numbers are not Real"
        Real r -> Real r
        Rational r -> Real $ fromRational r
        Integer x -> Real $ fromInteger x
castReal _ =
  error "only number types lower than or equal to real can be cast into real"

castRational :: LispVal -> LispVal
castRational (Number n) =
  Number
    $ case n of
        Complex _ -> error "Complex numbers are not Rational"
        Real _ -> error "Real numbers are not Rational"
        Rational r -> Rational r
        Integer x -> Rational $ x % 1
castRational _ =
  error "only number types lower than or equal to rational can be cast into rational"

-- TODO
complexBinop ::
     (Complex Double -> Complex Double -> Complex Double) -> [LispVal] -> ThrowsError LispVal
complexBinop op args =
  Number
    $ let unwrap arg = unwrappedArg
            where
              Number (Complex unwrappedArg) = castComplex arg
       in Complex $ foldl1 op (map unwrap args)

-- TODO #1
realBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
realBinop op args =
  Number
    $ let unwrap arg = unwrappedArg
            where
              Number (Real unwrappedArg) = castReal arg
       in Real $ foldl1 op (map unwrap args)

-- TODO
rationalBinop :: (Rational -> Rational -> Rational) -> [LispVal] -> ThrowsError LispVal
rationalBinop op args =
  Number
    $ let unwrap arg = unwrappedArg
            where
              Number (Rational unwrappedArg) = castRational arg
       in Rational $ foldl1 op (map unwrap args)

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop op args =
  Number
    $ let unwrap (Number (Integer arg)) = arg
       in Integer $ foldl1 op (map unwrap args)

questionUnop :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
questionUnop op [arg] = return . Bool $ op arg
questionUnop _ _ = error "invalid number of arguments"

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom a] = return (String a)
symbolToString _ = error "symbol->string takes one symbol"

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return (Atom s)
stringToSymbol _ = error "string->symbol takes one string"