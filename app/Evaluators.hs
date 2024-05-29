module Evaluators where

import ErrorHandling
import LispValParsers
import NumberParsers
import Primitives

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val =
  case val of
    String _ -> return val
    Char _ -> return val
    Number _ -> return val
    Bool _ -> return val
    (List [Atom "quote", val]) -> return val
    List (Atom func : args) -> mapM eval args >>= apply func
    Atom _ -> error "Atom evaluation hasn't been implemented yet"
    DottedList _ _ -> undefined
    Vector _ -> error "Vector evaluation hasn't been implemented yet"
    badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)
