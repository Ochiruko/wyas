module Evaluators where

import LispValParsers
import NumberParsers
import Primitives

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

eval :: LispVal -> LispVal
eval val =
  case val of
    String _ -> val
    Char _ -> val
    Number _ -> val
    Bool _ -> val
    (List [Atom "quote", val]) -> val
    List (Atom func:args) -> apply func $ map eval args
    Atom _ -> error "Atom evaluation hasn't been implemented yet"
    DottedList _ _ -> error "DottedList evaluation hasn't been implemented yet"
    Vector _ -> error "Vector evaluation hasn't been implemented yet"
    _ -> error $ "this failed: " ++ show val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primOps
