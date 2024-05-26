module Main where

import Tests
import Evaluators
import Text.Parsec

main :: IO ()
main = getLine >>= (putString . readExpr)
