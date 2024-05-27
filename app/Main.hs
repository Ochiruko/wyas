module Main where

import Evaluators
import Tests
import Text.Parsec

main :: IO ()
main = getLine >>= print . eval . readExpr
