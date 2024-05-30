module Main where

import Evaluators
import Tests
import ErrorHandling
import Text.Parsec

main :: IO ()
main = do
  arg <- getLine
  evaluated <- return . fmap show $ readExpr arg >>= eval
  putStrLn . extractValue . trapError $ evaluated