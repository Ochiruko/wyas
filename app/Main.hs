module Main where

import LispValParsers
import NumberParsers
import Tests
import Text.Parsec

main :: IO ()
main = runTests

evalNums = do
  putStrLn "testing parseNum."
  mainLoop
  where
    mainLoop = do
      putStrLn "Enter a Scheme number: "
      input <- getLine
      let output =
            case parse parseNum "lisp" input of
              Left err -> "there was an error: " ++ show err
              Right succ -> "success: " ++ show succ
      putStrLn output
      mainLoop
