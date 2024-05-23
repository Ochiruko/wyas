module Main where

import Text.Parsec
import NumberParsers
import LispValParsers
import Tests

main :: IO ()
main = runTests
  
evalNums = 
  do putStrLn "testing parseNum."
     mainLoop
     where mainLoop = 
             do putStrLn "Enter a Scheme number: "
                input <- getLine
                let output = case parse parseNum "lisp" input of
                               Left err -> "there was an error: " ++ show err
                               Right succ -> "success: " ++ show succ
                putStrLn output
                mainLoop