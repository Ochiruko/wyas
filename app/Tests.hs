module Tests where

import Data.Ratio
import Test.Hspec
import Text.Parsec
import Text.ParserCombinators.Parsec

import GeneralParsers
import LispValParsers
import NumberParsers

runTests :: IO ()
runTests =
  hspec $ do
    describe "parseNum" $ do
      it "parses all valid scheme numbers" $ do
        parse parseNum "lisp" "#x029f3" `shouldBe` Right (Integer 10739)
        parse parseNum "lisp" "23425." `shouldBe` Right (Real 23425.0)
        parse parseNum "lisp" "23425" `shouldBe` Right (Integer 23425)
        parse parseNum "lisp" "234/25" `shouldBe` Right (Rational (234 % 25))
    describe "parseExpr" $ do
      it "parses all valid scheme expressions" $ do
        parse parseExpr "lisp" "(a test)"
          `shouldBe` Right (List [Atom "a", Atom "test"])
        parse parseExpr "lisp" "(a (nested) test)"
          `shouldBe` Right (List [Atom "a", List [Atom "nested"], Atom "test"])
        parse parseExpr "lisp" "(a '(quoted (dotted . list)) test)"
          `shouldBe` Right
                       (List
                          [ Atom "a"
                          , List
                              [ Atom "quote"
                              , List
                                  [ Atom "quoted"
                                  , DottedList [Atom "dotted"] (Atom "list")
                                  ]
                              ]
                          , Atom "test"
                          ])
        parse parseExpr "lisp" "(a '(imbalanced parens)"
          `shouldNotBe` Right undefined
