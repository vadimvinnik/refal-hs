{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module RefalTest where

import Data.String.ToString
import Language.Refal
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

type TestObjectExpr = ObjectExpr Char
type TestPatternExpr = PatternExpr String Char
type TestActiveExpr = ActiveExpr String String Char

type TestPatternItem = PatternItem String Char
type TestActiveItem = ActiveItem String String Char

type TestSentence = Sentence String String Char

type TestFunctionBody = FunctionBody String String Char

type TestModule = Module String String Char

testExprToString :: TestObjectExpr -> String
testExprToString = toString

test_toString_blocks =
    do
        assertEqual
          (testExprToString empty)
          ""
        assertEqual
          (testExprToString (block empty))
          "()"
        assertEqual
          (testExprToString ((block empty) .+. (block empty)))
          "()()"
        assertEqual
          (testExprToString ((block empty) .+. (block ((block empty) .+. (block empty)))))
          "()(()())"
        assertEqual
          (testExprToString (quote "ab"))
          "ab"
        assertEqual
          (testExprToString (block (quote ":-) and :-(")))
          "(:-\\) and :-\\()"
