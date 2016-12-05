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

object_expr_0 = empty :: TestObjectExpr
object_expr_1 = block empty
object_expr_2 = (block empty) .+. (block empty)
object_expr_3 = (block empty) .+. (block ((block empty) .+. (block empty)))
object_expr_4 = quote "ab"
object_expr_5 = (quote "ab") .+. (block $ quote "cd") .+. (quote "ef")

test_stub =
    do
        assertEqual (toString object_expr_0) ""
