{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module RefalTest where

import Language.Refal
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

test_stub =
    do
        assertBool  (True)
        assertEqual 0 0
