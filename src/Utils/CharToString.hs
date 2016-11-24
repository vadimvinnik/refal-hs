module Utils.CharToString where

import Data.String.ToString

instance ToString Char where
  toString c = [c]
