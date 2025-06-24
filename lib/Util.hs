module Util where

import Data.Char (toLower)

stripPrefixModifier :: String -> String -> String
stripPrefixModifier p s =
  if take (length p) s == p
  then uncap $ drop (length p) s
  else s
  where uncap s = (toLower . head) s : tail s
