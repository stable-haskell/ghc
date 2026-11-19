-- MicroHs has no GHC.Unicode.  GHC.Unit.Module.Warnings imports the
-- whole module but only uses isAlphaNum; we re-export the Data.Char
-- toolkit matching GHC.Unicode's export list (plus the extra
-- classifiers MicroHs provides).
module GHC.Unicode
  ( unicodeVersion
  , GeneralCategory(..)
  , generalCategory
  , isAscii
  , isLatin1
  , isControl
  , isAsciiUpper
  , isAsciiLower
  , isPrint
  , isSpace
  , isUpper
  , isUpperCase
  , isLower
  , isLowerCase
  , isAlpha
  , isAlphaNum
  , isDigit
  , isOctDigit
  , isHexDigit
  , isNumber
  , isMark
  , isSeparator
  , isPunctuation
  , isSymbol
  , toUpper
  , toLower
  , toTitle
  ) where

import Prelude

import Data.Char
import Data.Version (Version, makeVersion)

unicodeVersion :: Version
unicodeVersion = makeVersion [17, 0, 0]

isUpperCase :: Char -> Bool
isUpperCase = isUpper

isLowerCase :: Char -> Bool
isLowerCase = isLower
