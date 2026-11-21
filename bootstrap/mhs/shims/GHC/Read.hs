-- MicroHs has no GHC.Read.  GHC.Platform only needs the Read class
-- (derived Platform instances plus a hand-written PlatformWordSize
-- instance using readPrec), but we re-export the full reader toolkit
-- so later stages keep compiling.
module GHC.Read
  ( Read(..)
  , ReadS
  , reads
  , read
  , readParen
  , lex
  , Lexeme(..)
  , lexP
  , parens
  , readListPrecDefault
  , readListDefault
  , readEither
  , readMaybe
  , expectP
  , choose
  ) where

import Prelude

import Text.Read
import Text.Read.Lex (Lexeme(..))
import Text.Read.Internal (choose)
