-- The compiler imports GHC.Bits from base.  MicroHs exports the same
-- operations from Data.Bits, plus extra Monoid wrappers (And, Ior, Xor,
-- Iff) and shift operators that GHC.Bits does not export and that
-- collide with Language.Haskell.Syntax.BooleanFormula.And.
module GHC.Bits
  ( Bits(..)
  , FiniteBits(..)
  , bitDefault
  , testBitDefault
  , popCountDefault
  , toIntegralSized
  ) where

import Data.Bits
