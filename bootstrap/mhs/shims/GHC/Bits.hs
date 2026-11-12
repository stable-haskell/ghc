-- The compiler imports GHC.Bits from base.  MicroHs exports the same
-- operations from Data.Bits.
module GHC.Bits (module Data.Bits) where
import Data.Bits
