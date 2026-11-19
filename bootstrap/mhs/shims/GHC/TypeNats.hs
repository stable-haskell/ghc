-- MicroHs provides Nat/KnownNat via Data.TypeLits but no natVal'.
-- GHC.Types.Error.Codes needs Nat, KnownNat and
-- natVal' @(DiagnosticCodeFor namespace con) proxy#.
module GHC.TypeNats
  ( Nat
  , KnownNat(..)
  , natVal
  , natVal'
  ) where

import Prelude

import Data.TypeLits (Nat, KnownNat(..))

natVal' :: forall n proxy. KnownNat n => proxy n -> Integer
natVal' = natVal
