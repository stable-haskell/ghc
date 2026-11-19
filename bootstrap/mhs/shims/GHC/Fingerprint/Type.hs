-- Re-export GHC.Fingerprint.Fingerprint from installed ghc-compat so that
-- GHC.Fingerprint and GHC.Fingerprint.Type share the exact same type.
module GHC.Fingerprint.Type
  ( Fingerprint(..)
  ) where

import GHC.Fingerprint (Fingerprint(..))
