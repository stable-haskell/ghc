{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
module GHC.TypeNats.Experimental where

import "ghc-internal" GHC.TypeNats.Internal
import GHC.TypeNats

plusSNat :: SNat n -> SNat m -> SNat (n + m)
plusSNat (UnsafeSNat n) (UnsafeSNat m) = UnsafeSNat (n + m)

timesSNat :: SNat n -> SNat m -> SNat (n * m)
timesSNat (UnsafeSNat n) (UnsafeSNat m) = UnsafeSNat (n * m)

-- power
-- minus
-- div
-- mod
-- log2
