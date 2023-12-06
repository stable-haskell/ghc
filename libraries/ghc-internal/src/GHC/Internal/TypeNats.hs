{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PatternSynonyms #-}
module GHC.Internal.TypeNats (
  SNat (UnsafeSNat),
)where

import GHC.Num.Natural(Natural)
import GHC.Types (Bool (..), Ordering (..))
import GHC.Classes (Eq (..), Ord (..))
import GHC.Num.Integer () -- Note [Depend on GHC.Num.Integer] in GHC.Base

-- | A value-level witness for a type-level natural number. This is commonly
-- referred to as a /singleton/ type, as for each @n@, there is a single value
-- that inhabits the type @'SNat' n@ (aside from bottom).
--
-- The definition of 'SNat' is intentionally left abstract. To obtain an 'SNat'
-- value, use one of the following:
--
-- 1. The 'natSing' method of 'KnownNat'.
--
-- 2. The @SNat@ pattern synonym.
--
-- 3. The 'withSomeSNat' function, which creates an 'SNat' from a 'Natural'
--    number.
--
-- /since base-4.18.0.0/
--
newtype SNat (n :: Natural) = UnsafeSNat_ Natural
type role SNat nominal

-- See Note [SNat constructor]
pattern UnsafeSNat :: Natural -> SNat n
pattern UnsafeSNat n = UnsafeSNat_ n
{-# COMPLETE UnsafeSNat #-}

-- | /since base-4.19.0.0/
instance Eq (SNat n) where
  _ == _ = True

-- | /since 4.19.0.0/
instance Ord (SNat n) where
  compare _ _ = EQ

{-
Note [SNat constructor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a concern raised that having the real constructor of SNat exposed may
allow accidental 'coerce'.  To avoid that, we define a pattern synonym. It
looks like real constructor, but prevents from coercing SNats when
(pseudo)constructor is in scope.

-}
