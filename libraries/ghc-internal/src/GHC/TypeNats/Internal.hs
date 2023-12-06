{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}
module GHC.TypeNats.Internal (
  SNat (..),
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
newtype SNat (n :: Natural) = UnsafeSNat Natural
type role SNat nominal

-- | /since base-4.19.0.0/
instance Eq (SNat n) where
  _ == _ = True

-- | /since 4.19.0.0/
instance Ord (SNat n) where
  compare _ _ = EQ
