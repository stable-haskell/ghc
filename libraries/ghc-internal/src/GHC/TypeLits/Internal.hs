{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}
module GHC.TypeLits.Internal (
  SChar (..),
  SSymbol (..),
) where

import GHC.Types (Char, Symbol, Bool (..), Ordering (..))
import GHC.Classes (Eq (..), Ord (..))
import GHC.Num.Integer () -- Note [Depend on GHC.Num.Integer] in GHC.Base

-- | A value-level witness for a type-level character. This is commonly referred
-- to as a /singleton/ type, as for each @c@, there is a single value that
-- inhabits the type @'SChar' c@ (aside from bottom).
--
-- The definition of 'SChar' is intentionally left abstract. To obtain an
-- 'SChar' value, use one of the following:
--
-- 1. The 'charSing' method of 'KnownChar'.
--
-- 2. The @SChar@ pattern synonym.
--
-- 3. The 'withSomeSChar' function, which creates an 'SChar' from a 'Char'.
--
-- /since base-4.18.0.0/
newtype SChar (s :: Char) = UnsafeSChar Char
type role SChar nominal

-- | /since base-4.19.0.0/
instance Eq (SChar c) where
  _ == _ = True

-- | /since base-4.19.0.0/
instance Ord (SChar c) where
  compare _ _ = EQ

-- | A value-level witness for a type-level symbol. This is commonly referred
-- to as a /singleton/ type, as for each @s@, there is a single value that
-- inhabits the type @'SSymbol' s@ (aside from bottom).
--
-- The definition of 'SSymbol' is intentionally left abstract. To obtain an
-- 'SSymbol' value, use one of the following:
--
-- 1. The 'symbolSing' method of 'KnownSymbol'.
--
-- 2. The @SSymbol@ pattern synonym.
--
-- 3. The 'withSomeSSymbol' function, which creates an 'SSymbol' from a
--    'String'.
--
-- /since base-4.18.0.0/
newtype SSymbol (s :: Symbol) = UnsafeSSymbol [Char]
type role SSymbol nominal

-- | /since base-4.19.0.0/
instance Eq (SSymbol s) where
  _ == _ = True

-- | /since base-4.19.0.0/
instance Ord (SSymbol s) where
  compare _ _ = EQ
