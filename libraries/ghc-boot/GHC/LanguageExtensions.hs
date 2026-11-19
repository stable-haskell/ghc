{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module re-exports the 'Extension' type along with an orphan 'Binary'
-- instance for it.
--
-- Note that the @ghc-boot@ package has a large set of dependencies; for this
-- reason the 'Extension' type itself is defined in the
-- "GHC.LanguageExtensions.Type" module provided by the @ghc-boot-th@ package,
-- which has no dependencies outside of @base@. For this reason
-- @template-haskell@ depends upon @ghc-boot-th@, not @ghc-boot@.
--
module GHC.LanguageExtensions ( module GHC.LanguageExtensions.Type ) where

import Data.Binary
import GHC.LanguageExtensions.Type
#if defined(__MHS__)
import Prelude
#endif

#if defined(__MHS__)
-- binary-mhs has no Generic defaults.  Constructor order is Enum order.
instance Binary Extension where
  put e = put (fromEnum e :: Int)
  get = fmap toEnum (get :: Get Int)
#else
instance Binary Extension
#endif
