{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Installed unit id of this build of @ghc-internal@.
--
-- Cabal's @cabal_macros.h@ defines @CURRENT_PACKAGE_KEY@ to that unit id.
-- The compiler copies 'unitId' into @cGhcInternalUnitId@. A build that does
-- not pass the header keeps the wired-in name.
module GHC.Internal.ThisUnitId (unitId) where

import GHC.Internal.Types (Char, List)

-- A string literal desugars through unpackCString# and unpackCStringUtf8#
-- in GHC.Internal.CString. This module is compiled as unit ghc-internal, so
-- that lookup reads the home-package interface. --make only loads a home
-- interface that is already built. The import is the edge that builds
-- CString first. CString itself has no Haskell string literal, so it does
-- not need its own interface to compile.
import GHC.Internal.CString ()

unitId :: List Char
#ifdef CURRENT_PACKAGE_KEY
unitId = CURRENT_PACKAGE_KEY
#else
unitId = "ghc-internal"
#endif
