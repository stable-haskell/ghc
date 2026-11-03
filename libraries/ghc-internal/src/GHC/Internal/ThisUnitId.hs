{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Installed unit id of this build of @ghc-internal@.
--
-- Cabal's @cabal_macros.h@ defines @CURRENT_PACKAGE_KEY@ to that unit id.
-- The compiler copies 'unitId' into @cGhcInternalUnitId@. A build that does
-- not pass the header keeps the wired-in name.
module GHC.Internal.ThisUnitId (unitId) where

import GHC.Internal.Types (Char, List)

unitId :: List Char
#ifdef CURRENT_PACKAGE_KEY
unitId = CURRENT_PACKAGE_KEY
#else
unitId = "ghc-internal"
#endif
