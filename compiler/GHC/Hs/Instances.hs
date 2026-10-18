{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module re-exports all Data instances for the GHC Haskell AST.
--
-- The instances are split into separate modules to enable parallel compilation:
--
--   * "GHC.Hs.Instances.Common" - Phase-independent instances
--   * "GHC.Hs.Instances.Transitions" - LR types spanning multiple phases
--   * "GHC.Hs.Instances.Parsed" - GhcPs (parsed) phase instances
--   * "GHC.Hs.Instances.Renamed" - GhcRn (renamed) phase instances
--   * "GHC.Hs.Instances.Typechecked" - GhcTc (typechecked) phase instances
--
-- See #9557 and #18254 for background on why these instances are slow to compile.
--
-- Note: This module and its sub-modules are excluded from stage1 builds
-- (via the -interpreter flag) since Data instances are only needed at runtime
-- for Template Haskell and GHCi, not for compilation.

module GHC.Hs.Instances (
  -- Re-exported for backward compatibility
) where

-- Import sub-modules for their orphan instances
import GHC.Hs.Instances.Common ()
import GHC.Hs.Instances.Transitions ()
import GHC.Hs.Instances.Parsed ()
import GHC.Hs.Instances.Renamed ()
import GHC.Hs.Instances.Typechecked ()
