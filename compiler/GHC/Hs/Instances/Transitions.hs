{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module originally contained Data instances for "LR" types that have
-- two phase parameters (Left and Right), representing transformations
-- between compiler phases (e.g., GhcPs -> GhcRn during renaming).
--
-- However, all LR types depend on phase-specific instances (like HsPatSynDir,
-- SyntaxExpr, etc.), so all instances have been moved to the phase-specific
-- modules (Parsed.hs, Renamed.hs, Typechecked.hs) based on their RIGHT type
-- parameter which determines which phase-specific types they need.
--
-- This module is kept for backwards compatibility and re-exports nothing.
-- It should not be imported in new code; it exists only so that existing
-- imports do not break. Consider removing it once all downstream consumers
-- have been updated.
--
-- See #9557 and #18254 for why we use -O0.
{-# OPTIONS_GHC -O0 #-}
{-# DEPRECATED "This module is empty; import the phase-specific GHC.Hs.Instances.* modules instead." #-}

module GHC.Hs.Instances.Transitions where

-- All LR instances have been moved to phase-specific modules:
--
-- Parsed.hs (GhcPs GhcPs variants):
--   HsLocalBindsLR, HsValBindsLR, HsBindLR, PatSynBind,
--   StmtLR, ParStmtBlock, ApplicativeStmt
--
-- Renamed.hs (GhcPs GhcRn and GhcRn GhcRn variants):
--   HsLocalBindsLR, HsValBindsLR, HsBindLR, PatSynBind,
--   StmtLR, ParStmtBlock, ApplicativeStmt
--
-- Typechecked.hs (GhcTc GhcTc variants):
--   HsLocalBindsLR, HsValBindsLR, HsBindLR, PatSynBind,
--   StmtLR, ParStmtBlock, ApplicativeStmt, HsMultAnnOf cross-phase
