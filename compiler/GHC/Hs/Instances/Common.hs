{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module contains TRULY phase-independent Data instances.
-- These are types that have NO dependency on phase-specific types
-- (GhcPs/GhcRn/GhcTc) or types containing them.
--
-- Types with Rn/Tc in their name or that contain HsExpr/Pat/HsType of
-- a specific phase should go in Renamed.hs or Typechecked.hs respectively.
--
-- See #9557 and #18254 for why we use -O0.
{-# OPTIONS_GHC -O0 #-}

module GHC.Hs.Instances.Common where

import Data.Data hiding ( Fixity )

import GHC.Prelude
import GHC.Hs.Extension
import GHC.Hs.Expr
import GHC.Hs.Pat
import GHC.Types.Name.Reader (WithUserRdr(..))
import GHC.Data.BooleanFormula (BooleanFormula(..))
import Language.Haskell.Syntax.Extension (Anno)

-- ---------------------------------------------------------------------
-- Truly phase-independent instances
-- These types have no phase-specific type parameters inside them
-- ---------------------------------------------------------------------

deriving instance Data HsArrowMatchContext

deriving instance Data fn => Data (HsStmtContext fn)
deriving instance Data fn => Data (HsMatchContext fn)

-- Splice types without phase-specific content
deriving instance Data HsImplicitLiftSplice
deriving instance Data HsUserSpliceExt
deriving instance Data HsQuasiQuoteExt
deriving instance Data a => Data (HsUntypedSpliceResult a)
deriving instance Data HsTypedSpliceResult

-- Polymorphic instance
deriving instance (Data a, Data b) => Data (HsFieldBind a b)
deriving instance Data a => Data (WithUserRdr a)

-- ---------------------------------------------------------------------
-- BooleanFormula instance (polymorphic over GhcPass)
-- ---------------------------------------------------------------------

deriving instance (Typeable p, Data (Anno (IdGhcP p)), Data (IdGhcP p)) => Data (BooleanFormula (GhcPass p))

-- ---------------------------------------------------------------------
-- NOTE: Many types that look "common" actually have phase dependencies:
--
-- Moved to Renamed.hs (depend on HsExpr GhcRn, etc.):
--   PendingRnSplice, SyntaxExprRn, OverLitRn, XBindStmtRn, HsThingRn, XXExprGhcRn
--
-- Moved to Typechecked.hs (depend on HsExpr GhcTc, Pat GhcTc, etc.):
--   AbsBinds, ABExport, TcSpecPrags, TcSpecPrag, RecStmtTc, HsBracketTc,
--   CmdTopTc, PendingTcSplice, SyntaxExprTc, XBindStmtTc, HsLitTc, OverLitTc,
--   ConPatTc, XXExprGhcTc, XXPatGhcTc
--
-- Moved to Parsed.hs (depend on HsType GhcPs, HsSigType GhcPs):
--   HsTypeGhcPsExt, XViaStrategyPs
-- ---------------------------------------------------------------------
