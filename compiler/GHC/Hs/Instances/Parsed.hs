{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module contains Data instances for GhcPs (parsed) phase that have
-- NO transitive dependencies on HsExpr, HsBindLR, HsLocalBinds, HsUntypedSplice,
-- or HsType (which contains HsSpliceTy -> HsUntypedSplice -> HsExpr).
--
-- Most GhcPs instances are in Renamed.hs because NHsValBindsLR (used by
-- HsLocalBinds inside HsExpr) has a field of type [LSig GhcRn].  Any Data
-- instance for NHsValBindsLR GhcPs would therefore mention GhcRn, so those
-- instances must live in Renamed.hs to avoid cyclic module dependencies.
--
-- Additionally, HsType GhcPs contains HsSpliceTy which uses HsUntypedSplice GhcPs,
-- and HsUntypedSplice contains HsExpr, so HsType and all types containing it must
-- also be in Renamed.hs.
--
-- Types that contain HsType GhcPs:
-- - Sig (TypeSig, PatSynSig, ClassOpSig, etc.)
-- - StandaloneKindSig
-- - RecordPatSynField (contains FieldOcc which has HsType in RdrName instance)
--
-- See #9557 and #18254 for why we use -O0.
{-# OPTIONS_GHC -O0 #-}

module GHC.Hs.Instances.Parsed where

import Data.Data hiding ( Fixity )

import GHC.Prelude
import GHC.Hs.Extension
import Language.Haskell.Syntax.Binds (FixitySig(..))
import GHC.Hs.Expr
import GHC.Hs.Lit
import GHC.Hs.ImpExp
import GHC.Parser.Annotation

-- Import sibling Instance modules for orphan instances we depend on
import GHC.Hs.Instances.Common ()      -- For HsFieldBind

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Binds - GhcPs only (no HsType dependency)
-- ---------------------------------------------------------------------

-- Note: RecordPatSynField, Sig, StandaloneKindSig moved to Renamed.hs
-- (they contain HsType-related types like FieldOcc, HsSigType, HsWildCardBndrs)

deriving instance Data (FixitySig GhcPs)

-- Note: HsIPBinds, IPBind, HsPatSynDir depend on HsExpr - in Renamed.hs

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Expr - GhcPs only (no HsExpr dependency)
-- ---------------------------------------------------------------------

deriving instance Data (FieldLabelStrings GhcPs)
deriving instance Data (HsRecUpdParent GhcPs)
deriving instance Data (DotFieldOcc GhcPs)
deriving instance Data (HsPragE GhcPs)

-- All other Expr instances are in Renamed.hs

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Lit - GhcPs only
-- ---------------------------------------------------------------------

deriving instance Data (HsLit GhcPs)
-- HsOverLit GhcPs is in Renamed.hs (contains ol_witness :: HsExpr GhcPs)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.ImpExp - GhcPs only
-- ---------------------------------------------------------------------

deriving instance Data (ImportDecl GhcPs)
deriving instance Data (IE GhcPs)

-- Eq instance for IE
deriving instance Eq (IE GhcPs)
