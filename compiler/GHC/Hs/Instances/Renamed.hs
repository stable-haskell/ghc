{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module contains Data instances for GhcRn (renamed) phase only.
--
-- See #9557 and #18254 for why we use -O0.
{-# OPTIONS_GHC -O0 #-}

module GHC.Hs.Instances.Renamed where

import Data.Data hiding ( Fixity )

import GHC.Prelude
import GHC.Hs.Extension
import GHC.Hs.Binds
import GHC.Hs.Decls
import GHC.Hs.Expr
import GHC.Hs.Lit
import GHC.Hs.Type
import GHC.Hs.Pat
import GHC.Hs.ImpExp
import GHC.Parser.Annotation

-- Import sibling Instance modules for orphan instances we depend on
import GHC.Hs.Instances.Common ()      -- For HsFieldBind
import GHC.Hs.Instances.Parsed ()      -- For HsType GhcPs, HsMultAnnOf (HsType) GhcPs, HsLit GhcPs, etc.
-- Note: Transitions.hs is now empty, all LR instances are in phase-specific modules

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Binds - GhcRn only
-- ---------------------------------------------------------------------

-- Note: NHsValBindsLR GhcPs is here because the type contains LSig GhcRn (hard-coded)
deriving instance Data (NHsValBindsLR GhcPs)
deriving instance Data (NHsValBindsLR GhcRn)
deriving instance Data (RecordPatSynField GhcRn)
deriving instance Data (HsIPBinds GhcRn)
deriving instance Data (IPBind GhcRn)
deriving instance Data (Sig GhcRn)
deriving instance Data (FixitySig GhcRn)
deriving instance Data (StandaloneKindSig GhcRn)
deriving instance Data (HsPatSynDir GhcRn)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Decls - GhcRn only
-- ---------------------------------------------------------------------

deriving instance Data (HsDecl GhcRn)
deriving instance Data (HsGroup GhcRn)
deriving instance Data (SpliceDecl GhcRn)
deriving instance Data (TyClDecl GhcRn)
deriving instance Data (FunDep GhcRn)
deriving instance Data (TyClGroup GhcRn)
deriving instance Data (FamilyResultSig GhcRn)
deriving instance Data (FamilyDecl GhcRn)
deriving instance Data (InjectivityAnn GhcRn)
deriving instance Data (FamilyInfo GhcRn)
deriving instance Data (HsDataDefn GhcRn)
deriving instance Data (HsDerivingClause GhcRn)
deriving instance Data (DerivClauseTys GhcRn)
deriving instance Data (ConDecl GhcRn)
deriving instance Data (HsConDeclGADTDetails GhcRn)
deriving instance Data (TyFamInstDecl GhcRn)
deriving instance Data (DataFamInstDecl GhcRn)
deriving instance Data rhs => Data (FamEqn GhcRn rhs)
deriving instance Data (ClsInstDecl GhcRn)
deriving instance Data (InstDecl GhcRn)
deriving instance Data (DerivDecl GhcRn)
deriving instance Data (DerivStrategy GhcRn)
deriving instance Data (DefaultDecl GhcRn)
deriving instance Data (ForeignDecl GhcRn)
deriving instance Data (ForeignImport GhcRn)
deriving instance Data (ForeignExport GhcRn)
deriving instance Data (RuleDecls GhcRn)
deriving instance Data (RuleDecl GhcRn)
deriving instance Data (RuleBndr GhcRn)
deriving instance Data (RuleBndrs GhcRn)
deriving instance Data (WarnDecls GhcRn)
deriving instance Data (WarnDecl GhcRn)
deriving instance Data (AnnProvenance GhcRn)
deriving instance Data (AnnDecl GhcRn)
deriving instance Data (RoleAnnotDecl GhcRn)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Expr - GhcRn only
-- ---------------------------------------------------------------------

deriving instance Data (FieldLabelStrings GhcRn)
deriving instance Data (HsRecUpdParent GhcRn)
deriving instance Data (LHsRecUpdFields GhcRn)
deriving instance Data (DotFieldOcc GhcRn)
deriving instance Data (HsPragE GhcRn)
deriving instance Data (HsExpr GhcRn)
deriving instance Data (HsTupArg GhcRn)
deriving instance Data (HsCmd GhcRn)
deriving instance Data (HsCmdTop GhcRn)

deriving instance Data (MatchGroup GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (MatchGroup GhcRn (LocatedA (HsCmd GhcRn)))

deriving instance Data (Match GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (Match GhcRn (LocatedA (HsCmd GhcRn)))

deriving instance Data (GRHSs GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (GRHSs GhcRn (LocatedA (HsCmd GhcRn)))

deriving instance Data (GRHS GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (GRHS GhcRn (LocatedA (HsCmd GhcRn)))

deriving instance Data (ApplicativeArg GhcRn)
deriving instance Data (HsUntypedSplice GhcRn)
deriving instance Data (HsTypedSplice GhcRn)
deriving instance Data (HsQuote GhcRn)
deriving instance Data (ArithSeqInfo GhcRn)

-- ---------------------------------------------------------------------
-- GhcPs instances that depend on HsLocalBinds (moved from Parsed.hs)
-- ---------------------------------------------------------------------
-- These are here because they contain HsLocalBinds which depends on
-- NHsValBindsLR GhcPs which contains [LSig GhcRn] (hard-coded).

deriving instance Data (HsExpr GhcPs)
deriving instance Data (HsCmd GhcPs)

deriving instance Data (MatchGroup GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (MatchGroup GhcPs (LocatedA (HsCmd GhcPs)))

deriving instance Data (Match GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (Match GhcPs (LocatedA (HsCmd GhcPs)))

deriving instance Data (GRHSs GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (GRHSs GhcPs (LocatedA (HsCmd GhcPs)))

deriving instance Data (GRHS GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (GRHS GhcPs (LocatedA (HsCmd GhcPs)))

deriving instance Data (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (StmtLR GhcPs GhcPs (LocatedA (HsCmd GhcPs)))

-- Other GhcPs instances that depend on HsExpr/HsCmd GhcPs
deriving instance Data (HsTupArg GhcPs)
deriving instance Data (HsCmdTop GhcPs)
deriving instance Data (ApplicativeArg GhcPs)
deriving instance Data (HsQuote GhcPs)
deriving instance Data (ArithSeqInfo GhcPs)
deriving instance Data (ApplicativeStmt GhcPs GhcPs)
deriving instance Data (ParStmtBlock GhcPs GhcPs)
deriving instance Data (HsOverLit GhcPs)
deriving instance Data (HsMultAnnOf (LocatedA (HsExpr GhcPs)) GhcPs)
deriving instance Data (AnnDecl GhcPs)
deriving instance Data (LHsRecUpdFields GhcPs)
deriving instance Data (HsUntypedSplice GhcPs)
deriving instance Data (HsTypedSplice GhcPs)
deriving instance Data (Pat GhcPs)

-- GhcPs binding instances that depend on HsExpr/HsType GhcPs
deriving instance Data (HsIPBinds GhcPs)
deriving instance Data (IPBind GhcPs)
deriving instance Data (HsPatSynDir GhcPs)
deriving instance Data (RecordPatSynField GhcPs)
deriving instance Data (Sig GhcPs)
deriving instance Data (StandaloneKindSig GhcPs)

-- GhcPs decl instances - all depend on HsExpr/HsBind/Splice transitively
deriving instance Data (HsDecl GhcPs)
deriving instance Data (HsGroup GhcPs)
deriving instance Data (SpliceDecl GhcPs)
deriving instance Data (TyClDecl GhcPs)
deriving instance Data (FunDep GhcPs)
deriving instance Data (TyClGroup GhcPs)
deriving instance Data (FamilyResultSig GhcPs)
deriving instance Data (FamilyDecl GhcPs)
deriving instance Data (InjectivityAnn GhcPs)
deriving instance Data (FamilyInfo GhcPs)
deriving instance Data (HsDataDefn GhcPs)
deriving instance Data (HsDerivingClause GhcPs)
deriving instance Data (DerivClauseTys GhcPs)
deriving instance Data (ConDecl GhcPs)
deriving instance Data (HsConDeclGADTDetails GhcPs)
deriving instance Data (TyFamInstDecl GhcPs)
deriving instance Data (DataFamInstDecl GhcPs)
deriving instance Data rhs => Data (FamEqn GhcPs rhs)
deriving instance Data (ClsInstDecl GhcPs)
deriving instance Data (InstDecl GhcPs)
deriving instance Data (DerivDecl GhcPs)
deriving instance Data (DerivStrategy GhcPs)
deriving instance Data (DefaultDecl GhcPs)
deriving instance Data (ForeignDecl GhcPs)
deriving instance Data (ForeignImport GhcPs)
deriving instance Data (ForeignExport GhcPs)
deriving instance Data (RuleDecls GhcPs)
deriving instance Data (RuleDecl GhcPs)
deriving instance Data (RuleBndr GhcPs)
deriving instance Data (RuleBndrs GhcPs)
deriving instance Data (WarnDecls GhcPs)
deriving instance Data (WarnDecl GhcPs)
deriving instance Data (AnnProvenance GhcPs)
deriving instance Data (RoleAnnotDecl GhcPs)

-- GhcPs type instances - depend on HsUntypedSplice via HsSpliceTy
deriving instance Data (HsBndrVis GhcPs)
deriving instance Data (LHsQTyVars GhcPs)
deriving instance Data flag => Data (HsOuterTyVarBndrs flag GhcPs)
deriving instance Data (HsSigType GhcPs)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcPs thing)
deriving instance Data (HsPatSigType GhcPs)
deriving instance Data (HsTyPat GhcPs)
deriving instance Data (HsForAllTelescope GhcPs)
deriving instance (Data flag) => Data (HsTyVarBndr flag GhcPs)
deriving instance Data (HsBndrVar GhcPs)
deriving instance Data (HsBndrKind GhcPs)
deriving instance Data (HsType GhcPs)
deriving instance Data (HsTyLit GhcPs)
deriving instance Data (HsMultAnnOf (LocatedA (HsType GhcPs)) GhcPs)
deriving instance (Data a, Data b) => Data (HsArg GhcPs a b)
deriving instance Data (HsConDeclRecField GhcPs)
deriving instance Data (HsConDeclField GhcPs)
deriving instance Data (FieldOcc GhcPs)

-- GhcPs pattern instances
deriving instance (Data body) => Data (HsRecFields GhcPs body)

-- GhcPs extension type instances
deriving instance Data HsTypeGhcPsExt
deriving instance Data XViaStrategyPs

-- Cross-phase ApplicativeStmt instances:
-- - GhcRn GhcPs: needs ApplicativeArg GhcRn (here) + SyntaxExpr GhcPs (NoExtField, trivial)
-- - GhcPs GhcRn: needs ApplicativeArg GhcPs (above) + SyntaxExprRn (below)
-- - GhcRn GhcRn: needs ApplicativeArg GhcRn + SyntaxExprRn (both here)
deriving instance Data (ApplicativeStmt GhcRn GhcPs)
deriving instance Data (ApplicativeStmt GhcPs GhcRn)
deriving instance Data (ApplicativeStmt GhcRn GhcRn)

-- ParStmtBlock with GhcRn as RIGHT type parameter
-- (depend on SyntaxExprRn defined below)
deriving instance Data (ParStmtBlock GhcPs GhcRn)
deriving instance Data (ParStmtBlock GhcRn GhcRn)

-- StmtLR with GhcRn as second type parameter
deriving instance Data (StmtLR GhcPs GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (StmtLR GhcRn GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (StmtLR GhcPs GhcRn (LocatedA (HsCmd GhcRn)))
deriving instance Data (StmtLR GhcRn GhcRn (LocatedA (HsCmd GhcRn)))

-- ---------------------------------------------------------------------
-- LR instances (formerly in Transitions.hs)
-- ---------------------------------------------------------------------

-- GhcPs GhcPs instances are here (not in Parsed.hs) because they depend on
-- NHsValBindsLR GhcPs which contains [LSig GhcRn] (hard-coded in the type).
-- The order matters: NHsValBindsLR GhcPs needs Sig GhcRn (line 43 above)
deriving instance Data (HsLocalBindsLR GhcPs GhcPs)
deriving instance Data (HsValBindsLR GhcPs GhcPs)
deriving instance Data (HsBindLR GhcPs GhcPs)
deriving instance Data (PatSynBind GhcPs GhcPs)

-- GhcPs GhcRn and GhcRn GhcRn instances
deriving instance Data (HsLocalBindsLR GhcPs GhcRn)
deriving instance Data (HsLocalBindsLR GhcRn GhcRn)
deriving instance Data (HsValBindsLR GhcPs GhcRn)
deriving instance Data (HsValBindsLR GhcRn GhcRn)
deriving instance Data (HsBindLR GhcPs GhcRn)
deriving instance Data (HsBindLR GhcRn GhcRn)
deriving instance Data (PatSynBind GhcPs GhcRn)
deriving instance Data (PatSynBind GhcRn GhcRn)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Lit - GhcRn only
-- ---------------------------------------------------------------------

deriving instance Data (HsLit GhcRn)
deriving instance Data (HsOverLit GhcRn)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Pat - GhcRn only
-- ---------------------------------------------------------------------

deriving instance Data (Pat GhcRn)
deriving instance (Data body) => Data (HsRecFields GhcRn body)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Type - GhcRn only
-- ---------------------------------------------------------------------

deriving instance Data (HsBndrVis GhcRn)
deriving instance Data (LHsQTyVars GhcRn)
deriving instance Data flag => Data (HsOuterTyVarBndrs flag GhcRn)
deriving instance Data (HsSigType GhcRn)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcRn thing)
deriving instance Data (HsPatSigType GhcRn)
deriving instance Data (HsTyPat GhcRn)
deriving instance Data (HsForAllTelescope GhcRn)
deriving instance (Data flag) => Data (HsTyVarBndr flag GhcRn)
deriving instance Data (HsBndrVar GhcRn)
deriving instance Data (HsBndrKind GhcRn)
deriving instance Data (HsType GhcRn)
deriving instance Data (HsTyLit GhcRn)
deriving instance Data (HsMultAnnOf (LocatedA (HsType GhcRn)) GhcRn)
deriving instance Data (HsMultAnnOf (LocatedA (HsExpr GhcRn)) GhcRn)
deriving instance (Data a, Data b) => Data (HsArg GhcRn a b)
deriving instance Data (HsConDeclRecField GhcRn)
deriving instance Data (HsConDeclField GhcRn)
deriving instance Data (FieldOcc GhcRn)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.ImpExp - GhcRn only
-- ---------------------------------------------------------------------

deriving instance Data (ImportDecl GhcRn)
deriving instance Data (IE GhcRn)

-- Eq instance for IE
deriving instance Eq (IE GhcRn)

-- Extension type instances (depend on GhcRn types defined above)
deriving instance Data HsThingRn
deriving instance Data XXExprGhcRn
deriving instance Data PendingRnSplice
deriving instance Data SyntaxExprRn
deriving instance Data OverLitRn
deriving instance Data XBindStmtRn
