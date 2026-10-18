{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module contains Data instances for GhcTc (typechecked) phase only.
--
-- See #9557 and #18254 for why we use -O0.
{-# OPTIONS_GHC -O0 #-}

module GHC.Hs.Instances.Typechecked where

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
import GHC.Hs.Instances.Renamed ()     -- For HsType GhcRn (needed by HsMultAnnOf cross-phase instance)
-- Note: Transitions.hs is now empty, all LR instances are in phase-specific modules

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Binds - GhcTc only
-- ---------------------------------------------------------------------

deriving instance Data (NHsValBindsLR GhcTc)
deriving instance Data (RecordPatSynField GhcTc)
deriving instance Data (HsIPBinds GhcTc)
deriving instance Data (IPBind GhcTc)
deriving instance Data (Sig GhcTc)
deriving instance Data (FixitySig GhcTc)
deriving instance Data (StandaloneKindSig GhcTc)
deriving instance Data (HsPatSynDir GhcTc)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Decls - GhcTc only
-- ---------------------------------------------------------------------

deriving instance Data (HsDecl GhcTc)
deriving instance Data (HsGroup GhcTc)
deriving instance Data (SpliceDecl GhcTc)
deriving instance Data (TyClDecl GhcTc)
deriving instance Data (FunDep GhcTc)
deriving instance Data (TyClGroup GhcTc)
deriving instance Data (FamilyResultSig GhcTc)
deriving instance Data (FamilyDecl GhcTc)
deriving instance Data (InjectivityAnn GhcTc)
deriving instance Data (FamilyInfo GhcTc)
deriving instance Data (HsDataDefn GhcTc)
deriving instance Data (HsDerivingClause GhcTc)
deriving instance Data (DerivClauseTys GhcTc)
deriving instance Data (ConDecl GhcTc)
deriving instance Data (HsConDeclGADTDetails GhcTc)
deriving instance Data (TyFamInstDecl GhcTc)
deriving instance Data (DataFamInstDecl GhcTc)
deriving instance Data rhs => Data (FamEqn GhcTc rhs)
deriving instance Data (ClsInstDecl GhcTc)
deriving instance Data (InstDecl GhcTc)
deriving instance Data (DerivDecl GhcTc)
deriving instance Data (DerivStrategy GhcTc)
deriving instance Data (DefaultDecl GhcTc)
deriving instance Data (ForeignDecl GhcTc)
deriving instance Data (ForeignImport GhcTc)
deriving instance Data (ForeignExport GhcTc)
deriving instance Data (RuleDecls GhcTc)
deriving instance Data (RuleDecl GhcTc)
deriving instance Data (RuleBndr GhcTc)
deriving instance Data (RuleBndrs GhcTc)
deriving instance Data (WarnDecls GhcTc)
deriving instance Data (WarnDecl GhcTc)
deriving instance Data (AnnProvenance GhcTc)
deriving instance Data (AnnDecl GhcTc)
deriving instance Data (RoleAnnotDecl GhcTc)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Expr - GhcTc only
-- ---------------------------------------------------------------------

deriving instance Data (FieldLabelStrings GhcTc)
deriving instance Data (HsRecUpdParent GhcTc)
deriving instance Data (LHsRecUpdFields GhcTc)
deriving instance Data (DotFieldOcc GhcTc)
deriving instance Data (HsPragE GhcTc)
deriving instance Data (HsExpr GhcTc)
deriving instance Data (HsTupArg GhcTc)
deriving instance Data (HsCmd GhcTc)
deriving instance Data (HsCmdTop GhcTc)

deriving instance Data (MatchGroup GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (MatchGroup GhcTc (LocatedA (HsCmd GhcTc)))

deriving instance Data (Match GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (Match GhcTc (LocatedA (HsCmd GhcTc)))

deriving instance Data (GRHSs GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (GRHSs GhcTc (LocatedA (HsCmd GhcTc)))

deriving instance Data (GRHS GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (GRHS GhcTc (LocatedA (HsCmd GhcTc)))

deriving instance Data (ApplicativeArg GhcTc)
deriving instance Data (HsUntypedSplice GhcTc)
deriving instance Data (HsTypedSplice GhcTc)
deriving instance Data (HsQuote GhcTc)
deriving instance Data (ArithSeqInfo GhcTc)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Lit - GhcTc only
-- ---------------------------------------------------------------------

deriving instance Data (HsLit GhcTc)
deriving instance Data (HsOverLit GhcTc)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Pat - GhcTc only
-- ---------------------------------------------------------------------

deriving instance Data (Pat GhcTc)
deriving instance (Data body) => Data (HsRecFields GhcTc body)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.Type - GhcTc only
-- ---------------------------------------------------------------------

deriving instance Data (HsBndrVis GhcTc)
deriving instance Data (LHsQTyVars GhcTc)
deriving instance Data flag => Data (HsOuterTyVarBndrs flag GhcTc)
deriving instance Data (HsSigType GhcTc)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcTc thing)
deriving instance Data (HsPatSigType GhcTc)
deriving instance Data (HsTyPat GhcTc)
deriving instance Data (HsForAllTelescope GhcTc)
deriving instance (Data flag) => Data (HsTyVarBndr flag GhcTc)
deriving instance Data (HsBndrVar GhcTc)
deriving instance Data (HsBndrKind GhcTc)
deriving instance Data (HsType GhcTc)
deriving instance Data (HsTyLit GhcTc)
deriving instance Data (HsMultAnnOf (LocatedA (HsExpr GhcTc)) GhcTc)
deriving instance (Data a, Data b) => Data (HsArg GhcTc a b)
deriving instance Data (HsConDeclRecField GhcTc)
deriving instance Data (HsConDeclField GhcTc)
deriving instance Data (FieldOcc GhcTc)

-- ---------------------------------------------------------------------
-- Data instances from GHC.Hs.ImpExp - GhcTc only
-- ---------------------------------------------------------------------

deriving instance Data (ImportDecl GhcTc)
deriving instance Data (IE GhcTc)

-- Eq instance for IE
deriving instance Eq (IE GhcTc)

-- Extension type instances (depend on GhcTc types defined above)
deriving instance Data XXExprGhcTc
deriving instance Data XXPatGhcTc

-- Instances from GHC.Hs.Binds that depend on GhcTc types
deriving instance Data AbsBinds
deriving instance Data ABExport
deriving instance Data TcSpecPrags
deriving instance Data TcSpecPrag

-- Instances from GHC.Hs.Expr that depend on GhcTc types
deriving instance Data RecStmtTc
deriving instance Data HsBracketTc
deriving instance Data CmdTopTc
deriving instance Data PendingTcSplice
deriving instance Data SyntaxExprTc
deriving instance Data XBindStmtTc

-- Instances from GHC.Hs.Lit that depend on GhcTc types
deriving instance Data HsLitTc
deriving instance Data OverLitTc

-- Instances from GHC.Hs.Pat that depend on GhcTc types
deriving instance Data ConPatTc

-- ---------------------------------------------------------------------
-- Cross-phase instances moved from Transitions.hs
-- These need phase-specific types that aren't available in Transitions.hs
-- ---------------------------------------------------------------------

-- ApplicativeStmt instances that need types from Typechecked.hs:
-- - GhcPs GhcTc: needs ApplicativeArg GhcPs (Parsed) + SyntaxExprTc (here)
-- - GhcRn GhcTc: needs ApplicativeArg GhcRn (Renamed) + SyntaxExprTc (here)
-- - GhcTc GhcPs: needs ApplicativeArg GhcTc (here) + SyntaxExpr GhcPs (NoExtField, trivial)
-- - GhcTc GhcRn: needs ApplicativeArg GhcTc (here) + SyntaxExprRn (Renamed)
-- - GhcTc GhcTc: needs ApplicativeArg GhcTc + SyntaxExprTc (both here)
deriving instance Data (ApplicativeStmt GhcPs GhcTc)
deriving instance Data (ApplicativeStmt GhcRn GhcTc)
deriving instance Data (ApplicativeStmt GhcTc GhcPs)
deriving instance Data (ApplicativeStmt GhcTc GhcRn)
deriving instance Data (ApplicativeStmt GhcTc GhcTc)

-- ParStmtBlock with GhcTc as RIGHT type parameter
-- (depend on SyntaxExprTc defined above)
deriving instance Data (ParStmtBlock GhcTc GhcTc)

-- StmtLR with GhcTc as second type parameter
deriving instance Data (StmtLR GhcTc GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (StmtLR GhcTc GhcTc (LocatedA (HsCmd GhcTc)))

-- ---------------------------------------------------------------------
-- LR instances with GhcTc GhcTc (formerly in Transitions.hs)
-- ---------------------------------------------------------------------

deriving instance Data (HsLocalBindsLR GhcTc GhcTc)
deriving instance Data (HsValBindsLR GhcTc GhcTc)
deriving instance Data (HsBindLR GhcTc GhcTc)
deriving instance Data (PatSynBind GhcTc GhcTc)

deriving instance Data (HsMultAnnOf (LocatedA (HsType GhcRn)) GhcTc)
