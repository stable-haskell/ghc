{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax.Expr where

import Language.Haskell.Syntax.Extension ( XRec )
import Data.Kind  ( Type )

import Prelude (Eq)
import Data.Data (Data)
#if defined(__MHS__)
import Data.Typeable (Typeable)
#endif

type role HsExpr nominal
type role MatchGroup nominal nominal
type role GRHSs nominal nominal
type role HsUntypedSplice nominal
data HsExpr (i :: Type)
data HsUntypedSplice (i :: Type)
data MatchGroup (a :: Type) (body :: Type)
data GRHSs (a :: Type) (body :: Type)
type family SyntaxExpr (i :: Type)

type LHsExpr a = XRec a (HsExpr a)

data HsDoFlavour
#if defined(__MHS__)
instance Typeable HsExpr
instance Typeable HsUntypedSplice
instance Typeable MatchGroup
instance Typeable GRHSs
instance Typeable HsDoFlavour
#endif
instance Eq HsDoFlavour
instance Data HsDoFlavour