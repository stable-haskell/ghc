{-# LANGUAGE MagicHash #-}

-- | Stub types for builds without interpreter support.
--
-- This module provides stub type definitions that are used when GHC is built
-- without interpreter support (i.e., when @HAVE_INTERPRETER@ is not defined).
-- These stubs allow the compiler to type-check code that references GHCi types
-- without actually depending on the ghci package.
--
-- __Important__: This module should ONLY be compiled and used when
-- @HAVE_INTERPRETER@ is NOT defined. When the interpreter is available,
-- the real types from GHCi should be used instead.
--
-- The conditional import pattern is:
--
-- @
-- #if defined(HAVE_INTERPRETER)
-- import GHCi.RemoteTypes (ForeignRef, ForeignHValue, RemoteRef, RemotePtr, HValueRef)
-- import GHCi.Message (Pipe, THMessage(..), THResultType(..))
-- import GHCi.BreakArray (BreakArray)
-- #else
-- import GHC.Runtime.Interpreter.Stubs
-- #endif
-- @
--
-- NOTE: This module is conditionally compiled via ghc.cabal.in
-- (only when @!flag(interpreter)@)
module GHC.Runtime.Interpreter.Stubs
  ( -- * Core GHCi Types
    HValue(..)
  , ForeignRef(..)
  , ForeignHValue
  , RemoteRef(..)
  , RemotePtr(..)
  , HValueRef
    -- * Communication Types
  , Pipe
  , LoadedDLL
  , ConInfoTable(..)
    -- * Break/Debug Types
  , BreakArray(..)
  , InternalBreakpointId(..)
    -- * Bytecode Types
  , CompiledByteCode(..)
  , UnlinkedBCO(..)
  , InternalModBreaks(..)
  , ModBreaks(..)
  , BreakInfoIndex
  , bcoFreeNames
    -- * Template Haskell Types
  , THMessage(..)
  , THResultType(..)
    -- * Eval Types
  , EvalExpr
  , ResumeContext
  , EvalStep(..)
    -- * StgToJS Types
  , LinkPlan(..)
  , StgToJSConfig(..)
    -- * Linker Environment Types
  , ItblEnv
  , AddrEnv
  ) where

import GHC.Prelude

import GHC.Exts (Any)
import GHC.Types.Name.Env (NameEnv)
import GHC.Types.Name (Name)
import GHC.Types.Unique.DSet (UniqDSet, emptyUniqDSet)
import Data.ByteString (ByteString)

import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import System.IO (Handle)

-- -----------------------------------------------------------------------------
-- Core GHCi Types
-- These are stubs for types from ghci:GHCi.RemoteTypes

-- | Stub for GHCi's HValue - a wrapper around Any
newtype HValue = HValue Any

-- | Stub for GHCi's ForeignRef - a foreign reference to a remote value
newtype ForeignRef a = ForeignRef (ForeignPtr ())

-- | Stub for GHCi's ForeignHValue - a foreign reference to an HValue
type ForeignHValue = ForeignRef HValue

-- | Stub for GHCi's RemoteRef - a reference to a remote value
newtype RemoteRef a = RemoteRef ()

-- | Stub for GHCi's RemotePtr - a pointer to a remote value
newtype RemotePtr a = RemotePtr (Ptr ())

-- | Stub for GHCi's HValueRef - a remote reference to an HValue
type HValueRef = RemoteRef HValue

-- -----------------------------------------------------------------------------
-- Communication Types
-- These are stubs for types from ghci:GHCi.Message

-- | Stub for GHCi's Pipe - communication channel with external interpreter
type Pipe = (Handle, Handle)

-- | Stub for GHCi's LoadedDLL - a loaded dynamic library
type LoadedDLL = ()

-- | Stub for GHCi's ConInfoTable - constructor info table
data ConInfoTable = ConInfoTable

-- -----------------------------------------------------------------------------
-- Break/Debug Types
-- These are stubs for types from ghci:GHCi.BreakArray and
-- ghc:GHC.ByteCode.Breakpoints

-- | Stub for GHCi's BreakArray - array of breakpoint flags
data BreakArray = BreakArray

-- | Stub for InternalBreakpointId - identifies a breakpoint
data InternalBreakpointId = InternalBreakpointId

-- -----------------------------------------------------------------------------
-- Bytecode Types
-- These are stubs for types from ghc:GHC.ByteCode.Types and GHC.ByteCode.Breakpoints

-- | Stub for CompiledByteCode - compiled bytecode
-- This stub provides the record fields needed by Loader.hs
data CompiledByteCode = CompiledByteCode
  { bc_bcos   :: [UnlinkedBCO]          -- ^ Unlinked bytecode objects
  , bc_itbls  :: [(Name, ConInfoTable)] -- ^ Info tables
  , bc_strs   :: [(Name, ByteString)]   -- ^ Top-level strings
  , bc_breaks :: Maybe InternalModBreaks -- ^ Breakpoint information
  }

-- | Stub for UnlinkedBCO - an unlinked bytecode object
data UnlinkedBCO = UnlinkedBCO

-- | Get the free names from an UnlinkedBCO - stub always returns empty set
bcoFreeNames :: UnlinkedBCO -> UniqDSet Name
bcoFreeNames _ = emptyUniqDSet

-- | Stub for InternalModBreaks - internal breakpoint info per module
data InternalModBreaks = InternalModBreaks

-- | Stub for ModBreaks - breakpoint info per module
data ModBreaks = ModBreaks

-- | Stub for BreakInfoIndex - index into break info array
type BreakInfoIndex = Int

-- -----------------------------------------------------------------------------
-- Template Haskell Types
-- These are stubs for types from ghci:GHCi.Message

-- | Stub for GHCi's THMessage - Template Haskell messages
data THMessage a = THMsg

-- | Stub for GHCi's THResultType - types of TH results
data THResultType
  = THAnnWrapper
  | THExp
  | THPat
  | THType
  | THDec
  | THAnnProvenance

-- -----------------------------------------------------------------------------
-- Eval Types
-- These are stubs for types used in GHC.Runtime.Eval

-- | Stub for EvalExpr - an expression to evaluate
type EvalExpr a = a

-- | Stub for ResumeContext - context for resuming evaluation
type ResumeContext a = a

-- | Stub for EvalStep - stepping mode for debugger
data EvalStep
  = EvalStepSingle
  | EvalStepOut
  | EvalStepNone

-- -----------------------------------------------------------------------------
-- StgToJS Types
-- These are stubs for types from ghc:GHC.StgToJS.* modules

-- | Stub for GHC.StgToJS.Linker.Types.LinkPlan
data LinkPlan = LinkPlan

-- | Stub for GHC.StgToJS.Types.StgToJSConfig
data StgToJSConfig = StgToJSConfig

-- -----------------------------------------------------------------------------
-- Linker Environment Types
-- These are stubs for types used in GHC.Linker.Types

-- | Stub for ItblEnv - info table environment
-- In full builds, this maps Names to info table pointers
type ItblEnv = NameEnv (Name, ())

-- | Stub for AddrEnv - address environment
-- In full builds, this maps Names to addresses
type AddrEnv = NameEnv (Name, ())
