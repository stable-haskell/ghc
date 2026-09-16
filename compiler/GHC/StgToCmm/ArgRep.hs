-----------------------------------------------------------------------------
--
-- Argument representations used in GHC.StgToCmm.Layout.
--
-- (c) The University of Glasgow 2013
--
-----------------------------------------------------------------------------


module GHC.StgToCmm.ArgRep (
        ArgRep(..), toArgRep, toArgRepOrV, argRepSizeW,

        argRepString, argRepSuffix, argRepVecWidth, isNonV, idArgRep,

        slowCallPattern,

        -- * Generic apply tables (shared with GHC.StgToCmm.AutoApply)
        applyTypes, stackApplyTypes, mkApplyName,

        ) where

import GHC.Prelude
import GHC.Platform

import GHC.StgToCmm.Closure    ( idPrimRep1 )
import GHC.Runtime.Heap.Layout ( WordOff )
import GHC.Types.Id            ( Id )
import GHC.Core.TyCon          ( PrimRep(..), PrimOrVoidRep(..), primElemRepSizeB )
import GHC.Types.Basic         ( RepArity )
import GHC.Settings.Constants  ( wORD64_SIZE, dOUBLE_SIZE )

import GHC.Utils.Outputable
import GHC.Data.FastString

import Data.Char ( toLower )

-- I extricated this code as this new module in order to avoid a
-- cyclic dependency between GHC.StgToCmm.Layout and GHC.StgToCmm.Ticky.
--
-- NSF 18 Feb 2013

-------------------------------------------------------------------------
--      Classifying arguments: ArgRep
-------------------------------------------------------------------------

-- ArgRep is re-exported by GHC.StgToCmm.Layout, but only for use in the
-- byte-code generator which also needs to know about the
-- classification of arguments.

data ArgRep = P   -- GC Ptr
            | N   -- Word-sized non-ptr
            | L   -- 64-bit non-ptr (long)
            | V   -- Void
            | F   -- Float
            | D   -- Double
            | V16 -- 16-byte (128-bit) vectors of Float/Double/Int8/Word32/etc.
            | V32 -- 32-byte (256-bit) vectors of Float/Double/Int8/Word32/etc.
            | V64 -- 64-byte (512-bit) vectors of Float/Double/Int8/Word32/etc.
            deriving ( Eq, Ord )
instance Outputable ArgRep where ppr = text . argRepString

argRepString :: ArgRep -> String
argRepString P = "P"
argRepString N = "N"
argRepString L = "L"
argRepString V = "V"
argRepString F = "F"
argRepString D = "D"
argRepString V16 = "V16"
argRepString V32 = "V32"
argRepString V64 = "V64"

-- | Lower-case name of an 'ArgRep' as used in the names of the RTS generic
-- apply routines: @stg_ap_pp_fast@, @stg_ap_v16@, @stg_ap_stk_ppp@, ...
argRepSuffix :: ArgRep -> String
argRepSuffix = map toLower . argRepString

-- | Width in bytes of a vector 'ArgRep', 'Nothing' for the scalar ones.
argRepVecWidth :: ArgRep -> Maybe Int
argRepVecWidth = \case
  V16 -> Just 16
  V32 -> Just 32
  V64 -> Just 64
  _   -> Nothing

-- | Name of the generic apply routine for a list of argument
-- representations, e.g. @stg_ap_ppv@.  The @_fast@, @_info@ and @_ret@
-- variants are formed by appending the corresponding suffix.
mkApplyName :: [ArgRep] -> String
mkApplyName args = "stg_ap_" ++ concatMap argRepSuffix args

toArgRep :: Platform -> PrimRep -> ArgRep
toArgRep platform rep = case rep of
   BoxedRep _        -> P
   IntRep            -> N
   WordRep           -> N
   Int8Rep           -> N  -- Gets widened to native word width for calls
   Word8Rep          -> N  -- Gets widened to native word width for calls
   Int16Rep          -> N  -- Gets widened to native word width for calls
   Word16Rep         -> N  -- Gets widened to native word width for calls
   Int32Rep          -> N  -- Gets widened to native word width for calls
   Word32Rep         -> N  -- Gets widened to native word width for calls
   AddrRep           -> N
   Int64Rep          -> case platformWordSize platform of
                           PW4 -> L
                           PW8 -> N
   Word64Rep         -> case platformWordSize platform of
                           PW4 -> L
                           PW8 -> N
   FloatRep          -> F
   DoubleRep         -> D
   (VecRep len elem) -> case len*primElemRepSizeB platform elem of
                           16 -> V16
                           32 -> V32
                           64 -> V64
                           _  -> error "toArgRep: bad vector primrep"

toArgRepOrV :: Platform -> PrimOrVoidRep -> ArgRep
toArgRepOrV _ VoidRep = V
toArgRepOrV platform (NVRep rep) = toArgRep platform rep

isNonV :: ArgRep -> Bool
isNonV V = False
isNonV _ = True

argRepSizeW :: Platform -> ArgRep -> WordOff -- Size in words
argRepSizeW platform = \case
   N   -> 1
   P   -> 1
   F   -> 1
   L   -> wORD64_SIZE `quot` ws
   D   -> dOUBLE_SIZE `quot` ws
   V   -> 0
   V16 -> 16          `quot` ws
   V32 -> 32          `quot` ws
   V64 -> 64          `quot` ws
  where
   ws       = platformWordSizeInBytes platform

idArgRep :: Platform -> Id -> ArgRep
idArgRep platform = toArgRepOrV platform . idPrimRep1

-- The patterns matched here must be exactly those in 'applyTypes', for
-- which the RTS has generic apply routines; see Note [Generic apply tables].

slowCallPattern :: [ArgRep] -> (FastString, RepArity)
-- Returns the generic apply function and arity
--
-- The first batch of cases match (some) specialised entries
-- The last group deals exhaustively with the cases for the first argument
--   (and the zero-argument case)
--
-- In 99% of cases this function will match *all* the arguments in one batch

slowCallPattern (P: P: P: P: P: P: _) = (fsLit "stg_ap_pppppp", 6)
slowCallPattern (P: P: P: P: P: _)    = (fsLit "stg_ap_ppppp", 5)
slowCallPattern (P: P: P: P: _)       = (fsLit "stg_ap_pppp", 4)
slowCallPattern (P: P: P: V: _)       = (fsLit "stg_ap_pppv", 4)
slowCallPattern (P: P: P: _)          = (fsLit "stg_ap_ppp", 3)
slowCallPattern (P: P: V: _)          = (fsLit "stg_ap_ppv", 3)
slowCallPattern (P: P: _)             = (fsLit "stg_ap_pp", 2)
slowCallPattern (P: V: _)             = (fsLit "stg_ap_pv", 2)
slowCallPattern (P: _)                = (fsLit "stg_ap_p", 1)
slowCallPattern (V: _)                = (fsLit "stg_ap_v", 1)
slowCallPattern (N: _)                = (fsLit "stg_ap_n", 1)
slowCallPattern (F: _)                = (fsLit "stg_ap_f", 1)
slowCallPattern (D: _)                = (fsLit "stg_ap_d", 1)
slowCallPattern (L: _)                = (fsLit "stg_ap_l", 1)
slowCallPattern (V16: _)              = (fsLit "stg_ap_v16", 1)
slowCallPattern (V32: _)              = (fsLit "stg_ap_v32", 1)
slowCallPattern (V64: _)              = (fsLit "stg_ap_v64", 1)
slowCallPattern []                    = (fsLit "stg_ap_0", 0)

-------------------------------------------------------------------------
--      The generic apply tables
-------------------------------------------------------------------------

-- Note [Generic apply tables]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The RTS contains pre-generated generic application code for a fixed set
-- of argument patterns (see GHC.StgToCmm.AutoApply, which generates
-- rts/AutoApply.cmm).  The two tables below are the single source of truth
-- for that set:
--
--  * 'applyTypes' lists the patterns for which the RTS has an
--    @stg_ap_<pat>_fast@ entry point and an @stg_ap_<pat>@ return frame.
--    Unknown calls whose argument pattern is not in the table are split
--    into a chain of calls from the table; see 'slowCallPattern' and
--    GHC.StgToCmm.Layout.slowArgs.
--
--  * 'stackApplyTypes' lists the argument-descriptor patterns for which
--    the RTS has @stg_ap_stk_<pat>@ and @stg_stk_save_<pat>@ routines.
--    Its order is load-bearing: the index of a pattern in the table, plus
--    the three generic descriptors ARG_GEN, ARG_GEN_BIG and ARG_BCO, is
--    the ARG_* value in rts/include/rts/storage/FunTypes.h, and indexes
--    the stg_ap_stack_entries, stg_stack_save_entries and stg_arg_bitmaps
--    arrays generated by GHC.StgToCmm.AutoApply.
--    GHC.StgToCmm.Layout.stdPattern picks the ARG_* value for a function.
--
-- Changing either table requires regenerating nothing by hand, but the
-- RTS declarations must follow: the RTS_RET/RTS_FUN_DECL lines in
-- rts/include/stg/MiscClosures.h, the SymI_HasProto lines in
-- rts/RtsSymbols.c, the ARG_* constants in FunTypes.h, and the ticky
-- counters (SLOW_CALL_*_ctr in rts/include/stg/Ticky.h, TICK_SLOW_CALL_*
-- in rts/include/Cmm.h, PR_CTR in rts/Ticky.c).

-- | Argument patterns with a generic apply routine in the RTS.
-- These have been shown to cover about 99% of cases in practice...
applyTypes :: [[ArgRep]]
applyTypes = [
        [V],
        [F],
        [D],
        [L],
        [V16],
        [V32],
        [V64],
        [N],
        [P],
        [P,V],
        [P,P],
        [P,P,V],
        [P,P,P],
        [P,P,P,V],
        [P,P,P,P],
        [P,P,P,P,P],
        [P,P,P,P,P,P]
   ]

-- | Argument-descriptor patterns with stack-apply and stack-save routines
-- in the RTS, in ARG_* order.  See Note [Generic apply tables].
--
-- No need for V args in the stack apply cases.
-- ToDo: the stack apply and stack save code doesn't make a distinction
-- between N and P (they both live in the same register), only the bitmap
-- changes, so we could share the apply/save code between lots of cases.
stackApplyTypes :: [[ArgRep]]
stackApplyTypes = [
        [],
        [N],
        [P],
        [F],
        [D],
        [L],
        [V16],
        [V32],
        [V64],
        [N,N],
        [N,P],
        [P,N],
        [P,P],
        [N,N,N],
        [N,N,P],
        [N,P,N],
        [N,P,P],
        [P,N,N],
        [P,N,P],
        [P,P,N],
        [P,P,P],
        [P,P,P,P],
        [P,P,P,P,P],
        [P,P,P,P,P,P],
        [P,P,P,P,P,P,P],
        [P,P,P,P,P,P,P,P]
   ]
