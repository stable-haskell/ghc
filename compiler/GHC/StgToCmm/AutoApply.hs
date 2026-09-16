{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
--
-- Generating the RTS generic application code (rts/AutoApply.cmm)
--
-- (c) The University of Glasgow 2004-2026
--
-----------------------------------------------------------------------------

-- | Generate the generic application code of the RTS: the @stg_ap_*_fast@
-- entry points and @stg_ap_*@ return frames used for unknown calls, and the
-- @stg_ap_stk_*@ / @stg_stk_save_*@ routines used by @stg_PAP_apply@ and
-- the heap-check failure code.
--
-- The generated code is Cmm source text, written by @ghc --gen-apply@ into
-- @rts/AutoApply.cmm@ (and the vector variants, see
-- Note [AutoApply.cmm for vectors]) when the RTS is built.
--
-- Everything the generator knows about the target comes from the
-- 'Platform': word size, the argument registers (through
-- 'GHC.Cmm.CallConv.assignArgumentsPos', the same function the code
-- generator uses for call sites) and the pointer-tagging and bitmap
-- constants; see Note [Target information for the generic apply code].
module GHC.StgToCmm.AutoApply
  ( genAutoApply
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Profile
import GHC.Platform.Tag

import GHC.Cmm.Node ( Convention(..) )
import GHC.Cmm.CallConv
import GHC.Cmm.Reg
import GHC.Cmm.Type
import GHC.StgToCmm.ArgRep

import GHC.Utils.Outputable ( SDoc, showSDocUnsafe )
import GHC.Utils.Panic
import GHC.Utils.Ppr

import Data.List          ( intercalate, intersperse, nub, sort )
import Data.Maybe         ( isNothing, mapMaybe )
import Data.Word          ( Word32 )
import qualified Data.Set as Set

{- Note [Target information for the generic apply code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The generic apply code is specialised to the target: how many argument
registers there are (and hence which arguments of stg_ap_pppppp are loaded
into registers and which stay on the stack), the word size (a Double takes
two stack words on a 32-bit target), the number of pointer tag bits (which
decides which arities get a tagged fast path) and the layout of small
bitmaps.

All of this is available from the compiler's 'Platform':

  * register assignment is done by GHC.Cmm.CallConv.assignArgumentsPos,
    exactly as for the compiled call sites that jump to this code, so the
    two cannot disagree;

  * the constants come from 'platformConstants', which GHC reads from the
    RTS's DerivedConstants.h header (see Note [Platform constants] in
    GHC.Platform).  This is why "ghc --gen-apply" must be able to find that
    header: when building the RTS itself, GHC looks in the -I include
    directories (see GHC.Unit.State.initUnits), which is what Hadrian
    arranges.

Historically this generator was a standalone program (utils/genapply) that
re-implemented the register assignment and parsed the constants from
comment lines that deriveConstants wrote into DerivedConstants.h for its
benefit.  Generating the code from within GHC removes that duplication and
the associated cross-compilation hazards (#24347).

Note [AutoApply.cmm for vectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generating and compiling stg_ap functions for vectors (e.g. stg_ap_v16_fast)
is quite tricky. The build platform generates Cmm code for the target platform,
and the requirements are:

  1. the host platform must be able to compile this code,
  2. the target platform must be able to:
      a. run this code when it passes the appropriate CPU flags such as -mavx
      b. not run into any problems in other code that does not use vector registers.

This is achieved by several means. The first step is to use CPP in the generated
Cmm code. Specifically, any mention of XMM/YMM/ZMM registers is wrapped
in a conditional. For example, stg_stk_save_v32 looks like:

    stg_stk_save_v32
    {
        #if defined(REG_YMM1)
        Sp_adj(-7);
        V32_[Sp+WDS(3)] = YMM1;
        ...
        #else
        ccall sbarf("stg_stk_save_v32: unsupported vector register") never returns;
        #endif
    }

This means that:

  - the RTS unconditionally defines symbols such as 'stg_ap_v32_fast',
  - but they may throw an error at runtime if vector registers are not supported.

This is a lot more straightforward than attempting to defer the generation of
this code. In particular, GHC.StgToCmm.ArgRep.slowCallPattern currently assumes
that 1-argument stg_ap functions exist for all representations, and revisiting
that design would constitute a fair amount of work.

Case in point: this CPP allows the generated application code to be compiled on
AArch64, for which REG_XMM1 is defined but both REG_YMM1 and REG_ZMM1 are not.
Note that GHC will complain at compile-time if we try to use 256/512 bit wide
vectors on AArch64, so it is immaterial that stg_ap_v32_fast will crash at runtime.

How about on X86_64? There are several points to consider:

  - The X86 NCG only partially supports vector registers; most operations
    only work on 128-bit wide vectors.

    However all that is needed is support for MOV instructions to perform the
    stack save/load; so it suffices to have (working register allocation and)
    support for move instructions for XMM/YMM/ZMM;
    see GHC.CmmToAsm.X86.Instr.movInstr.

  - For XMM load/stores, we can't assume that the target supports AVX, only SSE2.
    So we have to pessimistically emit SSE2 instructions instead of AVX instructions.
    On the other hand, YMM load/stores require -mavx2/-mavx512f, respectively.

    To do this, we must compile e.g stg_ap_v16_fast WITHOUT -mavx.
    (Were we to compile with -mavx, we would emit assembly containing AVX
    instructions, and these might not be available on the target platform.)

  - For YMM/ZMM load/stores, we must emit the appropriate AVX2/AVX512F
    instructions (as GHC.CmmToAsm.X86.Instr.movInstr does).

This means that:

  - stg_ap_v16_fast (and friends) should be compiled WITHOUT -mavx.
    (Were we to compile with -mavx, we would emit assembly containing AVX
    instructions, and these might not be available on the target platform.)
  - stg_ap_v32_fast (and friends) should be compiled with -mavx2.
  - stg_ap_v64_fast (and friends) should be compiled with -mavx512f.

However, there isn't currently a way to set CPU flags per function in Cmm, à la

  __attribute__(("target"="avx2"))
  stg_ap_v32_fast ...

Instead, we put all V16 code in AutoApply_V16.cmm, all V32 code into
AutoApply_V32.cmm, and all V64 code in AutoApply_V64.cmm.
On X86, we then compile AutoApply_V32.cmm with -mavx2, and AutoApply_V64.cmm
with -mavx512f. See references to AutoApply in Hadrian, Settings/Packages.hs.

Note that it is very important to set these flags. For example, were we to
compile AutoApply_V32.cmm without -mavx2 using the LLVM backend, LLVM would
attempt to compile usage of ymm registers into usage of pairs of xmm registers.
This violates the expected calling convention, and leads to segfaults
(e.g. in test T25062_V32).

See also Note [realArgRegsCover] in GHC.Cmm.CallConv, which deals with similar
concerns.
-}

-- -----------------------------------------------------------------------------
-- Argument kinds

-- size of a value in *words*
argSize :: Platform -> ArgRep -> Int
argSize = argRepSizeW

-- is a value a pointer?
isPtr :: ArgRep -> Bool
isPtr P = True
isPtr _ = False

-- | The 'CmmType' of an argument, for the purposes of register assignment.
-- Only the width and the category (float/vector/other) matter.
argRepCmmType :: Platform -> ArgRep -> CmmType
argRepCmmType platform = \case
  P   -> gcWord platform
  N   -> bWord platform
  L   -> b64
  F   -> f32
  D   -> f64
  V16 -> cmmVec 16 b8
  V32 -> cmmVec 32 b8
  V64 -> cmmVec 64 b8
  V   -> panic "GHC.StgToCmm.AutoApply.argRepCmmType: void argument"

-- | The widest vector among the arguments, if any.
largestVec :: [ArgRep] -> Maybe Int
largestVec reps = case mapMaybe argRepVecWidth reps of
  []     -> Nothing
  w : ws -> Just (foldr max w ws)

-- -----------------------------------------------------------------------------
-- Registers

-- | The Cmm name of an argument register: @R2@, @F1@, @D1@, @L1@, @XMM1@, ...
regName :: GlobalReg -> String
regName reg = showSDocUnsafe (pprGlobalReg reg :: SDoc)

-- | The Cmm type prefix for loading from/storing to the stack a value that
-- lives in the given register.
regRep :: GlobalReg -> String
regRep = \case
  FloatReg  {} -> "F_"
  DoubleReg {} -> "D_"
  LongReg   {} -> "L_"
  XmmReg    {} -> "V16_"
  YmmReg    {} -> "V32_"
  ZmmReg    {} -> "V64_"
  _            -> "W_"

isVectorReg :: GlobalReg -> Bool
isVectorReg = \case
  XmmReg {} -> True
  YmmReg {} -> True
  ZmmReg {} -> True
  _         -> False

-- -----------------------------------------------------------------------------
-- Loading/saving register arguments to the stack

loadRegArgs :: Platform -> Int -> [ArgRep] -> (Doc,Int)
loadRegArgs platform sp args = (loadRegOffs reg_locs, sp')
  where (reg_locs, _, sp') = assignRegs platform sp args

loadRegOffs :: [(GlobalReg,Int)] -> Doc
loadRegOffs reg_locs =
  vcat $ map (uncurry assign_stk_to_reg) reg_locs

saveRegOffs :: [(GlobalReg,Int)] -> Doc
saveRegOffs reg_locs =
  vcat $ map (uncurry assign_reg_to_stk) reg_locs

-- | Assign registers to the arguments of a generic apply routine.
--
-- The arguments live on the stack, starting at the given @Sp@ offset (in
-- words).  Registers are handed out by 'assignArgumentsPos' with the
-- 'NativeDirectCall' convention, i.e. exactly as for a call to a known
-- function whose node is already in R1, so the fast paths of the stg_ap
-- routines agree with the code the compiler generates for the callee.
-- Register assignment stops at the first argument that does not fit in a
-- register; that argument and all following ones are left on the stack.
--
-- Void arguments take no register and no stack space.
assignRegs
        :: Platform
        -> Int                  -- Sp of first arg
        -> [ArgRep]             -- args
        -> ([(GlobalReg,Int)],  -- regs and offsets to load
                                --   (in reverse argument order)
            [ArgRep],           -- left-over args
            Int)                -- Sp of left-over args
assignRegs platform sp args = (reverse reg_locs, leftovers, sp')
  where
    indexed_args = zip [0 :: Int ..] args

    (_, assts) = assignArgumentsPos (Profile platform Set.empty) 0 NativeDirectCall
                   (argRepCmmType platform . snd)
                   [ ia | ia@(_, rep) <- indexed_args, isNonV rep ]

    reg_of i = case [ r | ((i', _), RegisterParam r) <- assts, i' == i ] of
                 r : _ -> Just r
                 []    -> Nothing

    (reg_locs, leftovers, sp') = go sp indexed_args

    go off [] = ([], [], off)
    go off ((_, V) : rest) = go off rest
    go off all_args@((i, rep) : rest)
      | Just reg <- reg_of i
      , reg_ok rep reg
      , let (locs, lo, off') = go (off + argSize platform rep) rest
      = ((reg, off) : locs, lo, off')
      | otherwise
      = ([], map snd all_args, off)

    -- An L argument (64-bit non-pointer) goes in a long register or stays
    -- on the stack.  On 32-bit targets, the only ones where 'toArgRep'
    -- produces L, that is what 'assignArgumentsPos' does too.  On 64-bit
    -- targets L is word-sized and would get a vanilla register, but no
    -- code can call the stg_ap_l routines there, so keep them as they
    -- always were rather than changing dead code.
    reg_ok L (VanillaReg {}) = False
    reg_ok _ _               = True

-- | Wrap code that mentions vector registers in a CPP conditional on those
-- registers existing on the target; see Note [AutoApply.cmm for vectors].
vecsCpp :: Doc -> [GlobalReg] -> [Doc] -> [Doc]
vecsCpp fun regs code =
  case filter isVectorReg regs of
    [] -> code
    vs ->
      let cond = text (intercalate " && " [ "defined(REG_" ++ regName r ++ ")" | r <- vs ])
      in [ text "// Guard usage of vector registers"
         , text "#if" <+> cond ]
         ++ code
         ++ [ text "#else //" <+> cond
            , text "ccall sbarf(\"" <> fun <> text ": unsupported vector register\") never returns;"
            , text "#endif //" <+> cond
            ]

assign_reg_to_stk :: GlobalReg -> Int -> Doc
assign_reg_to_stk reg sp
   = loadSpWordOff (regRep reg) sp <> text " = " <> text (regName reg) <> semi

assign_stk_to_reg :: GlobalReg -> Int -> Doc
assign_stk_to_reg reg sp
   = text (regName reg) <> text " = " <> loadSpWordOff (regRep reg) sp <> semi

loadSpWordOff :: String -> Int -> Doc
loadSpWordOff rep off = text rep <> text "[Sp+WDS(" <> int off <> text ")]"

-- Make a jump
mkJump :: Platform
       -> Doc       -- Jump target
       -> [String]  -- Registers that are definitely live
       -> [ArgRep]  -- Jump arguments
       -> Doc
mkJump platform jump live args =
  text "jump" <+> jump <+> brackets (hcat (punctuate comma (map text liveRegs))) <+> semi
  where
    liveRegs = mkJumpLiveRegs platform live args

-- Make a jump, saving CCCS and restoring it on return
mkJumpSaveCCCS :: Platform
               -> Doc       -- Jump target
               -> [String]  -- Registers that are definitely live
               -> [ArgRep]  -- Jump arguments
               -> Doc
mkJumpSaveCCCS platform jump live args =
  text "jump_SAVE_CCCS" <> parens (hcat (punctuate comma (restoreCCCS_info : jump : map text liveRegs))) <+> semi
  where
    liveRegs = mkJumpLiveRegs platform live args
    restoreCCCS_info = text (stgRestoreCCCSInfo args)

stgRestoreCCCSInfo :: [ArgRep] -> String
stgRestoreCCCSInfo args
  = case largestVec args of
      Just 64 -> "stg_restore_cccs_v64_info"
      Just 32 -> "stg_restore_cccs_v32_info"
      Just 16 -> "stg_restore_cccs_v16_info"
      _       -> "stg_restore_cccs_d_info"

-- Calculate live registers for a jump
mkJumpLiveRegs :: Platform
               -> [String]  -- Registers that are definitely live
               -> [ArgRep]  -- Jump arguments
               -> [String]
mkJumpLiveRegs platform live args = (nub . sort) (live ++ map (regName . fst) reg_locs)
  where
    (reg_locs, _, _) = assignRegs platform 0 args

-- make a ptr/non-ptr bitmap from a list of argument types
mkBitmap :: Platform -> [ArgRep] -> Word32
mkBitmap platform args = foldr f 0 args
 where
  f :: ArgRep -> Word32 -> Word32
  f arg bm | isPtr arg = bm `shiftL` 1
           | otherwise = (bm `shiftL` size) .|. ((1 `shiftL` size) - 1)
           where size = argSize platform arg

-- -----------------------------------------------------------------------------
-- Generating the application functions

-- A SUBTLE POINT about stg_ap functions (can't think of a better
-- place to put this comment --SDM):
--
-- The entry convention to an stg_ap_ function is as follows: all the
-- arguments are on the stack (we might revisit this at some point,
-- but it doesn't make any difference on x86), and THERE IS AN EXTRA
-- EMPTY STACK SLOT at the top of the stack.
--
-- Why?  Because in several cases, stg_ap_* will need an extra stack
-- slot, eg. to push a return address in the THUNK case, and this is a
-- way of pushing the stack check up into the caller which is probably
-- doing one anyway.  Allocating the extra stack slot in the caller is
-- also probably free, because it will be adjusting Sp after pushing
-- the args anyway (this might not be true of register-rich machines
-- when we start passing args to stg_ap_* in regs).

mkApplyNameDoc :: [ArgRep] -> Doc
mkApplyNameDoc args = text (mkApplyName args)

mkApplyRetName :: [ArgRep] -> Doc
mkApplyRetName args
  = mkApplyNameDoc args <> text "_ret"

mkApplyFastName :: [ArgRep] -> Doc
mkApplyFastName args
  = mkApplyNameDoc args <> text "_fast"

mkApplyInfoName :: [ArgRep] -> Doc
mkApplyInfoName args
  = mkApplyNameDoc args <> text "_info"

mb_tag_node :: Platform -> Int -> Doc
mb_tag_node platform arity | Just tag <- tagForArity platform arity = mkTagStmt tag <> semi
                           | otherwise = empty

mkTagStmt :: Int -> Doc
mkTagStmt tag = text ("R1 = R1 + " ++ show tag)

type StackUsage = (Int, Int)  -- PROFILING, normal

maxStack :: [StackUsage] -> StackUsage
maxStack []       = panic "GHC.StgToCmm.AutoApply.maxStack: empty list"
maxStack (u : us) = foldr (\(p, n) (p', n') -> (max p p', max n n')) u us

stackCheck
   :: Platform
   -> [ArgRep]
   -> Bool       -- args in regs?
   -> Doc        -- fun_info_label
   -> StackUsage
   -> Doc
stackCheck platform args args_in_regs fun_info_label (prof_sp, norm_sp) =
  let
     (reg_locs, _leftovers, sp_offset) = assignRegs platform 1 args

     cmp_sp n
       | n > 0 =
          text "if (Sp - WDS(" <> int n <> text ") < SpLim) {" $$
          nest 4 (vcat [
            if args_in_regs
               then
                 text "Sp_adj" <> parens (int (-sp_offset)) <> semi $$
                 saveRegOffs reg_locs
               else
                 empty,
            text "Sp(0) = " <> fun_info_label <> char ';',
            mkJump platform (text "__stg_gc_enter_1") ["R1"] []
            ]) $$
          char '}'
       | otherwise = empty
  in
  vcat [ text "#if defined(PROFILING)",
         cmp_sp prof_sp,
         text "#else // defined(PROFILING)",
         cmp_sp norm_sp,
         text "#endif // defined(PROFILING)"
       ]

genMkPAP :: Platform
         -> String    -- Macro
         -> String    -- Jump target
         -> [String]  -- Registers that are definitely live
         -> String    -- Ticker
         -> String    -- Disamb
         -> Bool      -- Don't load argument registers before jump if True
         -> Bool      -- Arguments already in registers if True
         -> Bool      -- Is a PAP if True
         -> [ArgRep]  -- Arguments
         -> Int       -- Size of all arguments
         -> Doc       -- info label
         -> Bool      -- Is a function
         -> (Doc, StackUsage)
genMkPAP platform macro jump live _ticker disamb
        no_load_regs    -- don't load argument regs before jumping
        args_in_regs    -- arguments are already in regs
        is_pap args all_args_size fun_info_label
        is_fun_case
  = (doc, stack_usage)

  where
    doc = vcat smaller_arity_doc $$ exact_arity_case $$ larger_arity_doc

    stack_usage = maxStack (larger_arity_stack : smaller_arity_stack)

    n_args = length args

        -- offset of arguments on the stack at slow apply calls.
    stk_args_slow_offset = 1

    stk_args_offset
        | args_in_regs = 0
        | otherwise    = stk_args_slow_offset

-- The SMALLER ARITY cases:
--      if (arity == 1) {
--          Sp[0] = Sp[1];
--          Sp[1] = (W_)&stg_ap_1_info;
--          JMP_(GET_ENTRY(R1.cl));
    (smaller_arity_doc, smaller_arity_stack)
       = unzip [ smaller_arity i | i <- [1..n_args-1] ]

    smaller_arity arity = (smaller_doc, smaller_stack_usage)
      where
        (save_regs, smaller_stack_usage)
          | overflow_regs = save_extra_regs
          | otherwise     = shuffle_extra_args

        smaller_doc =
           text "if (arity == " <> int arity <> text ") {" $$
           nest 4 (vcat [
           --  text "TICK_SLOW_CALL_" <> text ticker <> text "_TOO_MANY();",

                -- load up regs for the call, if necessary
             load_regs,

                -- If we have more args in registers than are required
                -- for the call, then we must save some on the stack,
                -- and set up the stack for the follow-up call.
                -- If the extra arguments are on the stack, then we must
                -- instead shuffle them down to make room for the info
                -- table for the follow-on call.
             save_regs,

                -- for a PAP, we have to arrange that the stack contains a
                -- return address in the event that stg_PAP_entry fails its
                -- heap check.  See stg_PAP_entry in Apply.cmm for details.
             if is_pap
                then text "R2 = " <> mkApplyInfoName this_call_args <> semi

                else empty,
            if is_fun_case then mb_tag_node platform arity else empty,
            if overflow_regs
                then mkJumpSaveCCCS platform
                       (text jump) live (take arity args)
                else mkJump platform (text jump) live (if no_load_regs then [] else args)
            ]) $$
           text "}"

           -- offsets in case we need to save regs:
        (reg_locs, _, _)
           = assignRegs platform stk_args_offset args

           -- register assignment for *this function call*
        (reg_locs', reg_call_leftovers, reg_call_sp_stk_args)
           = assignRegs platform stk_args_offset (take arity args)

        load_regs
           | no_load_regs || args_in_regs = empty
           | otherwise                    = loadRegOffs reg_locs'

        (this_call_args, rest_args) = splitAt arity args

           -- the offset of the stack args from initial Sp
        sp_stk_args
           | args_in_regs = stk_args_offset
           | no_load_regs = stk_args_offset
           | otherwise    = reg_call_sp_stk_args

           -- the stack args themselves
        this_call_stack_args
           | args_in_regs = reg_call_leftovers -- sp offsets are wrong
           | no_load_regs = this_call_args
           | otherwise    = reg_call_leftovers

        stack_args_size = sum (map (argSize platform) this_call_stack_args)

        overflow_regs = args_in_regs && length reg_locs > length reg_locs'

        save_extra_regs = (save_extra_doc, (size,size))
          where
             -- we have extra arguments in registers to save
              extra_reg_locs = drop (length reg_locs') (reverse reg_locs)
              adj_reg_locs = [ (reg, off - adj + 1) |
                               (reg,off) <- extra_reg_locs ]
              adj = case extra_reg_locs of
                      (_reg, fst_off):_ -> fst_off
                      [] -> panic "GHC.StgToCmm.AutoApply.genMkPAP: no extra register locations"
              size = snd (last adj_reg_locs) + 1

              save_extra_doc =
                text "Sp_adj(" <> int (-size) <> text ");" $$
                saveRegOffs adj_reg_locs $$
                loadSpWordOff "W_" 0 <> text " = " <>
                             mkApplyInfoName rest_args <> semi

        shuffle_extra_args = (shuffle_extra_doc, (shuffle_prof_stack, shuffle_norm_stack))
          where
           shuffle_extra_doc =
            vcat [ text "#if defined(PROFILING)"
                 , shuffle_prof_doc
                 , text "#else // defined(PROFILING)"
                 , shuffle_norm_doc
                 , text "#endif // defined(PROFILING)"
                 ]

           (shuffle_prof_doc, shuffle_prof_stack) = shuffle True
           (shuffle_norm_doc, shuffle_norm_stack) = shuffle False

           -- Sadly here we have to insert an stg_restore_cccs frame
           -- just underneath the stg_ap_*_info frame if we're
           -- profiling; see Note [jump_SAVE_CCCS]
           shuffle prof = (shuffle_doc, -sp_adj)
             where
             sp_adj = sp_stk_args - 1 - offset
             offset = if prof then 2 else 0
             shuffle_doc =
               vcat (map (shuffle_down (offset + 1))
                      [sp_stk_args .. sp_stk_args + stack_args_size - 1]) $$
               (if prof
                 then
                   loadSpWordOff "W_" (sp_stk_args + stack_args_size - 3)
                     <> text " = " <> text (stgRestoreCCCSInfo args) <> semi $$
                   loadSpWordOff "W_" (sp_stk_args + stack_args_size - 2)
                     <> text " = CCCS;"
                 else empty) $$
               loadSpWordOff "W_" (sp_stk_args + stack_args_size-1)
                     <> text " = "
                     <> mkApplyInfoName rest_args <> semi $$
               text "Sp_adj(" <> int sp_adj <> text ");"

        shuffle_down j i =
             loadSpWordOff "W_" (i-j) <> text " = " <>
             loadSpWordOff "W_" i <> semi


-- The EXACT ARITY case
--
--      if (arity == 1) {
--          Sp++;
--          JMP_(GET_ENTRY(R1.cl));

    exact_arity_case
        = text "if (arity == " <> int n_args <> text ") {" $$
          let
             (reg_doc, sp')
                | no_load_regs || args_in_regs = (empty, stk_args_offset)
                | otherwise    = loadRegArgs platform stk_args_offset args
          in
          nest 4 (vcat [
--          text "TICK_SLOW_CALL_" <> text ticker <> text "_CORRECT();",
            reg_doc,
            text "Sp_adj(" <> int sp' <> text ");",
            if is_pap
                then text "R2 = " <> fun_info_label <> semi
                else empty,
            if is_fun_case then mb_tag_node platform n_args else empty,
            mkJump platform (text jump) live (if no_load_regs then [] else args)
          ])

-- The LARGER ARITY cases:
--
--      } else /* arity > 1 */ {
--          BUILD_PAP(1,0,(W_)&stg_ap_v_info);
--      }

    (larger_arity_doc, larger_arity_stack) = (larger_doc, stack)
     where
       -- offsets in case we need to save regs:
       (reg_locs, _leftovers, sp_offset)
           = assignRegs platform stk_args_slow_offset args
           -- BUILD_PAP assumes args start at offset 1

       stack | args_in_regs = (sp_offset, sp_offset)
             | otherwise    = (0,0)

       larger_doc =
           text "} else {" $$
           let
             save_regs
                | args_in_regs =
                        text "Sp_adj(" <> int (-sp_offset) <> text ");" $$
                        saveRegOffs  reg_locs
                | otherwise =
                        empty
           in
           nest 4 (vcat [
--              text "TICK_SLOW_CALL_" <> text ticker <> text "_TOO_FEW();",
                save_regs,
                -- Before building the PAP, tag the function closure pointer
                if is_fun_case then
                  vcat [
                     text "if (arity < " <> int (tagBitsMax platform) <> text ") {",
                     text "  R1 = R1 + arity" <> semi,
                     text "}"
                   ]
                  else empty
                ,
                text macro <> char '(' <> int n_args <> comma <>
                                        int all_args_size <>
                                        text "," <> fun_info_label <>
                                        text "," <> text disamb <>
                                        text ");"
           ]) $$
           char '}'


-- Note [jump_SAVE_CCCS]
-- ~~~~~~~~~~~~~~~~~~~~~
-- When profiling, if we have some extra arguments to apply that we
-- save to the stack, we must also save the current cost centre stack
-- and restore it when applying the extra arguments.  This is all
-- handled by the macro jump_SAVE_CCCS(target), defined in
-- rts/AutoApply.h.
--
-- At the jump, the stack will look like this:
--
--      ... extra args ...
--      stg_ap_pp_info
--      CCCS
--      stg_restore_cccs_info

-- --------------------------------------
-- Examine tag bits of function pointer and enter it
-- directly if needed.
-- TODO: remove the redundant case in the original code.
enterFastPath :: Platform -> Bool -> Bool -> [ArgRep] -> Doc
enterFastPath platform no_load_regs args_in_regs args
    | Just tag <- tagForArity platform (length args)
    = enterFastPathHelper platform tag no_load_regs args_in_regs args
enterFastPath _ _ _ _ = empty

-- | The pointer tag of a function of the given arity, if the arity is small
-- enough to be encoded in the tag bits (cf. GHC.StgToCmm.Closure.tagForArity).
tagForArity :: Platform -> Int -> Maybe Int
tagForArity platform i | isSmallFamily platform i = Just i
                       | otherwise                = Nothing

-- | One more than the largest pointer tag, i.e. @2^TAG_BITS@.
tagBitsMax :: Platform -> Int
tagBitsMax platform = fromDynTag (mAX_PTR_TAG platform) + 1

enterFastPathHelper :: Platform
                    -> Int
                    -> Bool
                    -> Bool
                    -> [ArgRep]
                    -> Doc
enterFastPathHelper platform tag no_load_regs args_in_regs args =
  text "if (GETTAG(R1)==" <> int tag <> text ") {" $$
  nest 4 (vcat [
    reg_doc,
    text "Sp_adj(" <> int sp' <> text ");",
    -- enter, but adjust offset with tag
    mkJump platform (text "%GET_ENTRY(R1-" <> int tag <> text ")") ["R1"] args
  ]) $$
  text "}"
  -- I don't totally understand this code, I copied it from
  -- exact_arity_case
  -- TODO: refactor
    where
        -- offset of arguments on the stack at slow apply calls.
    stk_args_slow_offset = 1

    stk_args_offset
        | args_in_regs = 0
        | otherwise    = stk_args_slow_offset

    (reg_doc, sp')
        | no_load_regs || args_in_regs = (empty, stk_args_offset)
        | otherwise    = loadRegArgs platform stk_args_offset args

tickForArity :: Platform -> Int -> Doc
tickForArity _platform _arity = empty
{-
    | Just tag <- tagForArity platform arity
    = vcat [
            text "W_[TOTAL_CALLS] = W_[TOTAL_CALLS] + 1;",
            text "W_[SLOW_CALLS_" <> int arity <> text "] = W_[SLOW_CALLS_" <> int arity <> text "] + 1;",
            text "if (TO_W_(StgFunInfoExtra_arity(%FUN_INFO(%INFO_PTR(UNTAG(R1))))) == " <> int arity <> text " ) {",
            text "  W_[RIGHT_ARITY_" <> int arity <> text "] = W_[RIGHT_ARITY_" <> int arity <> text "] + 1;",
            text "  if (GETTAG(R1)==" <> int tag <> text ") {",
            text "    W_[TAGGED_PTR_" <> int arity <> text "] = W_[TAGGED_PTR_" <> int arity <> text "] + 1;",
            text "  } else {",
            -- force a halt when not tagged!
--          text "    W_[0]=0;",
            text "  }",
            text "}"
          ]
tickForArity _ _ = text "W_[TOTAL_CALLS] = W_[TOTAL_CALLS] + 1;"
-}

-- -----------------------------------------------------------------------------
-- generate an apply function

-- args is a list of 'p', 'n', 'f', 'd' or 'l'
formalParam :: ArgRep -> Int -> Doc
formalParam V _ = empty
formalParam arg n =
    formalParamType arg <> space <>
    text "arg" <> int n <> text ", "

formalParamType :: ArgRep -> Doc
formalParamType arg = argRep arg

argRep :: ArgRep -> Doc
argRep F   = text "F_"
argRep D   = text "D_"
argRep L   = text "L_"
argRep P   = text "gcptr"
argRep V16 = text "V16_"
argRep V32 = text "V32_"
argRep V64 = text "V64_"
argRep _   = text "W_"

genApply :: Platform -> [ArgRep] -> Doc
genApply platform args =
   let
    fun_ret_label  = mkApplyRetName args
    fun_info_label = mkApplyInfoName args
    all_args_size  = sum (map (argSize platform) args)

    (bco_doc, bco_stack) =
       genMkPAP platform "BUILD_PAP" "ENTRY_LBL(stg_BCO)" ["R1"] "FUN" "BCO"
             True{-stack apply-} False{-args on stack-} False{-not a PAP-}
             args all_args_size fun_info_label {- tag stmt -}False

    (fun_doc, fun_stack) =
       genMkPAP platform "BUILD_PAP" "%GET_ENTRY(UNTAG(R1))" ["R1"] "FUN" "FUN"
             False{-reg apply-} False{-args on stack-} False{-not a PAP-}
             args all_args_size fun_info_label {- tag stmt -}True

    (pap_doc, pap_stack) =
       genMkPAP platform "NEW_PAP" "stg_PAP_apply" ["R1", "R2"] "PAP" "PAP"
             True{-stack apply-} False{-args on stack-} True{-is a PAP-}
             args all_args_size fun_info_label {- tag stmt -}False

    stack_usage = maxStack [bco_stack, fun_stack, pap_stack]
    applyName = mkApplyNameDoc args
    (regsOffs,_,_) =assignRegs platform 1 args
    regs = map fst regsOffs
   in
    vcat [
      text "INFO_TABLE_RET(" <> applyName <> text ", " <>
        text "RET_SMALL, W_ info_ptr, " <> (cat $ zipWith formalParam args [1..]) <>
        text ")\n{",
      nest 4 (vcat $ vecsCpp applyName regs [
       text "W_ _unused;",
       text "W_ info;",
       text "W_ arity;",
       text "unwind Sp = Sp + WDS(" <> int (1 + all_args_size) <> text ");",

--    if fast == 1:
--        print "static void *lbls[] ="
--        print "  { [FUN]             &&fun_lbl,"
--        print "    [FUN_1_0]         &&fun_lbl,"
--        print "    [FUN_0_1]        &&fun_lbl,"
--        print "    [FUN_2_0]        &&fun_lbl,"
--        print "    [FUN_1_1]        &&fun_lbl,"
--        print "    [FUN_0_2]        &&fun_lbl,"
--        print "    [FUN_STATIC]      &&fun_lbl,"
--        print "    [PAP]             &&pap_lbl,"
--        print "    [THUNK]           &&thunk_lbl,"
--        print "    [THUNK_1_0]              &&thunk_lbl,"
--        print "    [THUNK_0_1]              &&thunk_lbl,"
--        print "    [THUNK_2_0]              &&thunk_lbl,"
--        print "    [THUNK_1_1]              &&thunk_lbl,"
--        print "    [THUNK_0_2]              &&thunk_lbl,"
--        print "    [THUNK_STATIC]    &&thunk_lbl,"
--        print "    [THUNK_SELECTOR]  &&thunk_lbl,"
--        print "    [IND]            &&ind_lbl,"
--        print "    [IND_STATIC]      &&ind_lbl,"
--        print "  };"

       tickForArity platform (length args),
       text "",
       text "IF_DEBUG(apply,foreign \"C\" debugBelch(\"" <> fun_ret_label <>
          text "... \", NULL); foreign \"C\" printClosure(R1 \"ptr\"));",

       text "IF_DEBUG(sanity,(_unused) = foreign \"C\" checkStackFrame(Sp+WDS(" <> int (1 + all_args_size)
        <> text ")\"ptr\"));",

--       text "IF_DEBUG(sanity,checkStackChunk(Sp+" <> int (1 + all_args_size) <>
--        text ", CurrentTSO->stack + CurrentTSO->stack_size));",

--       text "TICK_SLOW_CALL(" <> int (length args) <> text ");",

       let do_assert [] _ = []
           do_assert (a:as) offset
                | isPtr a   = this : rest
                | otherwise = rest
                where this = text "ASSERT(LOOKS_LIKE_CLOSURE_PTR(Sp("
                                 <> int offset <> text ")));"
                      rest = do_assert as (offset + argSize platform a)
       in
       vcat (do_assert args 1),

       text  "again:",

       -- if pointer is tagged enter it fast!
       enterFastPath platform False False args,

       stackCheck platform args False{-args on stack-}
                  fun_info_label stack_usage,

       -- Functions can be tagged, so we untag them!
       text  "R1 = UNTAG(R1);",
       text  "info = %INFO_PTR(R1);",

--    if fast == 1:
--        print "    goto *lbls[info->type];";
--    else:
        text "switch [INVALID_OBJECT .. N_CLOSURE_TYPES] (TO_W_(%INFO_TYPE(%STD_INFO(info)))) {",
        nest 4 (vcat [

--    if fast == 1:
--        print "    bco_lbl:"
--    else:
        text "case BCO: {",
        nest 4 (vcat [
          text "arity = TO_W_(StgBCO_arity(R1));",
          text "ASSERT(arity > 0);",
          bco_doc
         ]),
        text "}",

--    if fast == 1:
--        print "    fun_lbl:"
--    else:
        text "case FUN,",
        text "     FUN_1_0,",
        text "     FUN_0_1,",
        text "     FUN_2_0,",
        text "     FUN_1_1,",
        text "     FUN_0_2,",
        text "     FUN_STATIC,",
        text "     CONTINUATION: {",
        nest 4 (vcat [
          text "arity = TO_W_(StgFunInfoExtra_arity(%FUN_INFO(info)));",
          text "ASSERT(arity > 0);",
          fun_doc
         ]),
        text "}",

--    if fast == 1:
--        print "    pap_lbl:"
--    else:

        text "case PAP: {",
        nest 4 (vcat [
          text "arity = TO_W_(StgPAP_arity(R1));",
          text "ASSERT(arity > 0);",
          pap_doc
         ]),
        text "}",

        text "",

--    if fast == 1:
--        print "    thunk_lbl:"
--    else:
        text "case AP,",
        text "     AP_STACK,",
        text "     BLACKHOLE,",
        text "     WHITEHOLE,",
        text "     THUNK,",
        text "     THUNK_1_0,",
        text "     THUNK_0_1,",
        text "     THUNK_2_0,",
        text "     THUNK_1_1,",
        text "     THUNK_0_2,",
        text "     THUNK_STATIC,",
        text "     THUNK_SELECTOR: {",
        nest 4 (vcat [
--          text "TICK_SLOW_CALL_UNEVALD(" <> int (length args) <> text ");",
          text "Sp(0) = " <> fun_info_label <> semi,
          -- CAREFUL! in SMP mode, the info table may already have been
          -- overwritten by an indirection, so we must enter the original
          -- info pointer we read, don't read it again, because it might
          -- not be enterable any more.
          mkJumpSaveCCCS platform
            (text "%ENTRY_CODE(info)") ["R1"] args,
            -- see Note [jump_SAVE_CCCS]
          text ""
         ]),
        text "}",

--    if fast == 1:
--        print "    ind_lbl:"
--    else:
        text "case IND,",
        text "     IND_STATIC: {",
        nest 4 (vcat [
          -- N.B. annoyingly the %acquire syntax must place its result in a local register
          -- as it is a Cmm prim call node.
          text "P_ p;",
          text "p = %acquire StgInd_indirectee(R1);",
          text "R1 = p;",
            -- An indirection node might contain a tagged pointer
          text "goto again;"
         ]),
        text "}",
        text "",

--    if fast == 0:

       text "default: {",
       nest 4 (
         text "ccall sbarf(\"" <> fun_ret_label <> text "\") never returns;"
       ),
       text "}"

        ]),
       text "}"
      ]),

      text "}"
    ]

-- -----------------------------------------------------------------------------
-- Making a fast unknown application, args are in regs

genApplyFast :: Platform -> [ArgRep] -> Doc
genApplyFast platform args =
   let
    fun_fast_label = mkApplyFastName args
    fun_ret_label  = text "RET_LBL" <> parens (mkApplyNameDoc args)
    fun_info_label = mkApplyInfoName args
    all_args_size  = sum (map (argSize platform) args)

    (fun_doc, fun_stack) =
       genMkPAP platform "BUILD_PAP" "%GET_ENTRY(UNTAG(R1))" ["R1"] "FUN" "FUN"
            False{-reg apply-} True{-args in regs-} False{-not a PAP-}
            args all_args_size fun_info_label {- tag stmt -}True

    (reg_locs, _leftovers, sp_offset) = assignRegs platform 1 args

    stack_usage = maxStack [fun_stack, (sp_offset,sp_offset)]

   in
    vcat $ [
     fun_fast_label,
     char '{',
     nest 4 (vcat $ vecsCpp fun_fast_label (map fst reg_locs) [
        text "W_ info;",
        text "W_ arity;",

        tickForArity platform (length args),

        -- if pointer is tagged enter it fast!
        enterFastPath platform False True args,

        stackCheck platform args True{-args in regs-}
                   fun_info_label stack_usage,

        -- Functions can be tagged, so we untag them!
        text  "R1 = UNTAG(R1);",
        text  "info = %GET_STD_INFO(R1);",
        text "switch [INVALID_OBJECT .. N_CLOSURE_TYPES] (TO_W_(%INFO_TYPE(info))) {",
        nest 4 (vcat [
          text "case FUN,",
          text "     FUN_1_0,",
          text "     FUN_0_1,",
          text "     FUN_2_0,",
          text "     FUN_1_1,",
          text "     FUN_0_2,",
          text "     FUN_STATIC,",
          text "     CONTINUATION: {",
          nest 4 (vcat [
            text "arity = TO_W_(StgFunInfoExtra_arity(%GET_FUN_INFO(R1)));",
            text "ASSERT(arity > 0);",
            fun_doc
           ]),
          char '}',

          text "default: {",
          nest 4 (vcat [
             text "Sp_adj" <> parens (int (-sp_offset)) <> semi,
             saveRegOffs reg_locs,
             mkJump platform fun_ret_label [] args
          ]),
          char '}'
        ]),

       char '}'
     ]),
     char '}'
  ]

-- -----------------------------------------------------------------------------
-- Making a stack apply

-- These little functions are like slow entry points.  They provide
-- the layer between the PAP entry code and the function's fast entry
-- point: namely they load arguments off the stack into registers (if
-- available) and jump to the function's entry code.
--
-- On entry: R1 points to the function closure
--           arguments are on the stack starting at Sp
--
-- Invariant: the list of arguments never contains void.  Since we're only
-- interested in loading arguments off the stack here, we can ignore
-- void arguments.

mkStackApplyEntryLabel:: [ArgRep] -> Doc
mkStackApplyEntryLabel args = text "stg_ap_stk_" <> text (concatMap argRepSuffix args)

genStackApply :: Platform -> [ArgRep] -> Doc
genStackApply platform args =
  vcat [
    fn_entry_label,
    text "{", nest 4 body, text "}"
   ]
 where
   fn_entry_label = mkStackApplyEntryLabel args
   (assign_regs, sp') = loadRegArgs platform 0 args
   (regs, _, _) = assignRegs platform 0 args
   body = vcat $
     vecsCpp fn_entry_label (map fst regs)
       [ assign_regs
       , text "Sp_adj" <> parens (int sp') <> semi
       , mkJump platform (text "%GET_ENTRY(UNTAG(R1))") ["R1"] args
       ]

-- -----------------------------------------------------------------------------
-- Stack save entry points.
--
-- These code fragments are used to save registers on the stack at a heap
-- check failure in the entry code for a function.  We also have to save R1
-- and the return address (stg_gc_fun_info) on the stack.  See __stg_gc_fun
-- in HeapStackCheck.cmm for more details.

mkStackSaveEntryLabel :: [ArgRep] -> Doc
mkStackSaveEntryLabel args = text "stg_stk_save_" <> text (concatMap argRepSuffix args)

genStackSave :: Platform -> [ArgRep] -> Doc
genStackSave platform args =
  vcat [
    fn_entry_label,
    text "{", nest 4 body, text "}"
   ]
 where
   fn_entry_label = mkStackSaveEntryLabel args
   body = vcat $
     vecsCpp fn_entry_label (map fst reg_locs)
       [ text "Sp_adj" <> parens (int (-sp_offset)) <> semi
       , saveRegOffs reg_locs
       , text "Sp(2) = R1;"
       , text "Sp(1) =" <+> int stk_args <> semi
       , text "Sp(0) = stg_gc_fun_info;"
       , text "jump stg_gc_noregs [];"
       ]

   std_frame_size = 3 -- the std bits of the frame. See StgRetFun in Closures.h,
                      -- and the comment on stg_fun_gc_gen
                      -- in HeapStackCheck.cmm.
   (reg_locs, leftovers, sp_offset) = assignRegs platform std_frame_size args

   -- number of words of arguments on the stack.
   stk_args = sum (map (argSize platform) leftovers) + sp_offset - std_frame_size

-- -----------------------------------------------------------------------------
-- The whole file

-- | Generate the Cmm source of the RTS generic application code.
--
-- Because of Note [AutoApply.cmm for vectors], the code for V16/V32/V64
-- arguments goes into separate files, selected by the second argument:
--
--  * 'Nothing': generate code for all ArgReps except vectors
--    (@AutoApply.cmm@, which also contains the dispatch tables)
--  * @'Just' 16@, @'Just' 32@, @'Just' 64@: generate the code involving
--    V16, V32 or V64 vectors (at the widest), respectively
genAutoApply :: Platform -> Maybe Int -> String
genAutoApply platform mbVec = renderStyle style the_code
  where
    wantArgs :: [ArgRep] -> Bool
    wantArgs reps =
        case mbVec of
          Nothing -> isNothing (largestVec reps)
          Just v  -> largestVec reps == Just v

    the_code = vcat [
                text "// DO NOT EDIT!",
                text "// Automatically generated by GHC.StgToCmm.AutoApply (ghc --gen-apply)",
                text "",
                text "#include \"Cmm.h\"",
                text "#include \"AutoApply.h\"",
                text "#if !defined(UnregisterisedCompiler)",
                text "import CLOSURE ALLOC_RTS_ctr;",
                text "import CLOSURE ALLOC_RTS_tot;",
                text "import CLOSURE HEAP_CHK_ctr;",
                text "import CLOSURE RtsFlags;",
                text "import CLOSURE stg_PAP_info;",
         vcat [ text "import CLOSURE" <+> mkApplyInfoName argReps <> semi
              | argReps <- applyTypes
              , wantArgs argReps ],
                text "import CLOSURE stg_gc_fun_info;",
                text "import CLOSURE stg_restore_cccs_d_info;",
                text "import CLOSURE stg_restore_cccs_v16_info;",
                text "import CLOSURE stg_restore_cccs_v32_info;",
                text "import CLOSURE stg_restore_cccs_v64_info;",
                text "#endif // !defined(UnregisterisedCompiler)",

         -- NB: the vector apply/save functions are defined in separate modules,
         -- as per Note [AutoApply.cmm for vectors], so we import them here.
         vcat [ text "import" <+> fun <> semi
              | isNothing mbVec
              , argReps <- stackApplyTypes
              , not (wantArgs argReps)
              , fun <- [ mkStackApplyEntryLabel argReps
                       , mkStackSaveEntryLabel argReps ]
              ],

                vcat $ intersperse (text "") $
                   [ genApply platform argReps
                   | argReps <- applyTypes
                   , wantArgs argReps ],
                vcat $ intersperse (text "") $
                   [ genStackFns platform argReps
                   | argReps <- stackApplyTypes
                   , wantArgs argReps ],
                vcat $ intersperse (text "") $
                   [ genApplyFast platform argReps
                   | argReps <- applyTypes
                   , wantArgs argReps ],

                if isNothing mbVec
                then
         vcat [ genStackApplyArray stackApplyTypes,
                genStackSaveArray stackApplyTypes,
                genBitmapArray platform stackApplyTypes
              ]
                else empty,
                text ""  -- add a newline at the end of the file
              ]

genStackFns :: Platform -> [ArgRep] -> Doc
genStackFns platform args
  =  genStackApply platform args
  $$ genStackSave platform args

genStackApplyArray :: [[ArgRep]] -> Doc
genStackApplyArray types =
  vcat [
    text "section \"relrodata\" {",
    text "stg_ap_stack_entries:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map arr_ent types),
    text "}"
  ]
 where
  arr_ent ty = text "W_" <+> mkStackApplyEntryLabel ty <> semi

genStackSaveArray :: [[ArgRep]] -> Doc
genStackSaveArray types =
  vcat [
    text "section \"relrodata\" {",
    text "stg_stack_save_entries:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map arr_ent types),
    text "}"
  ]
 where
  arr_ent ty = text "W_" <+> mkStackSaveEntryLabel ty <> semi

genBitmapArray :: Platform -> [[ArgRep]] -> Doc
genBitmapArray platform types =
  vcat [
    text "section \"rodata\" {",
    text "stg_arg_bitmaps:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map gen_bitmap types),
    text "}"
  ]
  where
   bitmapBitsShift = pc_BITMAP_BITS_SHIFT (platformConstants platform)
   gen_bitmap ty = text "W_" <+> int bitmap_val <> semi
        where bitmap_val =
                (fromIntegral (mkBitmap platform ty) `shiftL` bitmapBitsShift)
                 .|. sum (map (argSize platform) ty)
