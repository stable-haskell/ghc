-- Cmm representations using Hoopl's Graph CmmNode e x.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}

module GHC.Cmm (
     -- * Cmm top-level datatypes
     DCmmGroup,
     CmmProgram, CmmGroup, CmmGroupSRTs, RawCmmGroup, GenCmmGroup,
     CmmDecl, DCmmDecl, CmmDeclSRTs, GenCmmDecl(..),
     CmmDataDecl, cmmDataDeclCmmDecl, DCmmGraph,
     CmmGraph, GenCmmGraph, GenGenCmmGraph(..),
     toBlockMap, revPostorder, toBlockList,
     CmmBlock, RawCmmDecl,
     RawCmmProcInfo(..),
     CmmProcAttrs(..), emptyCmmProcAttrs, CmmTargetFeature(..),
     parseCmmTargetFeature, pprCmmTargetFeature, cmmTargetFeatureName,
     cmmTargetFeatureImplies,
     cmmTargetFeaturesSupportedOn, checkNoCmmProcAttrs,
     Section(..), SectionType(..),
     GenCmmStatics(..), type CmmStatics, type RawCmmStatics, CmmStatic(..),
     SectionProtection(..), sectionProtection,

     DWrap(..), unDeterm, removeDeterm, removeDetermDecl, removeDetermGraph,

     -- ** Blocks containing lists
     GenBasicBlock(..), blockId,
     ListGraph(..), pprBBlock,

     -- * Info Tables
     GenCmmTopInfo(..)
     , DCmmTopInfo
     , CmmTopInfo
     , CmmStackInfo(..), CmmInfoTable(..), topInfoTable, topInfoTableD,
     ClosureTypeInfo(..),
     ProfilingInfo(..), ConstrDescription,

     -- * Statements, expressions and types
     module GHC.Cmm.Node,
     module GHC.Cmm.Expr,

     -- * Pretty-printing
     pprCmmGroup, pprSection, pprStatic
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Types.Id
import GHC.Types.CostCentre
import GHC.Cmm.CLabel
import GHC.Cmm.BlockId
import GHC.Cmm.Node
import GHC.Runtime.Heap.Layout
import GHC.Cmm.Expr
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Utils.Outputable
import GHC.Utils.Panic (sorryDoc)

import Data.Void (Void)
import Data.List (intersperse)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-----------------------------------------------------------------------------
--  Cmm, GenCmm
-----------------------------------------------------------------------------

-- A CmmProgram is a list of CmmGroups
-- A CmmGroup is a list of top-level declarations

-- When object-splitting is on, each group is compiled into a separate
-- .o file. So typically we put closely related stuff in a CmmGroup.
-- Section-splitting follows suit and makes one .text subsection for each
-- CmmGroup.

type CmmProgram = [CmmGroup]

type GenCmmGroup d h g = [GenCmmDecl d h g]
-- | Cmm group after STG generation
type DCmmGroup    = GenCmmGroup CmmStatics    DCmmTopInfo              DCmmGraph
-- | Cmm group before SRT generation
type CmmGroup     = GenCmmGroup CmmStatics    CmmTopInfo               CmmGraph
-- | Cmm group with SRTs
type CmmGroupSRTs = GenCmmGroup RawCmmStatics CmmTopInfo               CmmGraph
-- | "Raw" cmm group (TODO (osa): not sure what that means)
type RawCmmGroup  = GenCmmGroup RawCmmStatics RawCmmProcInfo CmmGraph

-----------------------------------------------------------------------------
--  CmmDecl, GenCmmDecl
-----------------------------------------------------------------------------

-- GenCmmDecl is abstracted over
--   d, the type of static data elements in CmmData
--   h, the static info preceding the code of a CmmProc
--   g, the control-flow graph of a CmmProc
--
-- We expect there to be two main instances of this type:
--   (a) C--, i.e. populated with various C-- constructs
--   (b) Native code, populated with data/instructions

-- | A top-level chunk, abstracted over the type of the contents of
-- the basic blocks (Cmm or instructions are the likely instantiations).
data GenCmmDecl d h g
  = CmmProc     -- A procedure
     h                 -- Extra header such as the info table
     CLabel            -- Entry label
     [GlobalRegUse]    -- Registers live on entry. Note that the set of live
                       -- registers will be correct in generated C-- code, but
                       -- not in hand-written C-- code. However,
                       -- splitAtProcPoints calculates correct liveness
                       -- information for CmmProcs.
     g                 -- Control-flow graph for the procedure's code

  | CmmData     -- Static data
        Section
        d

  deriving (Functor)

instance (OutputableP Platform d, OutputableP Platform info, OutputableP Platform i)
      => OutputableP Platform (GenCmmDecl d info i) where
    pdoc = pprTop

type DCmmDecl    = GenCmmDecl CmmStatics DCmmTopInfo DCmmGraph
type CmmDecl     = GenCmmDecl CmmStatics    CmmTopInfo CmmGraph
type CmmDeclSRTs = GenCmmDecl RawCmmStatics CmmTopInfo CmmGraph
type CmmDataDecl = GenCmmDataDecl CmmStatics
type GenCmmDataDecl d = GenCmmDecl d Void Void -- When `CmmProc` case can be statically excluded

cmmDataDeclCmmDecl :: GenCmmDataDecl d -> GenCmmDecl d h g
cmmDataDeclCmmDecl = \ case
    CmmProc void _ _ _ -> case void of
    CmmData section d -> CmmData section d
{-# INLINE cmmDataDeclCmmDecl #-}

type RawCmmDecl
   = GenCmmDecl
        RawCmmStatics
        RawCmmProcInfo
        CmmGraph

-- | The header of a "raw" 'CmmProc', i.e. after 'GHC.Cmm.Info.cmmToRawCmm'
-- has turned the info tables into static data.
data RawCmmProcInfo = RawCmmProcInfo
     { raw_info_tbls  :: !(LabelMap RawCmmStatics)
       -- ^ The procedure's info tables, as static data.
     , raw_proc_attrs :: !CmmProcAttrs
       -- ^ Attributes the procedure carried in the source.
       -- See Note [Cmm target attributes].
       --
       -- Strict: lazy, this field holds on to the pre-raw 'CmmTopInfo'
       -- through the thunk 'GHC.Cmm.Info.mkInfoTable' builds, which is the
       -- leak the seqList there avoids.
     }

instance OutputableP Platform RawCmmProcInfo where
    pdoc platform (RawCmmProcInfo tbls attrs) =
      vcat [ pdoc platform tbls, ppr attrs ]

-----------------------------------------------------------------------------
--     Graphs
-----------------------------------------------------------------------------

type CmmGraph = GenCmmGraph CmmNode
type DCmmGraph = GenGenCmmGraph DWrap CmmNode

type GenCmmGraph n = GenGenCmmGraph LabelMap n

data GenGenCmmGraph s n = CmmGraph { g_entry :: BlockId, g_graph :: Graph' s Block n C C }
type CmmBlock = Block CmmNode C C

instance OutputableP Platform CmmGraph where
    pdoc = pprCmmGraph

toBlockMap :: CmmGraph -> LabelMap CmmBlock
toBlockMap (CmmGraph {g_graph=GMany NothingO body NothingO}) = body

pprCmmGraph :: Platform -> CmmGraph -> SDoc
pprCmmGraph platform g
   = text "{" <> text "offset"
  $$ nest 2 (vcat $ map (pdoc platform) blocks)
  $$ text "}"
  where blocks = revPostorder g
    -- revPostorder has the side-effect of discarding unreachable code,
    -- so pretty-printed Cmm will omit any unreachable blocks.  This can
    -- sometimes be confusing.

revPostorder :: CmmGraph -> [CmmBlock]
revPostorder g = {-# SCC "revPostorder" #-}
    revPostorderFrom (toBlockMap g) (g_entry g)

toBlockList :: CmmGraph -> [CmmBlock]
toBlockList g = mapElems $ toBlockMap g

-----------------------------------------------------------------------------
--     Info Tables
-----------------------------------------------------------------------------

-- | CmmTopInfo is attached to each CmmDecl (see defn of CmmGroup), and contains
-- the extra info (beyond the executable code) that belongs to that CmmDecl.
data GenCmmTopInfo f = TopInfo { info_tbls  :: f CmmInfoTable
                               , stack_info :: CmmStackInfo
                               , proc_attrs :: CmmProcAttrs
                                 -- ^ See Note [Cmm target attributes]
                               }

{- Note [Cmm target attributes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some hand-written Cmm procedures need particular CPU features, and others in the
same file must not have them.  The RTS's vector code is the case that forced
this: stg_ap_v32_fast moves 256-bit vectors, so it needs -mavx2 for the code
generator to emit YMM moves, while stg_ap_v16_fast in the same library must not
see -mavx, or it emits AVX instructions on targets that only have SSE2.  See
Note [AutoApply.cmm for vectors] in utils/genapply/Main.hs and
Note [realArgRegsCover] in GHC.Cmm.CallConv.

Per-file flags in the build system can only approximate this.  Whether a
procedure actually touches the 256-bit registers is decided in the source, by
#if defined(REG_YMM1), the same test that sets REGS_ALLOWED in rts/Jumps.h.
Hadrian used to match on a filename (Jumps_V32.cmm) and the target arch
instead, which is a second copy of that condition, kept in step by hand, and
one that can be no finer than a whole file.  The V16/V32/V64 file split exists
largely to give it something to match on.

So the procedure states the requirement itself, spelled after the GCC function
attribute:

    __attribute__((target("avx2")))
    stg_ap_v32_fast ( ... )
    {
        ...
    }

'CmmProcAttrs' carries that from the parser to the backends:

  * the parser attaches it to the proc's 'CmmTopInfo';

  * 'GHC.Cmm.ProcPoint.splitAtProcPoints' copies it to every proc it splits
    out, so a continuation is compiled like the proc it came from.  This is why
    it sits on 'CmmTopInfo' and not in a side table keyed on entry label, which
    would lose it here;

  * 'GHC.Cmm.Info.cmmToRawCmm' carries it into 'RawCmmProcInfo';

  * 'GHC.CmmToAsm.cmmNativeGens' derives a per-procedure 'NCGConfig', so
    instruction selection and the register allocator's spill code agree on
    which registers exist.

Per-procedure granularity is sound because Cmm has no inter-procedural
inlining.

None of this can be checked after the fact: an attribute that a backend ignored
shows up as a segfault, not as a build failure.  That is why the vocabulary is
closed, so that an unknown feature is a parse error, and why a backend that
cannot honour the attribute refuses it through 'checkNoCmmProcAttrs'.

The C backend refuses it outright.  It is reached by unregisterised builds,
where MACHREGS_NO_REGS is 1, so REG_YMM1 is not defined and there are no vector
registers in the calling convention to get wrong; nothing there needs the
feature in the first place.

Two limitations:

  * The attribute only turns features on.  There is no target("no-avx"), so
    under a global -mavx2 stg_ap_v16_fast still gets AVX; that half of the RTS
    invariant still depends on not passing -mavx2 globally.

  * CPP runs before the parser, so __AVX2__ and friends still come from the
    command line alone (see GHC.SysTools.Cpp).  A .cmm file cannot test for its
    own attribute.
-}

-- | A CPU feature a Cmm procedure can require.  A closed set, so that an
-- unknown feature is a parse error.  See Note [Cmm target attributes].
data CmmTargetFeature
  = CmmTargetAvx
  | CmmTargetAvx2
  | CmmTargetAvx512f
  deriving (Eq, Ord, Enum, Bounded)

-- | Attributes attached to a Cmm procedure in the source.
-- See Note [Cmm target attributes].
newtype CmmProcAttrs = CmmProcAttrs
  { cpa_target_features :: [CmmTargetFeature] }
  deriving (Eq)

emptyCmmProcAttrs :: CmmProcAttrs
emptyCmmProcAttrs = CmmProcAttrs []

-- | Recognise a @target(...)@ feature name, as written in the source.
-- Inverse of 'cmmTargetFeatureName'.
parseCmmTargetFeature :: String -> Maybe CmmTargetFeature
parseCmmTargetFeature = \case
  "avx"     -> Just CmmTargetAvx
  "avx2"    -> Just CmmTargetAvx2
  "avx512f" -> Just CmmTargetAvx512f
  _         -> Nothing

-- | The feature's name, as written in the source and as LLVM spells it.
cmmTargetFeatureName :: CmmTargetFeature -> String
cmmTargetFeatureName = \case
  CmmTargetAvx     -> "avx"
  CmmTargetAvx2    -> "avx2"
  CmmTargetAvx512f -> "avx512f"

-- | The features a feature implies, including itself.  Follows
-- 'isAvxEnabled' and friends in GHC.Driver.DynFlags, where -mavx512f implies
-- -mavx2 implies -mavx.
cmmTargetFeatureImplies :: CmmTargetFeature -> [CmmTargetFeature]
cmmTargetFeatureImplies = \case
  CmmTargetAvx     -> [CmmTargetAvx]
  CmmTargetAvx2    -> [CmmTargetAvx, CmmTargetAvx2]
  CmmTargetAvx512f -> [CmmTargetAvx, CmmTargetAvx2, CmmTargetAvx512f]

-- | Refuse a procedure whose target attributes this backend cannot honour.
-- See Note [Cmm target attributes].
checkNoCmmProcAttrs :: String   -- ^ what cannot honour them, e.g. "the C backend"
                     -> SDoc     -- ^ the procedure, for the message
                     -> CmmProcAttrs -> a -> a
checkNoCmmProcAttrs _ _ (CmmProcAttrs []) k = k
checkNoCmmProcAttrs what who (CmmProcAttrs fs) _ =
  sorryDoc ("Cmm target attributes are not supported by " ++ what) $
    vcat [ text "In procedure:" <+> who
         , text "Attributes:"
             <+> hcat (punctuate comma (map pprCmmTargetFeature fs)) ]

-- | Can this architecture have these features at all?  Shared by the backends
-- that honour the attribute, so they refuse the same things.
cmmTargetFeaturesSupportedOn :: Arch -> [CmmTargetFeature] -> Bool
cmmTargetFeaturesSupportedOn arch = all supported
  where
    supported f = case f of
      CmmTargetAvx     -> is_x86
      CmmTargetAvx2    -> is_x86
      CmmTargetAvx512f -> is_x86
    is_x86 = case arch of
      ArchX86    -> True
      ArchX86_64 -> True
      _          -> False

pprCmmTargetFeature :: CmmTargetFeature -> SDoc
pprCmmTargetFeature = text . cmmTargetFeatureName

instance Outputable CmmTargetFeature where
  ppr = pprCmmTargetFeature

instance Outputable CmmProcAttrs where
  ppr (CmmProcAttrs []) = empty
  ppr (CmmProcAttrs fs) =
    text "__attribute__((target(" <>
      doubleQuotes (hcat (intersperse comma (map ppr fs))) <> text ")))"

newtype DWrap a = DWrap [(BlockId, a)]

unDeterm :: DWrap a -> [(BlockId, a)]
unDeterm (DWrap f) = f

type DCmmTopInfo = GenCmmTopInfo DWrap
type CmmTopInfo  = GenCmmTopInfo LabelMap

instance OutputableP Platform CmmTopInfo where
    pdoc = pprTopInfo

pprTopInfo :: Platform -> CmmTopInfo -> SDoc
pprTopInfo platform (TopInfo {info_tbls=info_tbl, stack_info=stack_info,
                              proc_attrs=attrs}) =
  vcat [text "info_tbls: " <> pdoc platform info_tbl,
        text "stack_info: " <> ppr stack_info,
        -- Empty for generated code, where 'ppr' gives 'empty', so dump output
        -- is unchanged.
        ppr attrs]

topInfoTableD :: GenCmmDecl a DCmmTopInfo (GenGenCmmGraph s n) -> Maybe CmmInfoTable
topInfoTableD (CmmProc infos _ _ g) = case (info_tbls infos) of
                                          DWrap xs -> lookup (g_entry g) xs
topInfoTableD _                     = Nothing

topInfoTable :: GenCmmDecl a CmmTopInfo (GenGenCmmGraph s n) -> Maybe CmmInfoTable
topInfoTable (CmmProc infos _ _ g) = mapLookup (g_entry g) (info_tbls infos)
topInfoTable _                     = Nothing

data CmmStackInfo
   = StackInfo {
       arg_space :: ByteOff,
               -- number of bytes of arguments on the stack on entry to the
               -- the proc.  This is filled in by GHC.StgToCmm.codeGen, and
               -- used by the stack allocator later.
       do_layout :: Bool
               -- Do automatic stack layout for this proc.  This is
               -- True for all code generated by the code generator,
               -- but is occasionally False for hand-written Cmm where
               -- we want to do the stack manipulation manually.
  }

instance Outputable CmmStackInfo where
    ppr = pprStackInfo

pprStackInfo :: CmmStackInfo -> SDoc
pprStackInfo (StackInfo {arg_space=arg_space}) =
  text "arg_space: " <> ppr arg_space

-- | Info table as a haskell data type
data CmmInfoTable
  = CmmInfoTable {
      cit_lbl  :: CLabel, -- Info table label
      cit_rep  :: SMRep,
      cit_prof :: ProfilingInfo,
      cit_srt  :: Maybe CLabel,   -- empty, or a closure address
      cit_clo  :: Maybe (Id, CostCentreStack)
        -- Just (id,ccs) <=> build a static closure later
        -- Nothing <=> don't build a static closure
        --
        -- Static closures for FUNs and THUNKs are *not* generated by
        -- the code generator, because we might want to add SRT
        -- entries to them later (for FUNs at least; THUNKs are
        -- treated the same for consistency). See Note [SRTs] in
        -- GHC.Cmm.Info.Build, in particular the [FUN] optimisation.
        --
        -- This is strictly speaking not a part of the info table that
        -- will be finally generated, but it's the only convenient
        -- place to convey this information from the code generator to
        -- where we build the static closures in
        -- GHC.Cmm.Info.Build.doSRTs.
    } deriving (Eq, Ord)

instance OutputableP Platform CmmInfoTable where
    pdoc = pprInfoTable

data ProfilingInfo
  = NoProfilingInfo
  | ProfilingInfo ByteString ByteString -- closure_type, closure_desc
  deriving (Eq, Ord)

-----------------------------------------------------------------------------
--              Static Data
-----------------------------------------------------------------------------

data SectionType
  = Text
  | Data
  | ReadOnlyData
  | RelocatableReadOnlyData
  | UninitialisedData
    -- See Note [Initializers and finalizers in Cmm] in GHC.Cmm.InitFini
  | InitArray           -- .init_array on ELF, .ctor on Windows
  | FiniArray           -- .fini_array on ELF, .dtor on Windows
  | CString
  | OtherSection String
  deriving (Show)

data SectionProtection
  = ReadWriteSection
  | ReadOnlySection
  | WriteProtectedSection -- See Note [Relocatable Read-Only Data]
  deriving (Eq)

-- | Should a data in this section be considered constant at runtime
sectionProtection :: SectionType -> SectionProtection
sectionProtection t = case t of
    Text                    -> ReadOnlySection
    ReadOnlyData            -> ReadOnlySection
    RelocatableReadOnlyData -> WriteProtectedSection
    InitArray               -> ReadOnlySection
    FiniArray               -> ReadOnlySection
    CString                 -> ReadOnlySection
    Data                    -> ReadWriteSection
    UninitialisedData       -> ReadWriteSection
    (OtherSection _)        -> ReadWriteSection

{-
Note [Relocatable Read-Only Data]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Relocatable data are only read-only after relocation at the start of the
program. They should be writable from the source code until then. Failure to
do so would end up in segfaults at execution when using linkers that do not
enforce writability of those sections, such as the gold linker.
-}

data Section = Section SectionType CLabel

data CmmStatic
  = CmmStaticLit CmmLit
        -- ^ a literal value, size given by cmmLitRep of the literal.
  | CmmUninitialised Int
        -- ^ uninitialised data, N bytes long
  | CmmString ByteString
        -- ^ string of 8-bit values only, not zero terminated.
  | CmmFileEmbed FilePath Int
        -- ^ an embedded binary file and its byte length

instance OutputableP Platform CmmStatic where
    pdoc = pprStatic

instance Outputable CmmStatic where
  ppr (CmmStaticLit lit) = text "CmmStaticLit" <+> ppr lit
  ppr (CmmUninitialised n) = text "CmmUninitialised" <+> ppr n
  ppr (CmmString _) = text "CmmString"
  ppr (CmmFileEmbed fp _) = text "CmmFileEmbed" <+> text fp

-- | Static data before or after SRT generation
data GenCmmStatics (rawOnly :: Bool) where
    CmmStatics
      :: CLabel       -- Label of statics
      -> CmmInfoTable
      -> CostCentreStack
      -> [CmmLit]     -- Payload
      -> [CmmLit]     -- Non-pointers that go to the end of the closure
                      -- This is used by stg_unpack_cstring closures.
                      -- See Note [unpack_cstring closures] in StgStdThunks.cmm.
      -> GenCmmStatics 'False

    -- | Static data, after SRTs are generated
    CmmStaticsRaw
      :: CLabel       -- Label of statics
      -> [CmmStatic]  -- The static data itself
      -> GenCmmStatics a

instance OutputableP Platform (GenCmmStatics a) where
    pdoc = pprStatics

type CmmStatics    = GenCmmStatics 'False
type RawCmmStatics = GenCmmStatics 'True

{-
-----------------------------------------------------------------------------
--              Deterministic Cmm / Info Tables
-----------------------------------------------------------------------------

Note [DCmmGroup vs CmmGroup or: Deterministic Info Tables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consulting Note [Object determinism] one will learn that in order to produce
deterministic objects just after cmm is produced we perform a renaming pass which
provides fresh uniques for all unique-able things in the input Cmm.

After this point, we use a deterministic unique supply (an incrementing counter)
so any resulting labels which make their way into object code have a deterministic name.

A key assumption to this process is that the input is deterministic modulo the uniques
and the order that bindings appear in the definitions is the same.

CmmGroup uses LabelMap in two places:

* In CmmProc for info tables
* In CmmGraph for the blocks of the graph

LabelMap is not a deterministic structure, so traversing a LabelMap can process
elements in different order (depending on the given uniques).

Therefore before we do the renaming we need to use a deterministic structure, one
which we can traverse in a guaranteed order. A list does the job perfectly.

Once the renaming happens it is converted back into a LabelMap, which is now deterministic
due to the uniques being generated and assigned in a deterministic manner.

We prefer using the renamed LabelMap rather than the list in the rest of the
code generation because it is much more efficient than lists for the needs of
the code generator.
-}

-- Converting out of deterministic Cmm

removeDeterm :: DCmmGroup -> CmmGroup
removeDeterm = map removeDetermDecl

removeDetermDecl :: DCmmDecl -> CmmDecl
removeDetermDecl (CmmProc h e r g) = CmmProc (removeDetermTop h) e r (removeDetermGraph g)
removeDetermDecl (CmmData a b) = CmmData a b

removeDetermTop :: DCmmTopInfo -> CmmTopInfo
removeDetermTop (TopInfo a b c) = TopInfo (mapFromList $ unDeterm a) b c

removeDetermGraph :: DCmmGraph -> CmmGraph
removeDetermGraph (CmmGraph x y) =
  let y' = case y of
            GMany a (DWrap b) c -> GMany a (mapFromList b) c
  in CmmGraph x y'

-- -----------------------------------------------------------------------------
-- Basic blocks consisting of lists

-- These are used by the LLVM and NCG backends, when populating Cmm
-- with lists of instructions.

data GenBasicBlock i
   = BasicBlock BlockId [i]
   deriving (Functor)


-- | The branch block id is that of the first block in
-- the branch, which is that branch's entry point
blockId :: GenBasicBlock i -> BlockId
blockId (BasicBlock blk_id _ ) = blk_id

newtype ListGraph i
   = ListGraph [GenBasicBlock i]
   deriving (Functor)

instance Outputable instr => Outputable (ListGraph instr) where
    ppr (ListGraph blocks) = vcat (map ppr blocks)

instance OutputableP env instr => OutputableP env (ListGraph instr) where
    pdoc env g = ppr (fmap (pdoc env) g)


instance Outputable instr => Outputable (GenBasicBlock instr) where
    ppr = pprBBlock

instance OutputableP env instr => OutputableP env (GenBasicBlock instr) where
    pdoc env block = ppr (fmap (pdoc env) block)

pprBBlock :: Outputable stmt => GenBasicBlock stmt -> SDoc
pprBBlock (BasicBlock ident stmts) =
    hang (ppr ident <> colon) 4 (vcat (map ppr stmts))


-- --------------------------------------------------------------------------
-- Pretty-printing Cmm
-- --------------------------------------------------------------------------
--
-- This is where we walk over Cmm emitting an external representation,
-- suitable for parsing, in a syntax strongly reminiscent of C--. This
-- is the "External Core" for the Cmm layer.
--
-- As such, this should be a well-defined syntax: we want it to look nice.
-- Thus, we try wherever possible to use syntax defined in [1],
-- "The C-- Reference Manual", http://www.cs.tufts.edu/~nr/c--/index.html. We
-- differ slightly, in some cases. For one, we use I8 .. I64 for types, rather
-- than C--'s bits8 .. bits64.
--
-- We try to ensure that all information available in the abstract
-- syntax is reproduced, or reproducible, in the concrete syntax.
-- Data that is not in printed out can be reconstructed according to
-- conventions used in the pretty printer. There are at least two such
-- cases:
--      1) if a value has wordRep type, the type is not appended in the
--      output.
--      2) MachOps that operate over wordRep type are printed in a
--      C-style, rather than as their internal MachRep name.
--
-- These conventions produce much more readable Cmm output.

pprCmmGroup :: (OutputableP Platform d, OutputableP Platform info, OutputableP Platform g)
            => Platform -> GenCmmGroup d info g -> SDoc
pprCmmGroup platform tops
    = vcat $ intersperse blankLine $ map (pprTop platform) tops

-- --------------------------------------------------------------------------
-- Top level `procedure' blocks.
--

pprTop :: (OutputableP Platform d, OutputableP Platform info, OutputableP Platform i)
       => Platform -> GenCmmDecl d info i -> SDoc

pprTop platform (CmmProc info lbl live graph)

  = vcat [ pdoc platform lbl <> lparen <> rparen <+> lbrace <+> text "// " <+> ppr live
         , nest 8 $ lbrace <+> pdoc platform info $$ rbrace
         , nest 4 $ pdoc platform graph
         , rbrace ]

-- --------------------------------------------------------------------------
-- We follow [1], 4.5
--
--      section "data" { ... }
--

pprTop platform (CmmData section ds) =
    (hang (pprSection platform section <+> lbrace) 4 (pdoc platform ds))
    $$ rbrace

-- --------------------------------------------------------------------------
-- Pretty-printing info tables
-- --------------------------------------------------------------------------

pprInfoTable :: Platform -> CmmInfoTable -> SDoc
pprInfoTable platform (CmmInfoTable { cit_lbl = lbl, cit_rep = rep
                           , cit_prof = prof_info
                           , cit_srt = srt })
  = vcat [ text "label: " <> pdoc platform lbl
         , text "rep: " <> ppr rep
         , case prof_info of
             NoProfilingInfo -> empty
             ProfilingInfo ct cd ->
               vcat [ text "type: " <> text (show (BS.unpack ct))
                    , text "desc: " <> text (show (BS.unpack cd)) ]
         , text "srt: " <> pdoc platform srt ]

-- --------------------------------------------------------------------------
-- Static data.
--      Strings are printed as C strings, and we print them as I8[],
--      following C--
--

pprStatics :: Platform -> GenCmmStatics a -> SDoc
pprStatics platform (CmmStatics lbl itbl ccs payload extras) =
  pdoc platform lbl <> colon <+> pdoc platform itbl <+> ppr ccs <+> pdoc platform payload <+> ppr extras
pprStatics platform (CmmStaticsRaw lbl ds) = vcat ((pdoc platform lbl <> colon) : map (pprStatic platform) ds)

pprStatic :: Platform -> CmmStatic -> SDoc
pprStatic platform s = case s of
    CmmStaticLit lit   -> nest 4 $ text "const" <+> pdoc platform lit <> semi
    CmmUninitialised i -> nest 4 $ text "I8" <> brackets (int i)
    CmmString s'       -> nest 4 $ text "I8[]" <+> text (show s')
    CmmFileEmbed path _ -> nest 4 $ text "incbin " <+> text (show path)

-- --------------------------------------------------------------------------
-- data sections
--
pprSection :: Platform -> Section -> SDoc
pprSection platform (Section t suffix) =
  section <+> doubleQuotes (pprSectionType t <+> char '.' <+> pdoc platform suffix)
  where
    section = text "section"

pprSectionType :: SectionType -> SDoc
pprSectionType s = doubleQuotes $ case s of
  Text                    -> text "text"
  Data                    -> text "data"
  ReadOnlyData            -> text "readonly"
  RelocatableReadOnlyData -> text "relreadonly"
  UninitialisedData       -> text "uninitialised"
  InitArray               -> text "initarray"
  FiniArray               -> text "finiarray"
  CString                 -> text "cstring"
  OtherSection s'         -> text s'
