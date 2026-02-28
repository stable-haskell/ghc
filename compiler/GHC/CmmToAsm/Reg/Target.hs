{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Hard wired things related to registers.
--
-- This module provides architecture-specific register operations using a
-- GADT-based dispatch pattern. Each supported architecture is represented
-- as a type-level tag, and 'selectRegTarget' performs runtime dispatch
-- based on the platform, returning an existentially wrapped target.
--
-- The GADT approach provides:
--   1. Type-safe architecture dispatch
--   2. Cleaner separation of architecture-specific code
--   3. Better documentation through type-level architecture tags
--
-- CPP is used for:
--   1. Conditional imports of arch-specific modules
--   2. Conditional typeclass instances
--   3. Conditional branches in selectRegTarget
module GHC.CmmToAsm.Reg.Target (
        -- * Exported target functions
        targetVirtualRegSqueeze,
        targetRealRegSqueeze,
        targetClassOfRealReg,
        targetMkVirtualReg,
        targetRegDotColor,
        targetClassOfReg,
        mapRegFormatSet,
        -- * Internal (for testing/extension)
        ArchKind(..),
        RegTarget(..),
        SomeRegTarget(..),
        RegOps(..),
        selectRegTarget,
        withRegTarget,
)

where

import GHC.Prelude

import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.CmmToAsm.Format

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Unique
import GHC.Platform

-- Architecture-specific imports (CPP required - modules are conditionally compiled)
#if defined(HAVE_X86_NCG)
import qualified GHC.CmmToAsm.X86.Regs       as X86
import qualified GHC.CmmToAsm.X86.RegInfo    as X86
#endif
#if defined(HAVE_AARCH64_NCG)
import qualified GHC.CmmToAsm.AArch64.Regs   as AArch64
#endif
#if defined(HAVE_PPC_NCG)
import qualified GHC.CmmToAsm.PPC.Regs       as PPC
#endif
#if defined(HAVE_RISCV64_NCG)
import qualified GHC.CmmToAsm.RV64.Regs      as RV64
#endif
#if defined(HAVE_LOONGARCH64_NCG)
import qualified GHC.CmmToAsm.LA64.Regs      as LA64
#endif

-- -----------------------------------------------------------------------------
-- Architecture kind for type-level representation

-- | Architecture kind for type-level architecture representation.
-- Each supported architecture has a corresponding type-level tag.
data ArchKind
  = X86K          -- ^ X86 and X86_64 architectures
  | AArch64K      -- ^ ARM 64-bit architecture
  | PPCK          -- ^ PowerPC (32 and 64-bit) architectures
  | RV64K         -- ^ RISC-V 64-bit architecture
  | LA64K         -- ^ LoongArch 64-bit architecture
  | UnavailableK  -- ^ Unsupported/unavailable architecture

-- -----------------------------------------------------------------------------
-- GADT for architecture-specific register targets

-- | GADT representing architecture-specific register targets.
-- The type parameter ensures type-safe dispatch - each architecture
-- has its own constructor with a unique type tag.
data RegTarget (arch :: ArchKind) where
#if defined(HAVE_X86_NCG)
  X86RegTarget     :: RegTarget 'X86K
#endif
#if defined(HAVE_AARCH64_NCG)
  AArch64RegTarget :: RegTarget 'AArch64K
#endif
#if defined(HAVE_PPC_NCG)
  PPCRegTarget     :: RegTarget 'PPCK
#endif
#if defined(HAVE_RISCV64_NCG)
  RV64RegTarget    :: RegTarget 'RV64K
#endif
#if defined(HAVE_LOONGARCH64_NCG)
  LA64RegTarget    :: RegTarget 'LA64K
#endif
  -- | Target for unavailable architectures (stores arch name for error messages)
  UnavailableRegTarget :: !String -> RegTarget 'UnavailableK

-- | Existential wrapper for runtime dispatch.
-- This allows us to return a RegTarget without knowing the architecture at compile time.
data SomeRegTarget where
  SomeRegTarget :: RegOps arch => RegTarget arch -> SomeRegTarget

-- -----------------------------------------------------------------------------
-- Typeclass for architecture-specific register operations

-- | Typeclass for architecture-specific register operations.
-- Each architecture provides its own instance with the actual implementations.
class RegOps (arch :: ArchKind) where
  -- | Calculate max register colors denied to a node due to this virtual reg
  virtualRegSqueeze :: RegTarget arch -> RegClass -> VirtualReg -> Int

  -- | Calculate max register colors denied to a node due to this real reg
  realRegSqueeze :: RegTarget arch -> RegClass -> RealReg -> Int

  -- | Get the register class of a real register
  classOfRealReg :: RegTarget arch -> Platform -> RealReg -> RegClass

  -- | Create a virtual register from a unique and format
  mkVirtualReg :: RegTarget arch -> Unique -> Format -> VirtualReg

  -- | Get the dot-graph color for a register (for debugging)
  regDotColor :: RegTarget arch -> Platform -> RealReg -> SDoc

-- -----------------------------------------------------------------------------
-- Architecture-specific RegOps instances

#if defined(HAVE_X86_NCG)
instance RegOps 'X86K where
  virtualRegSqueeze X86RegTarget = X86.virtualRegSqueeze
  realRegSqueeze    X86RegTarget = X86.realRegSqueeze
  classOfRealReg    X86RegTarget = X86.classOfRealReg
  mkVirtualReg      X86RegTarget = X86.mkVirtualReg
  regDotColor       X86RegTarget = X86.regDotColor
#endif

#if defined(HAVE_AARCH64_NCG)
instance RegOps 'AArch64K where
  virtualRegSqueeze AArch64RegTarget = AArch64.virtualRegSqueeze
  realRegSqueeze    AArch64RegTarget = AArch64.realRegSqueeze
  classOfRealReg    AArch64RegTarget _ = AArch64.classOfRealReg  -- AArch64 doesn't use Platform
  mkVirtualReg      AArch64RegTarget = AArch64.mkVirtualReg
  regDotColor       AArch64RegTarget _ = AArch64.regDotColor     -- AArch64 doesn't use Platform
#endif

#if defined(HAVE_PPC_NCG)
instance RegOps 'PPCK where
  virtualRegSqueeze PPCRegTarget = PPC.virtualRegSqueeze
  realRegSqueeze    PPCRegTarget = PPC.realRegSqueeze
  classOfRealReg    PPCRegTarget _ = PPC.classOfRealReg
  mkVirtualReg      PPCRegTarget = PPC.mkVirtualReg
  regDotColor       PPCRegTarget _ = PPC.regDotColor
#endif

#if defined(HAVE_RISCV64_NCG)
instance RegOps 'RV64K where
  virtualRegSqueeze RV64RegTarget = RV64.virtualRegSqueeze
  realRegSqueeze    RV64RegTarget = RV64.realRegSqueeze
  classOfRealReg    RV64RegTarget _ = RV64.classOfRealReg
  mkVirtualReg      RV64RegTarget = RV64.mkVirtualReg
  regDotColor       RV64RegTarget _ = RV64.regDotColor
#endif

#if defined(HAVE_LOONGARCH64_NCG)
instance RegOps 'LA64K where
  virtualRegSqueeze LA64RegTarget = LA64.virtualRegSqueeze
  realRegSqueeze    LA64RegTarget = LA64.realRegSqueeze
  classOfRealReg    LA64RegTarget _ = LA64.classOfRealReg
  mkVirtualReg      LA64RegTarget = LA64.mkVirtualReg
  regDotColor       LA64RegTarget _ = LA64.regDotColor
#endif

-- | Instance for unavailable architectures - all operations panic
instance RegOps 'UnavailableK where
  virtualRegSqueeze (UnavailableRegTarget name) _ _ =
    panic $ "virtualRegSqueeze: " ++ name ++ " not available"
  realRegSqueeze (UnavailableRegTarget name) _ _ =
    panic $ "realRegSqueeze: " ++ name ++ " not available"
  classOfRealReg (UnavailableRegTarget name) _ _ =
    panic $ "classOfRealReg: " ++ name ++ " not available"
  mkVirtualReg (UnavailableRegTarget name) _ _ =
    panic $ "mkVirtualReg: " ++ name ++ " not available"
  regDotColor (UnavailableRegTarget name) _ _ =
    panic $ "regDotColor: " ++ name ++ " not available"

-- -----------------------------------------------------------------------------
-- Platform dispatch

-- | Select the appropriate RegTarget for a platform.
-- This is the single point of platform-based dispatch, returning
-- an existentially wrapped target with its RegOps constraint.
selectRegTarget :: Platform -> SomeRegTarget
selectRegTarget platform = case platformArch platform of
#if defined(HAVE_X86_NCG)
    ArchX86       -> SomeRegTarget X86RegTarget
    ArchX86_64    -> SomeRegTarget X86RegTarget
#else
    ArchX86       -> SomeRegTarget (UnavailableRegTarget "X86")
    ArchX86_64    -> SomeRegTarget (UnavailableRegTarget "X86_64")
#endif
#if defined(HAVE_AARCH64_NCG)
    ArchAArch64   -> SomeRegTarget AArch64RegTarget
#else
    ArchAArch64   -> SomeRegTarget (UnavailableRegTarget "AArch64")
#endif
#if defined(HAVE_PPC_NCG)
    ArchPPC       -> SomeRegTarget PPCRegTarget
    ArchPPC_64 _  -> SomeRegTarget PPCRegTarget
#else
    ArchPPC       -> SomeRegTarget (UnavailableRegTarget "PPC")
    ArchPPC_64 _  -> SomeRegTarget (UnavailableRegTarget "PPC_64")
#endif
#if defined(HAVE_RISCV64_NCG)
    ArchRISCV64   -> SomeRegTarget RV64RegTarget
#else
    ArchRISCV64   -> SomeRegTarget (UnavailableRegTarget "RISCV64")
#endif
#if defined(HAVE_LOONGARCH64_NCG)
    ArchLoongArch64 -> SomeRegTarget LA64RegTarget
#else
    ArchLoongArch64 -> SomeRegTarget (UnavailableRegTarget "LoongArch64")
#endif
    -- Architectures without NCG support
    ArchS390X       -> SomeRegTarget (UnavailableRegTarget "S390X")
    ArchARM _ _ _   -> SomeRegTarget (UnavailableRegTarget "ARM")
    ArchAlpha       -> SomeRegTarget (UnavailableRegTarget "Alpha")
    ArchMipseb      -> SomeRegTarget (UnavailableRegTarget "Mipseb")
    ArchMipsel      -> SomeRegTarget (UnavailableRegTarget "Mipsel")
    ArchJavaScript  -> SomeRegTarget (UnavailableRegTarget "JavaScript")
    ArchWasm32      -> SomeRegTarget (UnavailableRegTarget "Wasm32")
    ArchUnknown     -> SomeRegTarget (UnavailableRegTarget "Unknown")

-- | Helper to dispatch on SomeRegTarget.
-- Uses rank-2 types to provide the RegOps constraint to the continuation.
{-# INLINE withRegTarget #-}
withRegTarget :: SomeRegTarget -> (forall arch. RegOps arch => RegTarget arch -> r) -> r
withRegTarget (SomeRegTarget rt) f = f rt

-- -----------------------------------------------------------------------------
-- Exported functions (use withRegTarget for dispatch)

-- | Calculate the maximum number of register colors that could be
-- denied to a node of this class due to having this virtual reg as a neighbour.
{-# INLINE targetVirtualRegSqueeze #-}
targetVirtualRegSqueeze :: Platform -> RegClass -> VirtualReg -> Int
targetVirtualRegSqueeze platform cls vreg =
  withRegTarget (selectRegTarget platform) $ \rt -> virtualRegSqueeze rt cls vreg

-- | Calculate the maximum number of register colors that could be
-- denied to a node of this class due to having this real reg as a neighbour.
{-# INLINE targetRealRegSqueeze #-}
targetRealRegSqueeze :: Platform -> RegClass -> RealReg -> Int
targetRealRegSqueeze platform cls rreg =
  withRegTarget (selectRegTarget platform) $ \rt -> realRegSqueeze rt cls rreg

-- | Get the register class of a real register.
{-# INLINE targetClassOfRealReg #-}
targetClassOfRealReg :: Platform -> RealReg -> RegClass
targetClassOfRealReg platform rreg =
  withRegTarget (selectRegTarget platform) $ \rt -> classOfRealReg rt platform rreg

-- | Create a virtual register from a unique and format.
{-# INLINE targetMkVirtualReg #-}
targetMkVirtualReg :: Platform -> Unique -> Format -> VirtualReg
targetMkVirtualReg platform uniq fmt =
  withRegTarget (selectRegTarget platform) $ \rt -> mkVirtualReg rt uniq fmt

-- | Get the dot-graph color for a register (for debugging/visualization).
{-# INLINE targetRegDotColor #-}
targetRegDotColor :: Platform -> RealReg -> SDoc
targetRegDotColor platform rreg =
  withRegTarget (selectRegTarget platform) $ \rt -> regDotColor rt platform rreg

-- -----------------------------------------------------------------------------
-- Helper functions (no CPP needed)

-- | Get the register class of any register (virtual or real).
targetClassOfReg :: Platform -> Reg -> RegClass
targetClassOfReg platform reg = case reg of
    RegVirtual vr -> classOfVirtualReg (platformArch platform) vr
    RegReal rr    -> targetClassOfRealReg platform rr

-- | Map a function over registers in a set, preserving formats.
mapRegFormatSet :: HasDebugCallStack => (Reg -> Reg) -> UniqSet RegWithFormat -> UniqSet RegWithFormat
mapRegFormatSet f = mapUniqSet (\ ( RegWithFormat r fmt ) -> RegWithFormat ( f r ) fmt)
