{-# LANGUAGE CPP #-}
-- | Hard wired things related to registers.
--
-- This module provides architecture-specific register operations using a
-- record-based dispatch pattern. Each supported architecture provides a
-- 'RegTarget' record with its implementations, and 'selectRegTarget' performs
-- runtime dispatch based on the platform.
--
-- The CPP is consolidated to:
--   1. Conditional imports of arch-specific modules
--   2. Conditional definitions of arch-specific RegTarget records
--   3. A single dispatch function with conditional entries
--
-- This reduces CPP spread compared to having it in every function.
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
        RegTarget(..),
        selectRegTarget,
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
-- RegTarget record: bundles all arch-specific register operations

-- | Record containing all architecture-specific register operations.
-- Each supported architecture provides an implementation of this record.
data RegTarget = RegTarget
  { rtVirtualRegSqueeze :: !(RegClass -> VirtualReg -> Int)
    -- ^ Calculate max register colors denied to a node due to this virtual reg
  , rtRealRegSqueeze    :: !(RegClass -> RealReg -> Int)
    -- ^ Calculate max register colors denied to a node due to this real reg
  , rtClassOfRealReg    :: !(Platform -> RealReg -> RegClass)
    -- ^ Get the register class of a real register
  , rtMkVirtualReg      :: !(Unique -> Format -> VirtualReg)
    -- ^ Create a virtual register from a unique and format
  , rtRegDotColor       :: !(Platform -> RealReg -> SDoc)
    -- ^ Get the dot-graph color for a register (for debugging)
  }

-- -----------------------------------------------------------------------------
-- Architecture-specific RegTarget definitions

#if defined(HAVE_X86_NCG)
-- | X86/X86_64 register target
x86RegTarget :: RegTarget
x86RegTarget = RegTarget
  { rtVirtualRegSqueeze = X86.virtualRegSqueeze
  , rtRealRegSqueeze    = X86.realRegSqueeze
  , rtClassOfRealReg    = X86.classOfRealReg
  , rtMkVirtualReg      = X86.mkVirtualReg
  , rtRegDotColor       = X86.regDotColor
  }
#endif

#if defined(HAVE_AARCH64_NCG)
-- | AArch64 register target
aarch64RegTarget :: RegTarget
aarch64RegTarget = RegTarget
  { rtVirtualRegSqueeze = AArch64.virtualRegSqueeze
  , rtRealRegSqueeze    = AArch64.realRegSqueeze
  , rtClassOfRealReg    = \_ -> AArch64.classOfRealReg  -- AArch64 doesn't use Platform
  , rtMkVirtualReg      = AArch64.mkVirtualReg
  , rtRegDotColor       = \_ -> AArch64.regDotColor     -- AArch64 doesn't use Platform
  }
#endif

#if defined(HAVE_PPC_NCG)
-- | PowerPC register target
ppcRegTarget :: RegTarget
ppcRegTarget = RegTarget
  { rtVirtualRegSqueeze = PPC.virtualRegSqueeze
  , rtRealRegSqueeze    = PPC.realRegSqueeze
  , rtClassOfRealReg    = \_ -> PPC.classOfRealReg
  , rtMkVirtualReg      = PPC.mkVirtualReg
  , rtRegDotColor       = \_ -> PPC.regDotColor
  }
#endif

#if defined(HAVE_RISCV64_NCG)
-- | RISC-V 64-bit register target
rv64RegTarget :: RegTarget
rv64RegTarget = RegTarget
  { rtVirtualRegSqueeze = RV64.virtualRegSqueeze
  , rtRealRegSqueeze    = RV64.realRegSqueeze
  , rtClassOfRealReg    = \_ -> RV64.classOfRealReg
  , rtMkVirtualReg      = RV64.mkVirtualReg
  , rtRegDotColor       = \_ -> RV64.regDotColor
  }
#endif

#if defined(HAVE_LOONGARCH64_NCG)
-- | LoongArch64 register target
la64RegTarget :: RegTarget
la64RegTarget = RegTarget
  { rtVirtualRegSqueeze = LA64.virtualRegSqueeze
  , rtRealRegSqueeze    = LA64.realRegSqueeze
  , rtClassOfRealReg    = \_ -> LA64.classOfRealReg
  , rtMkVirtualReg      = LA64.mkVirtualReg
  , rtRegDotColor       = \_ -> LA64.regDotColor
  }
#endif

-- | RegTarget for unavailable/unsupported architectures
unavailableRegTarget :: String -> RegTarget
unavailableRegTarget archName = RegTarget
  { rtVirtualRegSqueeze = \_ _ -> panic $ "virtualRegSqueeze: " ++ archName ++ " not available"
  , rtRealRegSqueeze    = \_ _ -> panic $ "realRegSqueeze: " ++ archName ++ " not available"
  , rtClassOfRealReg    = \_ _ -> panic $ "classOfRealReg: " ++ archName ++ " not available"
  , rtMkVirtualReg      = \_ _ -> panic $ "mkVirtualReg: " ++ archName ++ " not available"
  , rtRegDotColor       = \_ _ -> panic $ "regDotColor: " ++ archName ++ " not available"
  }

-- -----------------------------------------------------------------------------
-- Platform dispatch

-- | Select the appropriate RegTarget for a platform.
-- This is the single point of platform-based dispatch.
selectRegTarget :: Platform -> RegTarget
selectRegTarget platform = case platformArch platform of
#if defined(HAVE_X86_NCG)
    ArchX86       -> x86RegTarget
    ArchX86_64    -> x86RegTarget
#else
    ArchX86       -> unavailableRegTarget "X86"
    ArchX86_64    -> unavailableRegTarget "X86_64"
#endif
#if defined(HAVE_AARCH64_NCG)
    ArchAArch64   -> aarch64RegTarget
#else
    ArchAArch64   -> unavailableRegTarget "AArch64"
#endif
#if defined(HAVE_PPC_NCG)
    ArchPPC       -> ppcRegTarget
    ArchPPC_64 _  -> ppcRegTarget
#else
    ArchPPC       -> unavailableRegTarget "PPC"
    ArchPPC_64 _  -> unavailableRegTarget "PPC_64"
#endif
#if defined(HAVE_RISCV64_NCG)
    ArchRISCV64   -> rv64RegTarget
#else
    ArchRISCV64   -> unavailableRegTarget "RISCV64"
#endif
#if defined(HAVE_LOONGARCH64_NCG)
    ArchLoongArch64 -> la64RegTarget
#else
    ArchLoongArch64 -> unavailableRegTarget "LoongArch64"
#endif
    -- Architectures without NCG support
    ArchS390X       -> unavailableRegTarget "S390X"
    ArchARM _ _ _   -> unavailableRegTarget "ARM"
    ArchAlpha       -> unavailableRegTarget "Alpha"
    ArchMipseb      -> unavailableRegTarget "Mipseb"
    ArchMipsel      -> unavailableRegTarget "Mipsel"
    ArchJavaScript  -> unavailableRegTarget "JavaScript"
    ArchWasm32      -> unavailableRegTarget "Wasm32"
    ArchUnknown     -> unavailableRegTarget "Unknown"

-- -----------------------------------------------------------------------------
-- Exported functions (simple wrappers around RegTarget dispatch)

-- | Calculate the maximum number of register colors that could be
-- denied to a node of this class due to having this virtual reg as a neighbour.
targetVirtualRegSqueeze :: Platform -> RegClass -> VirtualReg -> Int
targetVirtualRegSqueeze platform = rtVirtualRegSqueeze (selectRegTarget platform)

-- | Calculate the maximum number of register colors that could be
-- denied to a node of this class due to having this real reg as a neighbour.
targetRealRegSqueeze :: Platform -> RegClass -> RealReg -> Int
targetRealRegSqueeze platform = rtRealRegSqueeze (selectRegTarget platform)

-- | Get the register class of a real register.
targetClassOfRealReg :: Platform -> RealReg -> RegClass
targetClassOfRealReg platform = rtClassOfRealReg (selectRegTarget platform) platform

-- | Create a virtual register from a unique and format.
targetMkVirtualReg :: Platform -> Unique -> Format -> VirtualReg
targetMkVirtualReg platform = rtMkVirtualReg (selectRegTarget platform)

-- | Get the dot-graph color for a register (for debugging/visualization).
targetRegDotColor :: Platform -> RealReg -> SDoc
targetRegDotColor platform = rtRegDotColor (selectRegTarget platform) platform

-- -----------------------------------------------------------------------------
-- Helper functions (no CPP needed)

-- | Get the register class of any register (virtual or real).
targetClassOfReg :: Platform -> Reg -> RegClass
targetClassOfReg platform reg = case reg of
    RegVirtual vr -> classOfVirtualReg (platformArch platform) vr
    RegReal rr    -> targetClassOfRealReg platform rr

-- | Map a function over registers in a set, preserving formats.
mapRegFormatSet :: HasDebugCallStack => (Reg -> Reg) -> UniqSet RegWithFormat -> UniqSet RegWithFormat
mapRegFormatSet f = mapUniqSet (\(RegWithFormat r fmt) -> RegWithFormat (f r) fmt)
