module Main where

import Control.Monad (unless)
import GHC.Cmm
import GHC.Cmm.BlockId (blockLbl, mkBlockId)
import GHC.Cmm.CLabel
import GHC.CmmToAsm.X86.Instr
import GHC.CmmToAsm.X86.Regs (Imm(..))
import GHC.Data.FastString (fsLit)
import GHC.Types.Basic (FunctionOrData(..), mkAlignment)
import GHC.Types.Unique (mkUnique)

main :: IO ()
main = do
    -- Relative statics follow a local chain, but stop at the tail-call block.
    check "relative table shortcuts past its trampoline" $
        case statics (CmmLabelDiffOff (blockLbl first) base 7 W64) of
            CmmLabelDiffOff target base' 7 W64 ->
                target == blockLbl trampoline && base' == base
            _ -> False
    check "absolute table no longer shortcuts to the external target" $
        case statics (CmmLabel (blockLbl first)) of
            CmmLabel target -> target == external
            _ -> False
    -- JMP_TBL carries its own target list as well as the emitted statics.
    check "relative JMP_TBL shortcuts past its trampoline" $
        case shortcutJump mapping (table (Just base)) of
            JMP_TBL _ [Just (DestBlockId target)] _ _ _ -> target == trampoline
            _ -> False
    check "absolute JMP_TBL no longer shortcuts to the external target" $
        case shortcutJump mapping (table Nothing) of
            JMP_TBL _ [Just (DestImm (ImmCLbl target))] _ _ _ -> target == external
            _ -> False
  where
    first = mkBlockId (mkUnique 't' 1)
    trampoline = mkBlockId (mkUnique 't' 2)
    base = blockLbl (mkBlockId (mkUnique 't' 3))
    tableLabel = blockLbl (mkBlockId (mkUnique 't' 4))
    external = mkForeignLabel (fsLit "external_info")
                             ForeignLabelInThisPackage IsFunction
    mapping block
        | block == first = Just (DestBlockId trampoline)
        | block == trampoline = Just (DestImm (ImmCLbl external))
        | otherwise = Nothing
    statics lit =
        case shortcutStatics mapping (mkAlignment 1,
                CmmStaticsRaw tableLabel [CmmStaticLit lit]) of
            (_, CmmStaticsRaw _ [CmmStaticLit lit']) -> lit'
            _ -> error "unexpected jump-table statics"
    table relative = JMP_TBL (OpImm (ImmInt 0))
        [Just (DestBlockId trampoline)] (Section ReadOnlyData tableLabel)
        tableLabel relative
    check message success = unless success (error message)
