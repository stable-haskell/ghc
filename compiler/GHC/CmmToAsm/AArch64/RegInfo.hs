module GHC.CmmToAsm.AArch64.RegInfo where

import GHC.Prelude

import GHC.CmmToAsm.AArch64.Instr
import GHC.Cmm.BlockId
import GHC.Cmm

import GHC.Utils.Outputable

data JumpDest = DestBlockId BlockId

-- Debug Instance
instance Outputable JumpDest where
  ppr (DestBlockId bid) = text "jd<blk>:" <> ppr bid

-- Instances the methods of the same name in 'NgcImpl'

getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid

canShortcut :: Instr -> Maybe JumpDest
canShortcut _ = Nothing

shortcutStatics :: (BlockId -> Maybe JumpDest) -> RawCmmStatics -> RawCmmStatics
shortcutStatics _ other_static = other_static

shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump _ other = other
