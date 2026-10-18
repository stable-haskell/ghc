module Main where

import Prelude hiding ((<>))

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import GHC.Cmm.CLabel
import GHC.Cmm.DebugBlock
import GHC.Cmm.Expr
import GHC.CmmToAsm.Dwarf.Constants
import GHC.CmmToAsm.Dwarf.Types
import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Regs (globalRegMaybe)
import GHC.Types.Unique
import GHC.Utils.Outputable
import System.Environment (getArgs)
import qualified System.Info as Host

platform :: Arch -> OS -> Platform
platform arch os = genericPlatform
  { platformArchOS = ArchOS arch os
  , platformLeadingUnderscore = os == OSDarwin
  }

proc, point, point2, cie :: CLabel
[proc, point, point2, cie] = map (mkAsmTempLabel . mkUnique 'u') [1..4]

frame :: Platform -> DwarfFrame
frame p = DwarfFrame cie initial
  [DwarfFrameProc proc False [DwarfFrameBlock False
    [ UnwindPoint point (Map.fromList
        [ (Sp, Just (reg Sp (-8)))
        , (DoubleReg 1, Just (UwDeref (reg Sp (-8))))
        , (DoubleReg 2, Just (reg (DoubleReg 3) 8))
        , (DoubleReg 3, Just (UwDeref (reg Sp 16)))
        , (DoubleReg 4, Just (UwDeref (reg Sp 3)))
        ])
    , UnwindPoint point2 (Map.singleton Sp (Just (reg SpLim (-8))))
    ]]]
  where
    reg g n = UwReg (GlobalRegUse g (bWord p)) n
    initial = Map.singleton Sp (Just (reg Sp 0))

normalise :: String -> String
normalise = filter (not . isSpace)

check :: Platform -> IO ()
check p = do
  let asm = normalise (showSDocUnsafe (pprDwarfFrame p (frame p)))
      has fragments = unless (normalise (concat fragments) `isInfixOf` asm) $
        error ("Missing DWARF encoding: " ++ show fragments ++ "\n" ++ asm)
      byte n = ".byte " ++ show n
      uleb n = byte n
      sleb n = byte (n `mod` 128)
      global g = maybe (error "Missing global register") (dwarfRegNo p . RegReal) (globalRegMaybe p g)
      d n = global (DoubleReg n)
  -- Both signed CFA opcodes use the CIE's -8 data alignment, not bytes.
  has [byte dW_CFA_def_cfa_offset_sf, sleb (1 :: Int)]
  has [byte dW_CFA_def_cfa_sf, uleb (global SpLim), sleb (1 :: Int)]
  has [byte dW_CFA_offset_extended_sf, uleb (d 3), sleb (-2 :: Int)]
  if platformArch p == ArchAArch64
    then do
      unless (map (dwarfRegNo p . regSingle) [0,31,32,63] == [0,31,64,95]) $
        error "Incorrect AArch64 DWARF register mapping"
      has [byte dW_CFA_offset_extended, uleb (d 1), uleb (1 :: Int)]
      has [byte dW_OP_bregx, uleb (d 3), sleb (8 :: Int)]
    else do
      -- Existing x86 compact encodings must remain compact and unchanged.
      has [byte (dW_CFA_offset + d 1), uleb (1 :: Int)]
      has [byte (dW_OP_breg0 + d 3), sleb (8 :: Int)]

emit :: Platform -> IO ()
emit p = putStrLn $ showSDocUnsafe $
  text ".text" $$
  label proc $$ text "nop" $$ label point $$ text "nop" $$
  label point2 $$ text "nop" $$ label (mkAsmTempProcEndLabel proc) $$
  dwarfFrameSection p $$ dwarfSectionAnchor p "frame" $$
  (dwarfFrameLabel p <> colon) $$ pprDwarfFrame p (frame p)
  where label l = pprAsmLabel p l <> colon

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--emit-aarch64-macho"] -> emit (platform ArchAArch64 OSDarwin)
    ["--emit-aarch64-elf"] -> emit (platform ArchAArch64 OSLinux)
    ["--emit-x86-elf"] -> emit (platform ArchX86_64 OSLinux)
    ["--check-all"] -> do
      check (platform ArchAArch64 OSLinux)
      check (platform ArchX86_64 OSLinux)
    [] -> do
      check (platform ArchAArch64 OSLinux)
      if Host.arch == "x86_64" then check (platform ArchX86_64 OSLinux) else pure ()
    _ -> error "Unexpected probe arguments"
