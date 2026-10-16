module Main (main) where

import GHC
import GHC.CmmToLlvm.Config
import GHC.CmmToLlvm.Version.Type
import GHC.Driver.Config.CmmToLlvm
import GHC.Driver.Session
import GHC.Platform

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    liftIO $ checkFeatures flags

checkFeatures :: DynFlags -> IO ()
checkFeatures flags = do
  -- Model target defaults that must survive a function-level attribute:
  -- OpenBSD's retpolines and Android's cx16, without needing a cross compiler.
  let defaults = [ "+retpoline-indirect-calls", "+retpoline-indirect-branches"
                 , "+cx16", "-sse4.2", "+avx", "-avx" ]
      triple = "test-target-features"
      config = LlvmConfig [(triple, LlvmTarget "" "" defaults)] []
      platform = (targetPlatform flags)
        { platformArchOS = ArchOS ArchX86_64 OSLinux }
      baseline = flags
        { targetPlatform = platform
        , platformMisc = (platformMisc flags) { platformMisc_llvmTarget = triple }
        , sseVersion = Nothing, bmiVersion = Nothing
        , avx = False, avx2 = False, avx512f = False
        , avx512cd = False, avx512er = False, avx512pf = False, fma = False }
      version n = Just (LlvmVersion (n :| []))
      moduleFeatures = llvmTargetFeatureList config
        baseline { sseVersion = Just SSE42, avx2 = True } (version 18)
      procFeatures = llvmAddTargetFeatures (version 18) moduleFeatures
        ["+avx", "+avx2"]
      enabled name features =
        case filter (`elem` ['+' : name, '-' : name]) features of
          [] -> False
          fs -> last fs == '+' : name
      check label ok = unless ok (fail label)

  check "lost LLVM target defaults" (take (length defaults) moduleFeatures == defaults)
  check "lost global CPU features"
    (all (`elem` moduleFeatures) ["+sse4.2", "+popcnt", "+avx2", "+avx"])
  check "global feature did not override target default" (enabled "sse4.2" moduleFeatures)
  check "lost target defaults on attributed procedure"
    (take (length defaults) procFeatures == defaults)
  check "local requirement did not override disabled feature" (enabled "avx" procFeatures)
  check "feature deduplication changed last-occurrence precedence"
    (llvmAddTargetFeatures Nothing ["+avx", "-avx"] ["+avx"]
      == ["+avx", "-avx", "+avx"])

  -- A local AVX-512 requirement must use the same LLVM-version policy as
  -- global -mavx512f, including when the LLVM version cannot be determined.
  mapM_ (\(v, evex) -> do
    let local = llvmAddTargetFeatures v [] ["+avx", "+avx2", "+avx512f"]
        global = llvmTargetFeatureList config baseline { avx512f = True } v
    check "inconsistent local AVX-512 features" (("+evex512" `elem` local) == evex)
    check "inconsistent global AVX-512 features" (("+evex512" `elem` global) == evex)
    check "lost AVX-512 implications"
      (all (`elem` global) ["+avx", "+avx2", "+avx512f"]))
    [(Nothing, False), (version 17, False), (version 18, True), (version 21, True)]
