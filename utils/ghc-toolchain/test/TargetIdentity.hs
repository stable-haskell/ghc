import Prelude
import Control.Monad (forM_)

import GHC.Platform.ArchOS
import GHC.Toolchain.Program (Program(..))
import GHC.Toolchain.Target
import GHC.Toolchain.Tools.Ar
import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Cpp
import GHC.Toolchain.Tools.Cxx
import GHC.Toolchain.Tools.Link
import GHC.Toolchain.Tools.Nm
import GHC.Toolchain.Tools.Ranlib

main :: IO ()
main = do
    let triple = "x86_64-unknown-linux-gnu"
        program = Program "cc" ["--target=x86_64-unknown-linux-gnu"]
        target = Target
            { tgtPlatformTriple = triple
            , tgtArchOs = ArchOS ArchX86_64 OSLinux
            , tgtVendor = Just "unknown"
            , tgtLocallyExecutable = True
            , tgtSupportsGnuNonexecStack = True
            , tgtSupportsSubsectionsViaSymbols = False
            , tgtSupportsIdentDirective = True
            , tgtWordSize = WS8
            , tgtEndianness = LittleEndian
            , tgtSymbolsHaveLeadingUnderscore = False
            , tgtLlvmTarget = "x86_64-unknown-linux-gnu"
            , tgtUnregisterised = False
            , tgtTablesNextToCode = True
            , tgtUseLibffiForAdjustors = True
            , tgtCCompiler = Cc program
            , tgtCxxCompiler = Cxx program
            , tgtCPreprocessor = Cpp program
            , tgtHsCPreprocessor = HsCpp program
            , tgtJsCPreprocessor = Nothing
            , tgtCmmCPreprocessor = CmmCpp program True
            , tgtCCompilerLink = CcLink program False False False False True False
            , tgtAr = Ar program True True True False
            , tgtRanlib = Just (Ranlib program)
            , tgtNm = Nm program
            , tgtMergeObjs = Nothing
            , tgtLlc = Nothing
            , tgtOpt = Nothing
            , tgtLlvmAs = Nothing
            , tgtWindres = Nothing
            , tgtOtool = Nothing
            , tgtInstallNameTool = Nothing
            }
        restored = read (show target) :: Target
        rows = targetIdentitySettings restored
        restoredRows = read (show rows) :: [(String, String)]
    assert "Target Show/Read lost the complete machine triple" $
        targetPlatformTriple restored == triple
    assert "settings roundtrip lost complete target identity" $
        restoredRows ==
          [ ("target platform string", triple)
          , ("target name", triple)
          , ("target ABI", "")
          ]
    forM_ [ ("arm-unknown-linux-gnueabi", "-mabi=aapcs-linux", "armv5te-unknown-linux-gnueabi")
          , ("arm-unknown-linux-gnueabihf", "-mfloat-abi=hard", "armv7-unknown-linux-gnueabihf")
          , ("aarch64-apple-darwin", "-arch", "arm64-apple-darwin")
          , ("wasm32-unknown-wasi", "--target=wasm32-wasi", "wasm32-unknown-wasi")
          ] $ \(machine, flag, llvm) -> do
        let candidate = target
              { tgtPlatformTriple = machine
              , tgtCCompiler = Cc (Program "cc" [flag])
              , tgtLlvmTarget = llvm
              }
            roundtrip = read (show candidate) :: Target
            identity = read (show (targetIdentitySettings roundtrip)) :: [(String, String)]
        assert ("machine triple roundtrip failed for " ++ machine) $
            targetPlatformTriple roundtrip == machine &&
            identity == [ ("target platform string", machine)
                        , ("target name", machine)
                        , ("target ABI", "")
                        ]
        assert ("tool flags or LLVM target changed for " ++ machine) $
            prgFlags (ccProgram (tgtCCompiler roundtrip)) == [flag] &&
            tgtLlvmTarget roundtrip == llvm

assert :: String -> Bool -> IO ()
assert message ok = if ok then pure () else fail message
