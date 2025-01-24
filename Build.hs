#!/usr/bin/env runhaskell

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | GHC builder
--
-- Importantly, it doesn't link with the cabal library but use cabal-install
-- program instead (compared to e.g. Hadrian).
module Main (main) where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Tuple (swap)
import Distribution.Simple.GHC (getGhcInfo)
import Distribution.Simple.Program hiding (defaultProgramDb)
import Distribution.Simple.Utils (IOData (..), notice)
import Distribution.Verbosity as Verbosity
import System.Directory
import System.Environment
import System.FilePath

ghc0Program :: Program
ghc0Program =
    (simpleProgram "ghc")
        { programFindVersion = findProgramVersion "--numeric-version" id
        }

cabalProgram :: Program
cabalProgram =
    (simpleProgram "cabal")
        { programFindVersion = findProgramVersion "--numeric-version" id
        }

gitProgram :: Program
gitProgram = simpleProgram "git"

pythonProgram :: Program
pythonProgram = simpleProgram "python"

defaultProgramDb :: ProgramDb
defaultProgramDb = addKnownPrograms [ghc0Program, cabalProgram] emptyProgramDb

requireProgram' :: Verbosity -> MVar ProgramDb -> Program -> IO ConfiguredProgram
requireProgram' verbosity progdb program =
    modifyMVar progdb (fmap swap . requireProgram verbosity program)

userMaybeSpecifyPath' :: MVar ProgramDb -> String -> Maybe FilePath -> IO ()
userMaybeSpecifyPath' progdb name path = do
    modifyMVar_ progdb (return . userMaybeSpecifyPath name path)

main :: IO ()
main = do
    let verbosity = verboseNoWrap Verbosity.verbose

    progdb <- newMVar defaultProgramDb
    lookupEnv "GHC" >>= userMaybeSpecifyPath' progdb "ghc"
    lookupEnv "CABAL" >>= userMaybeSpecifyPath' progdb "cabal"

    modifyMVar_ progdb (configureAllKnownPrograms verbosity)
    ghc0 <- requireProgram' verbosity progdb ghc0Program
    cabal <- requireProgram' verbosity progdb cabalProgram

    notice verbosity $ "Bootstrapping GHC path: " ++ show (programPath ghc0)
    notice verbosity $ "Bootstrapping GHC version: " ++ show (programVersion ghc0)

    notice verbosity "Building stage1 GHC program and utility programs"
    buildGhcStage1 verbosity progdb defaultGhcBuildOptions cabal ghc0 "_build/stage0/"

    ghc1 <- requireProgram' verbosity progdb (simpleProgram "_build/stage0/bin/ghc")
    ghcPkg1 <- requireProgram' verbosity progdb (simpleProgram "_build/stage0/bin/ghc-pkg")
    deriveConstants <- requireProgram' verbosity progdb (simpleProgram "_build/stage0/bin/deriveConstants")
    genapply <- requireProgram' verbosity progdb (simpleProgram "_build/stage0/bin/genapply")
    genprimop <- requireProgram' verbosity progdb (simpleProgram "_build/stage0/bin/genprimopcode")

    notice verbosity "Building boot libraries with stage1 compiler..."
    buildBootLibraries verbosity progdb ghc1 ghcPkg1 deriveConstants genapply genprimop defaultGhcBuildOptions "_build/stage1/"

    notice verbosity "Done"

-- | Build stage1 GHC program
buildGhcStage1 :: Verbosity -> MVar ProgramDb -> GhcBuildOptions -> ConfiguredProgram -> ConfiguredProgram -> FilePath -> IO ()
buildGhcStage1 verbosity progdb opts cabal ghc0 dst = do
    let src = dst </> "src"
    prepareGhcSources verbosity progdb opts src

    let builddir = dst </> "cabal"
    createDirectoryIfMissing True builddir

    -- we need to augment the current environment to pass HADRIAN_SETTINGS
    -- environment variable to ghc-boot's Setup.hs script.
    stage0_settings <- getGhcInfo verbosity ghc0
    stage1_ghc_boot_settings <- do
        git <- requireProgram' verbosity progdb gitProgram
        commit_id <- getProgramOutput verbosity git ["rev-parse", "HEAD"]
        -- we infer stage1's host platform from stage0's settings
        let settings =
                [ ("hostPlatformArch", fromMaybe (error "Couldn't read 'target arch' setting") (lookup "target arch" stage0_settings))
                , ("hostPlatformOS", fromMaybe (error "Couldn't read 'target os' setting") (lookup "target os" stage0_settings))
                , ("cProjectGitCommitId", commit_id)
                , ("cProjectVersion", Text.unpack $ gboVersion opts)
                , ("cProjectVersionInt", Text.unpack $ gboVersionInt opts)
                , ("cProjectPatchLevel", Text.unpack $ gboVersionPatchLevel opts)
                , ("cProjectPatchLevel1", Text.unpack $ gboVersionPatchLevel1 opts)
                , ("cProjectPatchLevel2", Text.unpack $ gboVersionPatchLevel2 opts)
                ] ::
                    [(String, String)]
        pure (show settings)

    notice verbosity "  - Building GHC stage1 and bootstrapping utility programs..."

    let cabal_project_path = dst </> "cabal.project-stage0"
    makeCabalProject
        cabal_project_path
        [ "packages:"
        , "  " ++ src </> "ghc-bin/"
        , "  " ++ src </> "libraries/ghc/"
        , "  " ++ src </> "libraries/directory/"
        , "  " ++ src </> "libraries/file-io/"
        , "  " ++ src </> "libraries/filepath/"
        , "  " ++ src </> "libraries/ghc-platform/"
        , "  " ++ src </> "libraries/ghc-boot/"
        , "  " ++ src </> "libraries/ghc-boot-th/"
        , "  " ++ src </> "libraries/ghc-heap"
        , "  " ++ src </> "libraries/ghci"
        , "  " ++ src </> "libraries/os-string/"
        , "  " ++ src </> "libraries/process/"
        , "  " ++ src </> "libraries/semaphore-compat"
        , "  " ++ src </> "libraries/time"
        , "  " ++ src </> "libraries/unix/"
        , "  " ++ src </> "libraries/Win32/"
        , "  " ++ src </> "utils/ghc-pkg"
        , "  " ++ src </> "utils/hsc2hs"
        , "  " ++ src </> "utils/unlit"
        , "  " ++ src </> "utils/genprimopcode/"
        , "  " ++ src </> "utils/genapply/"
        , "  " ++ src </> "utils/deriveConstants/"
        , ""
        , "benchmarks: False"
        , "tests: False"
        , "allow-boot-library-installs: True"
        , ""
        , "package *"
        , "  library-vanilla: True"
        , "  shared: False"
        , "  executable-profiling: False"
        , "  executable-dynamic: False"
        , "  executable-static: True"
        , ""
        , "constraints:"
        , -- for some reason 2.23 doesn't build
          "  template-haskell <= 2.22"
        , ""
        , "package ghc-boot-th"
        , "  flags: +bootstrap"
        , ""
        , -- allow template-haskell with newer ghc-boot-th
          "allow-newer: ghc-boot-th"
        ]

    runProgram verbosity (cabal{programOverrideEnv = [("HADRIAN_SETTINGS", Just stage1_ghc_boot_settings)]}) 
            [ "install"
            , "--project-file=" ++ cabal_project_path
            , "--builddir=" ++ builddir
            , "-j"
            , "--with-compiler=" ++ programPath ghc0
            , "--installdir=" ++ (dst </> "bin") 
            , -- the targets
              "ghc-bin:ghc"
            , "ghc-pkg:ghc-pkg"
            , "genprimopcode:genprimopcode"
            , "deriveConstants:deriveConstants"
            , "genapply:genapply"
            ]

    -- initialize empty global package database
    pkgdb <- makeAbsolute (dst </> "pkgs")

    ghcpkg <- requireProgram' verbosity progdb $ simpleProgram (dst </> "bin/ghc-pkg")
    initEmptyDB verbosity ghcpkg pkgdb

    -- generate settings based on stage1 compiler settings
    createDirectoryIfMissing True (dst </> "lib")
    let stage1_settings = makeStage1Settings stage0_settings
    writeFile (dst </> "lib/settings") (show stage1_settings)

    -- try to run the stage1 compiler (no package db yet, so just display the
    -- version)
    ghc1 <- requireProgram' verbosity progdb $ simpleProgram (dst </> "bin/ghc")
    print ghc1

-- TODO:
-- - headers shared between different packages should be a common dependency
-- - event types should be generated by Setup.hs
-- package versions: do they need to be all the same?

-- | Prepare GHC sources in the given directory
prepareGhcSources :: Verbosity -> MVar ProgramDb -> GhcBuildOptions -> FilePath -> IO ()
prepareGhcSources verbosity progdb opts dst = do
    notice verbosity $ "  - Preparing sources in " ++ dst ++ "..."
    createDirectoryIfMissing True dst

    cp verbosity "./libraries" dst
    cp verbosity "./compiler" (dst </> "libraries/ghc")
    cp verbosity "./rts" (dst </> "libraries/rts")
    cp verbosity "./ghc" (dst </> "ghc-bin")
    cp verbosity "./utils" dst

    cp verbosity "./config.sub" (dst </> "libraries/rts")
    cp verbosity "./config.guess" (dst </> "libraries/rts")

    -- These needs to shared
    cp verbosity "rts/include/rts/Bytecodes.h" (dst </> "libraries/ghc")
    cp verbosity "rts/include/rts/storage/ClosureTypes.h" (dst </> "libraries/ghc")
    cp verbosity "rts/include/rts/storage/FunTypes.h" (dst </> "libraries/ghc")
    cp verbosity "rts/include/stg/MachRegs.h" (dst </> "libraries/ghc")
    cp verbosity "rts/include/stg/MachRegs" (dst </> "libraries/ghc/MachRegs")

    -- shared among ghc-internal rts and unlit
    cp verbosity "utils/fs/fs.c" (dst </> "libraries/ghc-internal/include")
    cp verbosity "utils/fs/fs.h" (dst </> "libraries/ghc-internal/cbits")
    cp verbosity "utils/fs/fs.c" (dst </> "libraries/rts/")
    cp verbosity "utils/fs/fs.h" (dst </> "libraries/rts/")
    cp verbosity "utils/fs/fs.c" (dst </> "utils/unlit/")
    cp verbosity "utils/fs/fs.h" (dst </> "utils/unlit/")

    python <- requireProgram' verbosity progdb pythonProgram

    runProgram
        verbosity
        python
        [ "rts/gen_event_types.py"
        , "--event-types-defines"
        , dst </> "libraries/rts/include/rts/EventLogConstants.h"
        ]

    runProgram
        verbosity
        python
        [ "rts/gen_event_types.py"
        , "--event-types-array"
        , dst </> "libraries/rts/include/rts/EventTypes.h"
        ]

    -- substitute variables in files
    let subst fin fout rs = do
            t <- Text.readFile fin
            Text.writeFile fout (List.foldl' (\v (needle, rep) -> Text.replace needle rep v) t rs)
    let subst_in f = subst (f <.> "in") f
    let common_substs =
            [ (,) "@ProjectVersion@" (gboVersion opts)
            , (,) "@ProjectVersionMunged@" (gboVersionMunged opts)
            , (,) "@ProjectVersionForLib@" (gboVersionForLib opts)
            , (,) "@ProjectPatchLevel1@" (gboVersionPatchLevel1 opts)
            , (,) "@ProjectPatchLevel2@" (gboVersionPatchLevel2 opts)
            , (,) "@ProjectVersionInt@" (gboVersionInt opts)
            ]
        llvm_substs =
            [ (,) "@LlvmMinVersion@" (gboLlvmMinVersion opts)
            , (,) "@LlvmMaxVersion@" (gboLlvmMaxVersion opts)
            ]
        boot_th_substs =
            [ (,) "@Suffix@" ""
            , (,) "@SourceRoot@" "."
            ]

    subst_in (dst </> "ghc-bin/ghc-bin.cabal") common_substs
    subst_in (dst </> "libraries/ghc/ghc.cabal") common_substs
    subst_in (dst </> "libraries/ghc-boot/ghc-boot.cabal") common_substs
    subst_in (dst </> "libraries/ghc-boot-th/ghc-boot-th.cabal") (common_substs ++ boot_th_substs)
    subst_in (dst </> "libraries/ghc-heap/ghc-heap.cabal") common_substs
    subst_in (dst </> "libraries/template-haskell/template-haskell.cabal") common_substs
    subst_in (dst </> "libraries/ghci/ghci.cabal") common_substs

    -- This is only used for a warning message. Nuke the check!
    subst_in (dst </> "libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs") llvm_substs

    subst_in (dst </> "utils/ghc-pkg/ghc-pkg.cabal") common_substs

    subst_in (dst </> "libraries/ghc-internal/ghc-internal.cabal") common_substs
    subst_in (dst </> "libraries/base/base.cabal") common_substs
    subst_in (dst </> "libraries/rts/include/ghcversion.h") common_substs

-- | Generate settings for stage1 compiler, based on given settings (stage0's
-- compiler settings)
makeStage1Settings :: [(String, String)] -> [(String, String)]
makeStage1Settings in_settings = out_settings
  where
    -- keep the previous setting, fail if it doesn't exist
    keep_fail s = keep_def s (error ("Couldn't find setting " <> show s))

    -- keep the previous setting, default to the given value if it doesn't exist
    keep_def s d = case lookup s in_settings of
        Nothing -> (s, d)
        Just v -> (s, v)

    -- use the previous setting, or if it doesn't exist use the setting for the
    -- second key. Fail if both don't exist. This is useful to support
    -- bootstrapping with old compilers that mingled some settings.
    keep_or_fail s s2 = case lookup s in_settings of
        Nothing -> case lookup s2 in_settings of
            Nothing -> error ("Couldn't find any of " <> show s <> " and " <> show s2)
            Just v -> (s, v)
        Just v -> (s, v)

    -- FIXME: we default to these flags for Cmm CPP, otherwise CPP fails
    -- with error: missing '(' after "__has_feature"
    -- because we pass `-traditional` while compiling Apply.cmm (in TSANUtils.h)
    default_cpp_flags = "-E"

    out_settings =
        [ keep_fail "C compiler command"
        , keep_fail "C compiler flags"
        , keep_fail "C++ compiler command"
        , keep_fail "C++ compiler flags"
        , keep_fail "C compiler link flags"
        , keep_fail "C compiler supports -no-pie"
        , keep_or_fail "CPP command" "Haskell CPP command"
        , keep_def "CPP flags" default_cpp_flags
        , keep_fail "Haskell CPP command"
        , keep_fail "Haskell CPP flags"
        , keep_or_fail "JavaScript CPP command" "Haskell CPP command"
        , keep_or_fail "JavaScript CPP flags" "Haskell CPP flags"
        , keep_or_fail "C-- CPP command" "Haskell CPP command"
        , keep_def "C-- CPP flags" default_cpp_flags
        , keep_def "C-- CPP supports -g0" "NO"
        , keep_fail "ld supports compact unwind"
        , keep_fail "ld supports filelist"
        , keep_fail "ld supports single module"
        , keep_fail "ld is GNU ld"
        , keep_fail "Merge objects command"
        , keep_fail "Merge objects flags"
        , keep_def "Merge objects supports response files" "NO"
        , keep_fail "ar command"
        , keep_fail "ar flags"
        , keep_fail "ar supports at file"
        , keep_fail "ar supports -L"
        , keep_fail "ranlib command"
        , keep_fail "otool command"
        , keep_fail "install_name_tool command"
        , keep_fail "windres command"
        , keep_fail "unlit command"
        , keep_fail "cross compiling"
        , keep_fail "target platform string"
        , keep_fail "target os"
        , keep_fail "target arch"
        , keep_fail "target word size"
        , keep_fail "target word big endian"
        , keep_fail "target has GNU nonexec stack"
        , keep_fail "target has .ident directive"
        , keep_fail "target has subsections via symbols"
        , keep_fail "target has libm"
        , keep_fail "Unregisterised"
        , keep_fail "LLVM target"
        , keep_fail "LLVM llc command"
        , keep_fail "LLVM opt command"
        , keep_def "LLVM llvm-as command" "llvm-as"
        , keep_fail "Use inplace MinGW toolchain"
        , keep_def "target RTS linker only supports shared libraries" "NO"
        , ("Use interpreter", "NO")
        , ("base unit-id", "base") -- there is no base yet... Anyway this isn't really useful to set
        , keep_fail "Support SMP"
        , keep_fail "RTS ways"
        , keep_fail "Tables next to code"
        , keep_fail "Leading underscore"
        , keep_fail "Use LibFFI"
        , keep_fail "RTS expects libdw"
        , ("Relative Global Package DB", "../pkgs")
        ]

buildBootLibraries ::
    Verbosity ->
    MVar ProgramDb ->
    -- | ghc
    ConfiguredProgram ->
    -- | ghc-pkg
    ConfiguredProgram ->
    -- | derive_constants
    ConfiguredProgram ->
    -- | genapply
    ConfiguredProgram ->
    -- | genprimop
    ConfiguredProgram ->
    GhcBuildOptions ->
    FilePath ->
    IO ()
buildBootLibraries verbosity progdb ghc ghcpkg derive_constants genapply genprimop opts dst = do
    src <- makeAbsolute (dst </> "src")
    prepareGhcSources verbosity progdb opts src

    -- Build the RTS
    src_rts <- makeAbsolute (src </> "libraries/rts")
    build_dir <- makeAbsolute (dst </> "cabal")
    ghcversionh <- makeAbsolute (src_rts </> "include/ghcversion.h")

    let cabal_project_rts_path = dst </> "cabal.project-rts"
    -- cabal's code handling escaping is bonkers. We need to wrap the whole
    -- option into \" otherwise it does weird things (like keeping only the
    -- last double-quote).
    let def_string k v = "  ghc-options: \"-optc-D" ++ k ++ "=\\\"" ++ v ++ "\\\"\""
    let rts_options =
            [ "package rts"
            , def_string "ProjectVersion" (Text.unpack (gboVersionInt opts))
            , def_string "RtsWay" "FIXME"
            , def_string "HostPlatform" "FIXME"
            , def_string "HostArch" "FIXME"
            , def_string "HostOS" "FIXME"
            , def_string "HostVendor" "FIXME"
            , def_string "BuildPlatform" "FIXME"
            , def_string "BuildArch" "FIXME"
            , def_string "BuildOS" "FIXME"
            , def_string "BuildVendor" "FIXME"
            , def_string "TargetPlatform" "FIXME"
            , def_string "TargetArch" "FIXME"
            , def_string "TargetOS" "FIXME"
            , def_string "TargetVendor" "FIXME"
            , def_string "GhcUnregisterised" "FIXME"
            , def_string "TablesNextToCode" "FIXME"
            , "  flags: +use-system-libffi" -- FIXME: deal with libffi (add package?)
            ]

    makeCabalProject cabal_project_rts_path $
        [ "package-dbs: clear, global"
        , ""
        , "packages:"
        , "  " ++ src </> "libraries/rts"
        , ""
        , "benchmarks: False"
        , "tests: False"
        , "allow-boot-library-installs: True"
        , "active-repositories: :none"
        , ""
        , "package *"
        , "  library-vanilla: True"
        , "  shared: False"
        , "  executable-profiling: False"
        , "  executable-dynamic: False"
        , "  executable-static: False"
        , ""
        ]
            ++ rts_options

    -- FIXME: deriveConstants requires ghcautoconf.h and ghcplatform.h but these
    -- files are generated by the configure script of the RTS...
    -- We use the following hack:
    --  1. run cabal until it fails. This should generate the headers we need before failing.
    --  2. use deriveConstants to generate the other files
    --  3. rerun cabal to build the rts

    notice verbosity "  - Generating headers and sources..."

    -- first run is expected to fail because of misssing headers
    cabal <- requireProgram' verbosity progdb cabalProgram
    runProgram
        verbosity
        cabal
        [ "build"
        , "--project-file=" ++ cabal_project_rts_path
        , "rts"
        , "--with-compiler=" ++ programPath ghc
        , "--with-hc-pkg=" ++ programPath ghcpkg
        , "--ghc-options=\"-ghcversion-file=" ++ ghcversionh ++ "\""
        , "--builddir=" ++ build_dir
        ]

    ghcplatform_dir <- takeDirectory <$> getSimpleProgramOutput verbosity "find" ["build_dir", "-name", "ghcplatform.h"]

    let derived_constants = src_rts </> "include/DerivedConstants.h"
    withSystemTempDirectory "derive-constants" $ \tmp_dir -> do
        runProgram
            verbosity
            derive_constants
            [ "--gen-header"
            , "-o"
            , derived_constants
            , "--target-os"
            , "linux" -- FIXME
            , "--tmpdir"
            , tmp_dir
            , "--gcc-program"
            , "gcc" -- FIXME
            , "--nm-program"
            , "nm" -- FIXME
            , "--objdump-program"
            , "objdump" -- FIXME
            , "--gcc-flag"
            , "-I" ++ src_rts </> "include"
            , "--gcc-flag"
            , "-I" ++ src_rts
            , "--gcc-flag"
            , "-I" ++ ghcplatform_dir
            ]

    -- Generate autoapply
    let run_genapply args out = writeFile out =<< getProgramOutput verbosity genapply args
    run_genapply [derived_constants] (src_rts </> "AutoApply.cmm")
    run_genapply [derived_constants, "-V16"] (src_rts </> "AutoApply_V16.cmm")
    run_genapply [derived_constants, "-V32"] (src_rts </> "AutoApply_V32.cmm")
    run_genapply [derived_constants, "-V64"] (src_rts </> "AutoApply_V64.cmm")

    -- Generate primop code for ghc-prim
    --
    -- Note that this can't be done in a Setup.hs for ghc-prim because
    -- cabal-install can't build Setup.hs because it depends on base, Cabal, etc.
    -- libraries that aren't built yet.
    let primops_txt = src </> "libraries/ghc/GHC/Builtin/primops.txt"
    let primops_txt_pp = primops_txt <.> ".pp"

    primops <- getSimpleProgramOutput verbosity "gcc" ["-E", "-undef", "-traditional", "-P", "-x", "c", primops_txt_pp]
    writeFile primops_txt primops

    writeFile (src </> "libraries/ghc-prim/GHC/Prim.hs")
        =<< getProgramInvocationOutput
            verbosity
            (programInvocation genprimop ["--make-haskell-source"])
                { progInvokeInput = Just (IODataText primops)
                }

    writeFile (src </> "libraries/ghc-prim/GHC/PrimopWrappers.hs")
        =<< getProgramInvocationOutput
            verbosity
            (programInvocation genprimop ["--make-haskell-wrappers"])
                { progInvokeInput = Just (IODataText primops)
                }

    notice verbosity "  - Building libffi..."
    src_libffi <- makeAbsolute (src </> "libffi")
    dst_libffi <- makeAbsolute (dst </> "libffi")
    let libffi_version = "3.4.6"
    createDirectoryIfMissing True src_libffi
    createDirectoryIfMissing True dst_libffi

    runSimpleProgram verbosity "tar" ["-xvf", "libffi-tarballs/libffi-" ++ libffi_version ++ ".tar.gz", "-C", src_libffi]

    let workdir = src_libffi </> "libffi-" ++ libffi_version
    runSimpleProgramCWD verbosity workdir "./configure" ["--disable-docs", "--with-pic=yes", "--disable-multi-os-directory", "--prefix=" ++ dst_libffi]
    runSimpleProgramCWD verbosity workdir "make" ["install", "-j"]

    cp verbosity (dst_libffi </> "include" </> "*") (src_rts </> "include")
    cp verbosity (dst_libffi </> "lib" </> "libffi.a") (takeDirectory ghcplatform_dir </> "libCffi.a")

    -- build boot libraries: ghc-internal, base... but not GHC itself
    let cabal_project_bootlibs_path = dst </> "cabal-project-boot-libs"
    makeCabalProject cabal_project_bootlibs_path $
        [ "package-dbs: clear, global"
        , ""
        , "packages:"
        , "  " ++ src </> "libraries/rts"
        , "  " ++ src </> "libraries/ghc-prim"
        , "  " ++ src </> "libraries/ghc-internal"
        , "  " ++ src </> "libraries/base"
        , "  " ++ src </> "libraries/ghc"
        , "  " ++ src </> "libraries/ghc-platform/"
        , "  " ++ src </> "libraries/ghc-boot/"
        , "  " ++ src </> "libraries/ghc-boot-th/"
        , "  " ++ src </> "libraries/ghc-heap"
        , "  " ++ src </> "libraries/ghci"
        , "  " ++ src </> "libraries/stm"
        , "  " ++ src </> "libraries/template-haskell"
        , "  " ++ src </> "libraries/hpc"
        , "  " ++ src </> "ghc-bin/"
        , "  " ++ src </> "utils/ghc-pkg"
        , "  " ++ src </> "utils/hsc2hs"
        , "  " ++ src </> "utils/unlit"
        , "  " ++ src </> "libraries/array"
        , "  " ++ src </> "libraries/binary"
        , "  " ++ src </> "libraries/bytestring"
        , "  " ++ src </> "libraries/containers/containers"
        , "  " ++ src </> "libraries/deepseq"
        , "  " ++ src </> "libraries/directory/"
        , "  " ++ src </> "libraries/exceptions"
        , "  " ++ src </> "libraries/file-io/"
        , "  " ++ src </> "libraries/filepath/"
        , "  " ++ src </> "libraries/mtl"
        , "  " ++ src </> "libraries/os-string/"
        , "  " ++ src </> "libraries/parsec"
        , "  " ++ src </> "libraries/pretty/"
        , "  " ++ src </> "libraries/process/"
        , "  " ++ src </> "libraries/semaphore-compat"
        , "  " ++ src </> "libraries/text"
        , "  " ++ src </> "libraries/time"
        , "  " ++ src </> "libraries/transformers"
        , "  " ++ src </> "libraries/unix/"
        , "  " ++ src </> "libraries/Win32/"
        , "  " ++ src </> "libraries/Cabal/Cabal-syntax"
        , "  " ++ src </> "libraries/Cabal/Cabal"
        , "  https://github.com/haskell/alex/archive/refs/tags/v3.5.2.0.tar.gz"
        , ""
        , "benchmarks: False"
        , "tests: False"
        , "allow-boot-library-installs: True"
        , "active-repositories: :none"
        , ""
        , "package *"
        , "  library-vanilla: True"
        , "  shared: False"
        , "  executable-profiling: False"
        , "  executable-dynamic: False"
        , "  executable-static: False"
        , ""
        , "package ghc-internal"
        , -- FIXME: make our life easier for now by using the native bignum backend
          "  flags: +bignum-native"
        , ""
        , "package text"
        , -- FIXME: avoid having to deal with system-cxx-std-lib fake package for now
          "  flags: -simdutf"
        , ""
        ]
            ++ rts_options

    let boot_libs_env = dst </> "boot-libs.env"

    notice verbosity "  - Building boot libraries..."
    runProgram
        verbosity
        cabal
        [ "install"
        , "--lib"
        , "--package-env=" ++ boot_libs_env
        , "--force-reinstalls"
        , "-v3"
        , -- [ "build"
          "--project-file=" ++ cabal_project_bootlibs_path
        , "--with-compiler=" ++ programPath ghc
        , "--with-hc-pkg=" ++ programPath ghcpkg
        , "--ghc-options=\"-ghcversion-file=" ++ ghcversionh ++ "\""
        , "--builddir=" ++ build_dir
        , "-j"
        , -- targets
          "rts"
        , "ghc-prim"
        , "ghc-internal"
        , "base"
        ]

    -- The libraries have been installed globally.
    (global_gb : pkg_ids) <- (map (drop 11) . drop 2 . lines) <$> readFile boot_libs_env
    putStrLn $ "We've built boot libraries in " ++ global_gb ++ ":"
    mapM_ (putStrLn . ("  - " ++)) pkg_ids

-- TODO: copy the libs in another db

---------------------------
-- Options
---------------------------

data GhcBuildOptions = GhcBuildOptions
    { gboVersion :: !Text
    -- ^ GHC version
    , gboVersionInt :: !Text
    -- ^ GHC version as an Int
    , gboVersionMunged :: !Text
    -- ^ GHC version "munged"
    , gboVersionForLib :: !Text
    -- ^ GHC version for libraries?
    , gboVersionPatchLevel :: !Text
    -- ^ GHC patchlevel version
    , gboVersionPatchLevel1 :: !Text
    -- ^ GHC patchlevel1 version
    , gboVersionPatchLevel2 :: !Text
    -- ^ GHC patchlevel2 version
    , gboLlvmMinVersion :: !Text
    -- ^ Min LLVM version supported
    , gboLlvmMaxVersion :: !Text
    -- ^ Max LLVM version supported
    }

defaultGhcBuildOptions :: GhcBuildOptions
defaultGhcBuildOptions =
    GhcBuildOptions
        { gboVersion = "9.13"
        , gboVersionInt = "913"
        , gboVersionMunged = "9.13"
        , gboVersionForLib = "9.1300"
        , gboVersionPatchLevel = "0"
        , gboVersionPatchLevel1 = "0"
        , gboVersionPatchLevel2 = "0"
        , gboLlvmMinVersion = "13"
        , gboLlvmMaxVersion = "20"
        }

---------------------------
-- Utilities
---------------------------

cp :: Verbosity -> String -> String -> IO ()
cp verbosity src dst =
    runProgramInvocation verbosity $
        simpleProgramInvocation "cp" ["-rf", src, dst]

makeCabalProject :: FilePath -> [String] -> IO ()
makeCabalProject path xs = writeFile path $ unlines (xs ++ common)
  where
    common =
        [ ""
        , "program-options"
        , "  ghc-options: -fhide-source-paths -j"
        ]

withSystemTempDirectory :: String -> (String -> IO a) -> IO a
withSystemTempDirectory prefix = do
    bracket
        ( do
            tmpdir <- getTemporaryDirectory
            let dir = tmpdir </> prefix
            createDirectory dir
            return dir
        )
        removeDirectoryRecursive

initEmptyDB :: Verbosity -> ConfiguredProgram -> FilePath -> IO ()
initEmptyDB verbosity ghcPkg pkgdb = do
    exists <- doesDirectoryExist pkgdb
    -- don't try to recreate the DB if it already exist as it would fail
    unless exists $  runProgram verbosity ghcPkg ["init", pkgdb]

runSimpleProgram :: Verbosity -> FilePath -> [String] -> IO ()
runSimpleProgram verbosity path args =
    runProgramInvocation verbosity $
        simpleProgramInvocation path args

getSimpleProgramOutput :: Verbosity -> FilePath -> [String] -> IO String
getSimpleProgramOutput verbosity path args =
    getProgramInvocationOutput verbosity $
        simpleProgramInvocation path args

runSimpleProgramCWD :: Verbosity -> FilePath -> FilePath -> [String] -> IO ()
runSimpleProgramCWD verbosity workdir path args =
    runProgramInvocation verbosity $
        (simpleProgramInvocation path args){progInvokeCwd = Just workdir}

