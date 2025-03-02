{-# OPTIONS_GHC -Wall #-}

import Data.Map.Strict qualified as Map
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program (Program, gccProgram, programPath, requireProgram, runProgram, getProgramOutput, simpleProgram)
import Distribution.Simple.Setup
import Distribution.Types.BuildInfo as BI
import Distribution.Types.InstalledPackageInfo as IPI
import Distribution.Types.Library
import Distribution.Types.PackageDescription
import Distribution.Utils.Path (interpretSymbolicPathCWD, (</>), makeRelativePathEx)
import Distribution.Verbosity (normal)
import System.Directory (getCurrentDirectory)
import System.Info (os)

nmProgram :: Program
nmProgram = simpleProgram "nm"

objdumpProgram :: Program
objdumpProgram = simpleProgram "objdump"

deriveConstantsProgram :: Program
deriveConstantsProgram = simpleProgram "deriveConstants"

genapplyProgram :: Program
genapplyProgram = simpleProgram "genapply"

main :: IO ()
main =
    defaultMainWithHooks
        autoconfUserHooks
            { postConf = \args flags pd lbi -> do
                postConf autoconfUserHooks args flags pd lbi

                let verbosity = fromFlagOrDefault normal (configVerbosity flags)
                    progdb = withPrograms lbi

                (nm, _progdb) <- requireProgram verbosity nmProgram progdb
                (objdump, _progdb) <- requireProgram verbosity objdumpProgram progdb
                (gcc, _progdb) <- requireProgram verbosity gccProgram progdb
                (deriveConstants, _progdb) <- requireProgram verbosity deriveConstantsProgram progdb
                (genapply, _progdb) <- requireProgram verbosity genapplyProgram progdb

                -- Include dirs for this package
                let thisIncDirs = foldMap (BI.includeDirs . libBuildInfo) (allLibraries pd)

                -- \^ Include dirs for dependencies
                let depsIncDirs =
                        [ incdir
                        | Just clbis <- [Map.lookup (CLibName defaultLibName) (componentNameMap lbi)]
                        , clbi <- clbis
                        , (unitId, _) <- componentPackageDeps clbi
                        , Just ipi <- [lookupUnitId (installedPkgs lbi) unitId]
                        , incdir <- IPI.includeDirs ipi
                        ]

                let derivedConstantsH = interpretSymbolicPathCWD (buildDir lbi </> makeRelativePathEx "include/DerivedConstants.h")

                -- The fact that we can't just run some pre-gen shell script is really annoying.
                -- why does everything need to be haskell?
                getCurrentDirectory >>= putStrLn
                runProgram verbosity deriveConstants $
                    [ "--gen-header"
                    , "-o"
                    , derivedConstantsH
                    , "--target-os"
                    , os
                    , "--gcc-program"
                    , programPath gcc
                    , "--nm-program"
                    , programPath nm
                    , "--objdump-program"
                    , programPath objdump
                    , "--tmpdir"
                    , interpretSymbolicPathCWD (buildDir lbi)
                    , "--gcc-flag"
                    , "-I" ++ interpretSymbolicPathCWD (buildDir lbi </> makeRelativePathEx "include")
                    -- pass `-fcommon` to force symbols into the common section. If they
                    -- end up in the ro data section `nm` won't list their size, and thus
                    -- derivedConstants will fail. Recent clang (e.g. 16) will by default
                    -- use `-fno-common`.
                    , "--gcc-flag", "-fcommon"
                    ]
                        ++ foldMap ((\i -> ["--gcc-flag", "-I" ++ i]) . interpretSymbolicPathCWD) thisIncDirs
                        ++ foldMap (\incdir -> ["--gcc-flag", "-I" ++ incdir]) depsIncDirs
                getProgramOutput verbosity genapply [derivedConstantsH]
                    >>= writeFile (interpretSymbolicPathCWD (buildDir lbi </> makeRelativePathEx "AutoApply.cmm"))
                getProgramOutput verbosity genapply [derivedConstantsH, "-V16"]
                    >>= writeFile (interpretSymbolicPathCWD (buildDir lbi </> makeRelativePathEx "AutoApply_V16.cmm"))
                getProgramOutput verbosity genapply [derivedConstantsH, "-V32"]
                    >>= writeFile (interpretSymbolicPathCWD (buildDir lbi </> makeRelativePathEx "AutoApply_V32.cmm"))
                getProgramOutput verbosity genapply [derivedConstantsH, "-V64"]
                    >>= writeFile (interpretSymbolicPathCWD (buildDir lbi </> makeRelativePathEx "AutoApply_V64.cmm"))

            }
