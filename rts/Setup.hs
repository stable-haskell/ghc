{-# OPTIONS_GHC -Wall #-}

import Data.Map.Strict qualified as Map
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program (Program, gccProgram, programPath, requireProgram, runProgram, simpleProgram)
import Distribution.Simple.Setup
import Distribution.Types.BuildInfo as BI
import Distribution.Types.InstalledPackageInfo as IPI
import Distribution.Types.Library
import Distribution.Types.PackageDescription
import Distribution.Verbosity (normal)

nmProgram :: Program
nmProgram = simpleProgram "nm"

objdumpProgram :: Program
objdumpProgram = simpleProgram "objdump"

deriveConstantsProgram :: Program
deriveConstantsProgram = simpleProgram "deriveConstants"

main :: IO ()
main =
    defaultMainWithHooks
        autoconfUserHooks
            { postConf = \args flags pd lbi -> do
                postConf autoconfUserHooks args flags pd lbi
                
                let verbosity = fromFlagOrDefault normal (configVerbosity flags)
                let progdb = withPrograms lbi
                (nm, _progdb) <- requireProgram verbosity nmProgram progdb
                (objdump, _progdb) <- requireProgram verbosity objdumpProgram progdb
                (gcc, _progdb) <- requireProgram verbosity gccProgram progdb
                (deriveConstants, _progdb) <- requireProgram verbosity deriveConstantsProgram progdb

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


                runProgram verbosity deriveConstants $
                    [ "--gen-header"
                    , "-o"
                    , "include/DerivedConstants.h"
                    , "--target-os"
                    , "linux"
                    , "--gcc-program"
                    , programPath gcc
                    , "--nm-program"
                    , programPath nm
                    , "--objdump-program"
                    , programPath objdump
                    , "--tmpdir"
                    , buildDir lbi
                    ]
                        ++ foldMap (\i -> ["--gcc-flag", "-I" ++ i]) thisIncDirs
                        ++ foldMap (\incdir -> ["--gcc-flag", "-I" ++ incdir]) depsIncDirs
            }
