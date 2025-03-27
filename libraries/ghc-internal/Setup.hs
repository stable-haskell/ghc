{-# LANGUAGE CPP #-}
module Main where

import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentName (ComponentName(CLibName))
import Distribution.Types.LocalBuildInfo
import Distribution.Types.LibraryName (LibraryName(LMainLibName))
import Distribution.Verbosity
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Simple.Setup
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
#endif

import System.IO
import System.Process
import System.Directory
import System.FilePath
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import GHC.ResponseFile
import System.Environment

main :: IO ()
main = defaultMainWithHooks ghcHooks
  where
    ghcHooks = autoconfUserHooks
      { postConf = \args cfg pd lbi -> do
          let verbosity = fromFlagOrDefault minBound (configVerbosity cfg)
          ghcAutogen verbosity lbi
          postConf autoconfUserHooks args cfg pd lbi
      }

ghcAutogen :: Verbosity -> LocalBuildInfo -> IO ()
ghcAutogen verbosity lbi = do

#if MIN_VERSION_Cabal(3,14,0)
    let fromSymPath = interpretSymbolicPathLBI lbi
#else
    let fromSymPath = id
#endif
    notice verbosity "Running ghc-internal pre-build hook: Generating Prim modules..."

    -- Get GHC path from LocalBuildInfo
    let compilerInfo = compiler lbi
    -- ghcPath <- case compilerFlavor compilerInfo of
    --              GHC -> return $ compilerPath compilerInfo
    --              _   -> fail "This setup script requires GHC."

    (ghc   ,withPrograms) <- requireProgram normal ghcProgram (withPrograms lbi)

    -- Get autogen directory
    let autogenDir = fromSymPath (autogenPackageModulesDir lbi)
    let internalDir = autogenDir </> "GHC" </> "Internal"

    -- Ensure target directory exists
    createDirectoryIfMissing True internalDir

    -- Define output file paths
    let primHsPath = internalDir </> "Prim.hs"
    let primWrappersHsPath = internalDir </> "PrimopWrappers.hs"

    -- Run GHC commands
    notice verbosity $ "Generating " ++ primHsPath
    getProgramOutput normal ghc ["--print-prim-module"] >>= rewriteFileEx verbosity primHsPath

    notice verbosity $ "Generating " ++ primWrappersHsPath
    getProgramOutput normal ghc ["--print-prim-wrappers-module"] >>= rewriteFileEx verbosity primWrappersHsPath
