{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Main where

import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Types.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Simple.Setup
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
#endif

import System.IO
import System.Directory
import System.FilePath
import System.Environment
import Control.Monad
import Data.Char
import GHC.ResponseFile
import Distribution.System (Platform(..))

main :: IO ()
main = defaultMainWithHooks ghcHooks
  where
    ghcHooks = simpleUserHooks
      { confHook = \(gpd, hbi) cfg -> do
          let verbosity = fromFlagOrDefault minBound (configVerbosity cfg)
          lbi <- confHook simpleUserHooks (gpd, hbi) cfg
          gitCommitId <- lookupEnv "GIT_COMMIT_ID" >>= \case
            Just str -> return str
            Nothing -> do
              (git, progdb) <- requireProgram verbosity (simpleProgram "git") defaultProgramDb
              getProgramOutput verbosity git ["rev-parse", "HEAD"]
          info verbosity $ "Git Commit Id = " ++ gitCommitId
          let cfs = configFlags lbi
              cPa = configProgramArgs cfs ++ [("ghc", ["-D GIT_COMMIT_ID=" ++ gitCommitId])]
          return lbi { configFlags = cfs { configProgramArgs = cPa } }

      , postConf = \args cfg pd lbi -> do
          let verbosity = fromFlagOrDefault minBound (configVerbosity cfg)
          ghcAutogen verbosity lbi
          postConf simpleUserHooks args cfg pd lbi
      }

ghcAutogen :: Verbosity -> LocalBuildInfo -> IO ()
ghcAutogen verbosity lbi@LocalBuildInfo {hostPlatform, pkgDescrFile} = do
#if MIN_VERSION_Cabal(3,14,0)
  let fromSymPath = interpretSymbolicPathLBI lbi
#else
  let fromSymPath = id
#endif

  -- Get compiler/ root directory from the cabal file
  let Just compilerRoot = takeDirectory . fromSymPath <$> pkgDescrFile

  let platformHostFile = "GHC/Platform/Host.hs"
      platformHostPath = fromSymPath (autogenPackageModulesDir lbi) </> platformHostFile
  -- Write GHC.Platform.Host
  createDirectoryIfMissingVerbose verbosity True (takeDirectory platformHostPath)

  -- hostPlatform is listed in LocalBuildInfo as "the platform we are building for"
  let Platform arch os = hostPlatform

  rewriteFileEx verbosity platformHostPath $
    unlines
        [ "module GHC.Platform.Host where"
        , ""
        , "import GHC.Platform.ArchOS"
        , ""
        , "-- terrbile"
        , "hostPlatformArch :: Arch"
        , "hostPlatformArch = Arch" ++ show arch
        , ""
        , "-- terrbile"
        , "hostPlatformOS   :: OS"
        , "hostPlatformOS   = OS" ++ show os
        , ""
        , "hostPlatformArchOS :: ArchOS"
        , "hostPlatformArchOS = ArchOS hostPlatformArch hostPlatformOS"
        ]
