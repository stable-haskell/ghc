{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main where

import Control.Exception (SomeException, bracket_, displayException, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import GHC (getLogger, parseDynamicFlags, runGhc, setSessionDynFlags)
import GHC.Data.ShortText (pack)
import GHC.Types.SrcLoc (noLoc)
import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Session
import GHC.Platform (TargetABI(..), targetABIName, platformArchOS, platformUnregisterised)
import GHC.Platform.Profile (Profile(..), profileBuildTag, profileIsProfiling)
import GHC.Platform.Ways
import GHC.Settings.IO (initSettings)
import GHC.Unit.Finder.Types (finder_ways)
import GHC.Unit.Info
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.FilePath ((</>))

check :: String -> Bool -> IO ()
check label ok = unless ok (error label)

main :: IO ()
main = do
  [libdir] <- getArgs
  raw <- read <$> readFile (libdir </> "settings")
  bracket_ (createDirectory "profiling-settings")
           (removeDirectoryRecursive "profiling-settings") $ do
    let loadABI abi = do
          writeFile ("profiling-settings" </> "settings") $
            show (("target ABI", abi) : filter ((/= "target ABI") . fst) raw)
          runExceptT (initSettings "profiling-settings")
    native <- loadABI "" >>= either (const $ error "native settings rejected") pure
    prof <- loadABI "profiling" >>= either (const $ error "profiling settings rejected") pure
    unknown <- loadABI "unknown-abi"
    check "unknown ABI silently accepted" $ case unknown of
      Left _ -> True
      Right _ -> False
    let normal = defaultDynFlags native
        target = defaultDynFlags prof
        (consistent, _, _) = makeDynFlagsConsistent $
          gopt_unset target Opt_ExternalInterpreter
    check "default target identity changed" (targetABI normal == DefaultABI)
    check "profiling target identity missing" (targetABI target == ProfilingABI)
    check "compiler metadata does not describe the selected ABI" $
      lookup "GHC Profiled" (compilerInfo normal) == Just "NO" &&
      lookup "GHC Profiled" (compilerInfo target) == Just "YES"
    check "target ABI spelling" (targetABIName (targetABI target) == "profiling")
    check "same-architecture ABI changed the platform" $
      targetPlatform normal == targetPlatform target
    check "profiling code generation not selected by target" $
      profileIsProfiling (targetProfile target)
    check "profiling target tried to use the host interpreter" $
      gopt Opt_ExternalInterpreter target && gopt Opt_ExternalInterpreter consistent
    check "profiling target searched for p_hi interfaces" $
      Set.null (finder_ways (initFinderOpts target))
    check "ordinary output suffixes changed" $
      objectSuf target == "o" && hiSuf_ target == "hi"
    check "target user database aliases native" $
      versionedFilePathForABI (platformArchOS (targetPlatform target)) ProfilingABI
        == versionedFilePath (platformArchOS (targetPlatform normal)) ++ "-profiling"
    -- Interface ABI tags must stay distinct for both static and dynamic code,
    -- including unregisterised targets, independently of filename ways.
    forM_ [False, True] $ \unreg ->
      forM_ [Set.empty, Set.singleton WayDyn] $ \ws -> do
        let nativeProfile = (targetProfile normal)
              { profilePlatform = (targetPlatform normal) { platformUnregisterised = unreg }
              , profileWays = ws }
            profilingProfile = nativeProfile { profileTargetABI = ProfilingABI }
        check "profiling ABI aliases the native interface tag" $
          profileBuildTag nativeProfile /= profileBuildTag profilingProfile
        check "ABI profiling depends on build ways" $
          not (profileIsProfiling nativeProfile) && profileIsProfiling profilingProfile
    forM_ [opt_P, opt_JSP, opt_c, opt_cxx] $ \options ->
      check "target preprocessing does not follow the ABI" $
        "-DPROFILING" `elem` options target && "-DPROFILING" `notElem` options normal
    check "Haskell CPP fingerprint omits the target ABI" $
      "-DPROFILING" `elem` fst (opt_P_signature target) &&
      "-DPROFILING" `notElem` fst (opt_P_signature normal)
    check "JavaScript CPP fingerprint omits the target ABI" $
      "-DPROFILING" `elem` fst (opt_JSP_signature target) &&
      "-DPROFILING" `notElem` fst (opt_JSP_signature normal)
    let library = GenericUnitInfo { unitLibraries = map pack ["HSfoo", "HSrts-threaded-debug"] }
        namever = GhcNameVersion "ghc" "test"
    forM_ [[], [WayThreaded, WayDebug]] $ \rtsWays -> do
      let profWays = Set.fromList rtsWays
      check "profiling target added a library suffix" $
        unitHsLibs namever profWays library == ["HSfoo", "HSrts-threaded-debug"]
      check "profiling dynamic target added a profiling suffix" $
        unitHsLibs namever (Set.insert WayDyn profWays) library
          == ["HSfoo-ghctest", "HSrts-threaded-debug-ghctest"]
    -- A profiling target starts with an empty package database while its boot
    -- libraries are built. Initializing the external interpreter must not try
    -- to link iserv (and therefore require ghci) before it is actually used.
    initializedNormal <- initDynFlags normal
    initializedTarget <- initDynFlags target
    runGhc (Just libdir) $ do
      logger <- getLogger
      legacy <- liftIO $ try (parseDynamicFlags logger initializedNormal [noLoc "-prof"])
      liftIO $ check "ordinary compiler accepted legacy -prof" $
        case legacy of
          Left (err :: SomeException) -> "-prof requires a separate profiling compiler" `isInfixOf` displayException err
          Right _ -> False
      (redundant, _, _) <- parseDynamicFlags logger initializedTarget [noLoc "-prof"]
      liftIO $ check "profiling compiler rejected redundant -prof or changed ABI" $
        targetABI redundant == ProfilingABI && Set.null (ways redundant)
      (bootstrap, _, _) <- parseDynamicFlags logger initializedTarget $ map noLoc
        [ "-clear-package-db", "-no-user-package-db", "-no-rts"
        , "-no-auto-link-packages", "-fno-code"
        ]
      _ <- setSessionDynFlags bootstrap
      pure ()
