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
import GHC.Driver.Session hiding (Option)
import GHC.Platform (TargetABI(..), targetABIName, platformArchOS, platformOS, platformUnregisterised, PlatformMisc(..), OS(..))
import GHC.Platform.Profile (Profile(..), profileBuildTag, profileIsProfiling)
import GHC.Platform.Ways
import GHC.Settings.IO (initSettings)
import GHC.UniqueSubdir (uniqueSubdirForTargetName)
import GHC.Unit.Finder.Types (finder_ways)
import GHC.Unit.Info
import GHC.TargetName (parseTargetName, targetNameForABI, validateTargetNameNamespace)
import GHC.Version (cProjectVersion)
import GHC.TargetDispatch (resolveTargetAlias, targetNameFromProgram, lastTargetName, stripWindowsExeExtension)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Console.GetOpt (ArgDescr(..), OptDescr(..), ArgOrder(Permute), getOpt)

check :: String -> Bool -> IO ()
check label ok = unless ok (error label)

main :: IO ()
main = do
  [libdir] <- getArgs
  raw <- read <$> readFile (libdir </> "settings")
  bracket_ (createDirectory "profiling-settings")
           (removeDirectoryRecursive "profiling-settings") $ do
    let machine = maybe (error "target platform missing from fixture settings") id $
          lookup "target platform string" raw
        loadSettings settings = do
          writeFile ("profiling-settings" </> "settings") (show settings)
          runExceptT (initSettings "profiling-settings")
        loadABI abi includeName = do
          let name = if abi == "profiling" then machine ++ "-prof" else machine
              settings = [("target ABI", abi)] ++
                (if includeName then [("target name", name)] else []) ++
                filter ((`notElem` ["target ABI", "target name"]) . fst) raw
          loadSettings settings
    native <- loadABI "" True >>= either (const $ error "native settings rejected") pure
    prof <- loadABI "profiling" True >>= either (const $ error "profiling settings rejected") pure
    legacy <- loadABI "profiling" False >>= either (const $ error "legacy settings rejected") pure
    unknown <- loadABI "unknown-abi" True
    inconsistent <- loadSettings $ ("target name", "different-target") :
      filter ((/= "target name") . fst) raw
    queryKey <- loadSettings $ ("Target name", "different-target") : raw
    duplicateABI <- loadSettings $ ("target ABI", "profiling") : raw
    check "unknown ABI silently accepted" $ case unknown of
      Left _ -> True
      Right _ -> False
    check "contradictory target name silently accepted" $ case inconsistent of
      Left _ -> True
      Right _ -> False
    check "raw query identity key silently accepted" $ case queryKey of
      Left _ -> True
      Right _ -> False
    check "duplicate ABI setting silently accepted" $ case duplicateABI of
      Left _ -> True
      Right _ -> False
    let normal = defaultDynFlags native
        target = defaultDynFlags prof
        (consistent, _, _) = makeDynFlagsConsistent $
          gopt_unset target Opt_ExternalInterpreter
    check "default target identity changed" (targetABI normal == DefaultABI)
    check "profiling target identity missing" (targetABI target == ProfilingABI)
    check "legacy profiling settings lost their ABI" (targetABI (defaultDynFlags legacy) == ProfilingABI)
    check "profiling target defaults requested a dynamic or debug way" $
      Set.null (ways target)
    check "compiler metadata does not describe the selected ABI" $
      lookup "GHC Profiled" (compilerInfo normal) == Just "NO" &&
      lookup "GHC Profiled" (compilerInfo target) == Just "YES"
    check "target ABI spelling" (targetABIName (targetABI target) == "profiling")
    check "machine triple with invalid trailing component was accepted" $
      case targetNameForABI "x86_64-unknown-linux." "profiling" of
        Left _ -> True
        Right _ -> False
    let dottedMachine = "x86_64.vendor.abi-linux-gnu"
        version = "9.14.0"
    check "versioned ghc basename truncated the dotted machine or patch version" $
      targetNameFromProgram "ghc" version (dottedMachine ++ "-ghc-" ++ version) == Just dottedMachine
    check "versioned ghc-pkg basename truncated the dotted machine or patch version" $
      targetNameFromProgram "ghc-pkg" version (dottedMachine ++ "-ghc-pkg-" ++ version) == Just dottedMachine
    check "uppercase Windows executable suffix changed target dispatch" $
      targetNameFromProgram "ghc" version
        (stripWindowsExeExtension (dottedMachine ++ "-ghc-" ++ version ++ ".EXE")) == Just dottedMachine
    let targetOption = Option [] ["target"] (ReqArg id "TARGET") "select target"
        selectedTarget args = case getOpt Permute [targetOption] args of
          (targets, [], []) -> lastTargetName targets
          _ -> Nothing
    check "ghc-pkg info selected a different target for --target=x then --target x" $
      selectedTarget ["--target=first", "--target", "second"] == Just "second"
    check "ghc-pkg info selected a different target for --target x then --target=x" $
      selectedTarget ["--target", "first", "--target=second"] == Just "second"
    check "255-character target name was rejected" $
      either (const False) (const True) (parseTargetName (replicate 255 'a'))
    check "256-character target name was accepted" $
      either (const True) (const False) (parseTargetName (replicate 256 'a'))
    let fitsNamespace = replicate (254 - length cProjectVersion) 'a'
        exceedsNamespace = replicate (255 - length cProjectVersion) 'a'
    check "bounded target name did not fit its versioned namespace" $
      case parseTargetName fitsNamespace of
        Left _ -> False
        Right name -> either (const False) (const True) (validateTargetNameNamespace name cProjectVersion)
    check "overlong versioned target namespace was accepted" $
      case parseTargetName exceedsNamespace of
        Left _ -> True
        Right name -> either (const True) (const False) (validateTargetNameNamespace name cProjectVersion)
    bracket_ (createDirectory "target-dispatch")
             (removeDirectoryRecursive "target-dispatch") $ do
      createDirectory ("target-dispatch" </> "x86_64-unknown-linux")
      writeFile ("target-dispatch" </> "x86_64-unknown-linux" </> "target.aliases")
        "[\"linux-host\"]"
      alias <- resolveTargetAlias "target-dispatch" "linux-host"
      folded <- resolveTargetAlias "target-dispatch" "LINUX-HOST"
      check "target alias did not resolve" (alias == Right "x86_64-unknown-linux")
      check "case-folded target alias was accepted without exact spelling" $
        case folded of Left _ -> True; Right _ -> False
      createDirectory ("target-dispatch" </> "aarch64-unknown-linux")
      writeFile ("target-dispatch" </> "aarch64-unknown-linux" </> "target.aliases")
        "[\"LINUX-HOST\"]"
      collision <- resolveTargetAlias "target-dispatch" "linux-host"
      check "case-folded alias collision was not rejected" $
        case collision of Left _ -> True; Right _ -> False
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
      versionedFilePathForABI (platformArchOS (targetPlatform target)) ProfilingABI Nothing
        == versionedFilePath (platformArchOS (targetPlatform normal)) ++ "-profiling"
    let canonicalName = either (error . show) id $ targetNameForABI machine "profiling"
        legacyNamespace = versionedFilePathForABI
          (platformArchOS (targetPlatform target)) ProfilingABI Nothing
        qualifiedNamespace = versionedFilePathForABI
          (platformArchOS (targetPlatform target)) ProfilingABI (Just canonicalName)
    check "canonical target name does not select its qualified namespace" $
      qualifiedNamespace == uniqueSubdirForTargetName canonicalName &&
      qualifiedNamespace /= legacyNamespace
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
    let replaceRaw key value rows = (key, value) : filter ((/= key) . fst) rows
        unsupportedTarget = initializedTarget
          { platformMisc = (platformMisc initializedTarget)
              { platformMisc_targetIsDynamic = False }
          , rawSettings = replaceRaw "RTS ways" "v thr" (rawSettings initializedTarget)
          }
        capableTarget = initializedTarget
          { platformMisc = (platformMisc initializedTarget)
              { platformMisc_targetIsDynamic = True }
          , rawSettings = replaceRaw "RTS ways" "v thr debug thr_debug" (rawSettings initializedTarget)
          }
    runGhc (Just libdir) $ do
      logger <- getLogger
      forM_ ["-dynamic", "-dynamic-too", "-debug"] $ \flag -> do
        rejected <- liftIO $ try (parseDynamicFlags logger unsupportedTarget [noLoc flag])
        liftIO $ check ("profiling target accepted " ++ flag) $ case rejected of
          Left (err :: SomeException) ->
            flag `isInfixOf` displayException err &&
            (machine ++ "-prof") `isInfixOf` displayException err &&
            "static non-debug" `isInfixOf` displayException err
          Right _ -> False
      forM_ ["-dynamic", "-dynamic-too", "-debug"] $ \flag -> do
        accepted <- liftIO $ try (parseDynamicFlags logger capableTarget [noLoc flag])
        let shouldAccept = flag /= "-dynamic-too" || platformOS (targetPlatform target) /= OSMinGW32
        liftIO $ check ("profiling target capability mismatch for " ++ flag) $ case accepted of
          Right _ -> shouldAccept
          Left (_ :: SomeException) -> not shouldAccept
      threadDebugRejected <- liftIO $ try $ parseDynamicFlags logger unsupportedTarget $ map noLoc
        ["-threaded", "-debug"]
      liftIO $ check "profiling target accepted unavailable threaded-debug RTS" $ case threadDebugRejected of
        Left (err :: SomeException) -> "-debug" `isInfixOf` displayException err
        Right _ -> False
      threadDebugAccepted <- liftIO $ try $ parseDynamicFlags logger capableTarget $ map noLoc
        ["-threaded", "-debug"]
      liftIO $ check "profiling target rejected advertised threaded-debug RTS" $ case threadDebugAccepted of
        Right _ -> True
        Left (_ :: SomeException) -> False
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
