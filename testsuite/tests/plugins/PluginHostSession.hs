module Main where

import GHC
import GHC.Driver.Env
import GHC.Data.OsPath (unsafeEncodeUtf)
import GHC.Driver.Hooks
import GHC.Driver.MakeFile (doMkDependHS)
import GHC.Driver.Monad (getSession, modifySession, setSession)
import GHC.Driver.Session
import GHC.Linker.Types (loader_state)
import GHC.Runtime.Interpreter.Types
import GHC.Runtime.Loader (initializePlugins)
import GHC.Driver.Plugins (emptyPlugins)
import GHC.Types.PkgQual (PkgQual(NoPkgQual))
import GHC.Unit.External (ExternalUnitCache(euc_eps))
import GHC.Unit.Env (UnitEnv(ue_eps))
import GHC.Unit.Finder
import GHC.Unit.Types (unitString, moduleUnit, toUnitId)
import GHC.Unit.State (lookupUnitId)
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Home.ModInfo (HomeModInfo(hm_iface))
import GHC.Unit.Module.ModIface (mi_deps)
import GHC.Unit.Module.Deps (dep_direct_mods, dep_plugin_pkgs)
import qualified Data.Set as Set

import Control.Concurrent.MVar (readMVar)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.Directory
import System.Environment
import System.FilePath
import System.Process

check :: String -> Bool -> IO ()
check label ok = unless ok (error label)

expectFailure :: String -> IO a -> IO ()
expectFailure label action = do
  result <- try (action >> pure ()) :: IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right _ -> error (label ++ " unexpectedly succeeded")

pluginLoader :: HscEnv -> IO (Maybe Interp)
pluginLoader = readMVar . hsc_plugin_interp

sameLoader :: Maybe Interp -> Maybe Interp -> Bool
sameLoader (Just a) (Just b) = loader_state (interpLoader a) == loader_state (interpLoader b)
sameLoader _ _ = False

main :: IO ()
main = do
  [libdir, pluginDb] <- getArgs
  starts <- newIORef (0 :: Int)
  -- Only the external interpreter receives this marker, even if the caller
  -- happened to export a value with the same name.
  unsetEnv "GHC_PLUGIN_HOST_TEST"
  environment <- getEnvironment
  let startIserv process = do
        modifyIORef' starts (+ 1)
        (_, _, _, handle) <- createProcess process
          { env = Just (("GHC_PLUGIN_HOST_TEST", "target") :
                        filter ((/= "GHC_PLUGIN_HOST_TEST") . fst) environment) }
        pure handle
  runGhc (Just libdir) $ do
    initial <- getSessionDynFlags
    logger <- getLogger
    -- A discarded DB must not be read. The host's global DB must be supplied
    -- again after clearing the stack. No normal -package-db exposes the plugin.
    (flags, leftovers, _) <- liftIO $ parseDynamicFlagsCmdLine logger initial $ map noLoc
      [ "-fexternal-interpreter", "-plugin-libdir", libdir
      , "-plugin-package-db", "discarded-plugin-db"
      , "-clear-plugin-package-dbs"
      , "-plugin-package-db", libdir </> "package.conf.d"
      , "-plugin-package-db", pluginDb
      , "-hide-all-plugin-packages", "-plugin-package", "simple-plugin (Simple.Plugin)"
      , "-iPluginHostShadow"
      , "-fplugin", "Simple.Plugin"
      , "-fplugin-opt", "Simple.Plugin:reload"
      ]
    liftIO $ check "unparsed plugin flags" (null leftovers)
    modifySession $ \h -> h { hsc_hooks = (hsc_hooks h)
      { createIservProcessHook = Just startIserv } }
    setSessionDynFlags flags { ghcLink = NoLink, verbosity = 0 }
    targetEnv <- getSession
    hostEnv <- liftIO $ hscPluginEnv targetEnv
    hostModule <- liftIO $ do
      targetLookup <- findImportedModule targetEnv (mkModuleName "Simple.Plugin") NoPkgQual
      hostLookup <- findPluginModule targetEnv (mkModuleName "Simple.Plugin")
      check "host plugin could not be found" $ case hostLookup of
        Found {} -> True
        _ -> False
      check "host and target EPS are shared"
        (euc_eps (ue_eps (hsc_unit_env hostEnv)) /= euc_eps (ue_eps (hsc_unit_env targetEnv)))
      case (targetLookup, hostLookup) of
        (Found _ targetMod, Found _ hostMod) -> do
          check "host plugin shadowed the target source homonym"
            (moduleUnit targetMod /= moduleUnit hostMod)
          check "host package leaked into target packages"
            (isNothing (lookupUnitId (hsc_units targetEnv) (toUnitId (moduleUnit hostMod))))
          pure hostMod
        _ -> error "host plugin or target homonym lookup failed"

    loaded <- liftIO $ initializePlugins targetEnv
    firstLoader <- liftIO $ pluginLoader loaded
    liftIO $ do
      check "plugin changed target interpreter" (sameLoader (hsc_interp targetEnv) (hsc_interp loaded))
      expectFailure "separate plugin packages with internal target" $ initializePlugins loaded
        { hsc_interp = firstLoader, hsc_plugins = emptyPlugins }
      expectFailure "bad plugin module" $ initializePlugins loaded
        { hsc_dflags = (hsc_dflags loaded) { pluginModNames = [mkModuleName "MissingPlugin"] } }
      -- A failed host-environment initialization must restore the previous
      -- cache and release its lock, so the following load can succeed.
      expectFailure "bad plugin DB" $ hscPluginEnv loaded
        { hsc_dflags = (hsc_dflags loaded)
          { pluginPackageDBFlags = [PackageDB (PkgDbPath (unsafeEncodeUtf "missing-plugin-db"))] } }
      recovered <- hscPluginEnv loaded
      check "failed initialization replaced the cached host EPS"
        (euc_eps (ue_eps (hsc_unit_env hostEnv)) == euc_eps (ue_eps (hsc_unit_env recovered)))

    -- Changing options forces initializePlugins through its loading path.
    reloaded <- liftIO $ initializePlugins loaded
      { hsc_dflags = (hsc_dflags loaded)
        { pluginModNameOpts = [(mkModuleName "Simple.Plugin", "second")] } }
    secondLoader <- liftIO $ pluginLoader reloaded
    liftIO $ check "plugin loader was recreated on reload" (sameLoader firstLoader secondLoader)
    setSession reloaded
    -- The installed-ID spelling must also be validated against host packages.
    (idFlags, idLeftovers, _) <- liftIO $ parseDynamicFlagsCmdLine logger
      ((hsc_dflags reloaded) { pluginPackageFlags = [] })
      (map noLoc ["-plugin-package-id", unitString (moduleUnit hostModule)])
    liftIO $ check "unparsed plugin package ID" (null idLeftovers)
    -- Host and target namespaces may even use the same unit ID. The host
    -- plugin must still be recorded as an external plugin package.
    let pluginId = toUnitId (moduleUnit hostModule)
    setSessionDynFlags idFlags { homeUnitId_ = pluginId }
    selectedById <- getSession >>= liftIO . initializePlugins
    idLoader <- liftIO $ pluginLoader selectedById
    liftIO $ check "package selection recreated the plugin loader" (sameLoader firstLoader idLoader)
    setSession selectedById
    target <- guessTarget "PluginHostTH.hs" Nothing Nothing
    setTargets [target]
    outcome <- load LoadAllTargets
    liftIO $ check "TH and plugin compilation failed" (succeeded outcome)
    compiled <- getSession
    info <- liftIO $ lookupHpt (hsc_HPT compiled) (mkModuleName "PluginHostTH")
    liftIO $ case info of
      Nothing -> error "compiled module missing from target HPT"
      Just hmi -> do
        let dependencies = mi_deps (hm_iface hmi)
        check "host plugin was recorded as a target home module"
          (Set.null (dep_direct_mods dependencies))
        check "same-ID host plugin was not recorded as a plugin package"
          (Set.member pluginId (dep_plugin_pkgs dependencies))
    -- Pure plugins and unchanged host dependencies must not force recompilation.
    -- The golden output requires exactly one invocation of the plugin's pass.
    secondOutcome <- load LoadAllTargets
    liftIO $ check "second compilation failed" (succeeded secondOutcome)
    -- ghc -M must find host plugins independently of normal target imports.
    -- Target profiling suffixes must not be added to a host interface path.
    depFlags <- getSessionDynFlags
    setSessionDynFlags depFlags
      { depMakefile = "PluginHostDeps.mk", depIncludePkgDeps = True
      , depSuffixes = ["", "p"], ghcMode = MkDepend }
    doMkDependHS ["PluginHostTH.hs"]
    dependencies <- liftIO $ readFile "PluginHostDeps.mk"
    liftIO $ do
      check "dependency generation omitted the host interface"
        ("Simple/Plugin." `isInfixOf` dependencies)
      check "dependency generation followed the target plugin homonym"
        (not ("PluginHostShadow" `isInfixOf` dependencies))
      check "dependency generation added target suffix to host interface"
        (not ("Simple/Plugin.p_" `isInfixOf` dependencies))
  count <- readIORef starts
  check "TH did not use exactly one external interpreter" (count == 1)
  -- Foreign host settings must be rejected before reading any package DB.
  settings <- read <$> readFile (libdir </> "settings")
  createDirectoryIfMissing True "foreign-plugin-libdir"
  let wrongWordSize = if lookup "target word size" settings == Just "8" then "4" else "8"
  writeFile ("foreign-plugin-libdir" </> "settings")
    (show (("target word size", wrongWordSize) : filter ((/= "target word size") . fst) settings))
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    session <- getSession
    result <- liftIO $ try $ hscPluginEnv session
      { hsc_dflags = flags { pluginLibDir = Just "foreign-plugin-libdir" } }
    liftIO $ case result of
      Left exception -> check "foreign host diagnostic lost context"
        ("Plugin host configuration" `isInfixOf` show (exception :: SomeException))
      Right _ -> error "foreign plugin settings unexpectedly accepted"
  putStrLn "host packages, target TH, persistent loader and failure recovery: OK"
