module Main where

import GHC
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Env
import GHC.Driver.Monad (getSession)
import GHC.Driver.Plugins
import GHC.HsToCore.Usage
import GHC.Linker.Types
import GHC.Tc.Utils.Monad (initIfaceLoad)
import GHC.Types.Name.Set (emptyNameSet)
import GHC.Types.Unique.DFM
import GHC.Types.Unique.DSet
import GHC.Unit.Module.Deps
import GHC.Unit.Types
import GHC.Unit.Finder (flushFinderCaches)

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import System.Directory
import System.Environment
import System.FilePath
import Data.Time (getCurrentTime)

check :: String -> Bool -> IO ()
check label ok = unless ok (error label)

main :: IO ()
main = do
  [libdir] <- getArgs
  root <- getCurrentDirectory
  let hostFile = root </> "host-package.a"
      targetFile = root </> "target-package.a"
      hostObject = root </> "host-module.o"
      targetObject = root </> "target-module.o"
      files = [hostFile, targetFile, hostObject, targetObject]
      uid = stringToUnitId "same-id-different-platform"
      mod = mkModule (RealUnit (Definite uid)) (mkModuleName "Shared")
      loaded path = listToUDFM [(uid, LoadedPkgInfo uid [Archive path] [] [] emptyUniqDSet)]
  mapM_ (\path -> writeFile path path) files
  now <- getCurrentTime
  let link path = Linkable now mod (DotO path ModuleObject :| [])
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    setSessionDynFlags flags
    env <- getSession
    let plugins = emptyPlugins { loadedPluginDeps = ([link hostObject], loaded hostFile) }
        usages = liftIO $ initIfaceLoad env $ mkUsageInfo (UsageConfig False) plugins (hsc_FC env)
          (hsc_unit_env env) (mkModule mainUnit (mkModuleName "Target")) Map.empty []
          emptyNameSet [] [] [link targetObject] (loaded targetFile)
        fileHashes = Map.fromList . map (\usage -> (unpackFS (usg_file_path usage), usg_file_hash usage)) .
          filter isFile
        isFile UsageFile {} = True
        isFile _ = False
        refresh = liftIO $ flushFinderCaches (hsc_FC env) (hsc_unit_env env)
    initial <- fileHashes <$> usages
    liftIO $ check "host/target dependency was lost"
      (Map.size initial == 4 && all (`Map.member` initial) files)
    liftIO $ appendFile hostFile " changed host"
    refresh
    changedHost <- fileHashes <$> usages
    liftIO $ do
      check "host implementation change was not tracked" (Map.lookup hostFile initial /= Map.lookup hostFile changedHost)
      check "host change altered target fingerprint" (Map.lookup targetFile initial == Map.lookup targetFile changedHost)
      appendFile targetFile " changed target"
    refresh
    changedTarget <- fileHashes <$> usages
    liftIO $ do
      check "target implementation change was not tracked" (Map.lookup targetFile changedHost /= Map.lookup targetFile changedTarget)
      check "target change altered host fingerprint" (Map.lookup hostFile changedHost == Map.lookup hostFile changedTarget)
  putStrLn "host and target implementation dependencies survive shared identities: OK"
