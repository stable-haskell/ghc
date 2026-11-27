-- Minimal `findExecutable` for MicroHs stage1 tool builds.
--
-- MicroHs `base` has `System.Directory` but no `findExecutable`, and an
-- installed package module can never be shadowed by a source file (see
-- MhsTool.Process), so hsc2hs's `System.Directory (findExecutable)`
-- import is patched to this module (see
-- bootstrap/mhs/hsc2hs-mhs-port.patch).
module MhsTool.Directory
  ( findExecutable
  , XdgDirectory(..)
  , getXdgDirectory
  , getAppUserDataDirectory
  ) where

import Prelude

import Foreign.C.String (CString, withCAString)
import Foreign.C.Types (CInt(..))
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

foreign import ccall "mhs_tool_posix.h mhs_tool_access_x" c_accessX
  :: CString -> IO CInt

-- | Search PATH for an executable file, like
-- `System.Directory.findExecutable`. A name containing a slash is
-- checked directly.
findExecutable :: String -> IO (Maybe FilePath)
findExecutable name
  | '/' `elem` name = do
      ok <- isExecutable name
      return (if ok then Just name else Nothing)
  | otherwise = do
      path <- catchIOError (getEnv "PATH") (\_ -> return "/usr/bin:/bin")
      search (splitPath path)
  where
    search [] = return Nothing
    search (d : ds) = do
      let p = d ++ "/" ++ name
      ok <- isExecutable p
      if ok then return (Just p) else search ds

isExecutable :: FilePath -> IO Bool
isExecutable p = do
  ex <- doesFileExist p
  if not ex
    then return False
    else (/= 0) <$> withCAString p c_accessX

splitPath :: String -> [String]
splitPath s = case break (== ':') s of
  (d, ':' : rest) -> d : splitPath rest
  (d, _)          -> [d]

data XdgDirectory
  = XdgData
  | XdgConfig
  | XdgCache
  | XdgState
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectory kind rel = do
  let (envName, fallbackRel) = case kind of
        XdgData   -> ("XDG_DATA_HOME",   ".local/share")
        XdgConfig -> ("XDG_CONFIG_HOME", ".config")
        XdgCache  -> ("XDG_CACHE_HOME",  ".cache")
        XdgState  -> ("XDG_STATE_HOME",  ".local/state")
  menv <- lookupEnv envName
  base <- case menv of
    Just p | not (null p) -> return p
    _ -> do
      home <- getHomeDirectory
      return (home </> fallbackRel)
  return (if null rel then base else base </> rel)

getAppUserDataDirectory :: FilePath -> IO FilePath
getAppUserDataDirectory app = do
  home <- getHomeDirectory
  return (home </> ("." ++ app))
