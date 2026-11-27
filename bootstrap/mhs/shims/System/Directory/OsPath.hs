-- MicroHs has no directory-ospath package.  OsPath is filepath's type.
-- These wrappers decode to FilePath and call System.Directory.
module System.Directory.OsPath
  ( getModificationTime
  , doesDirectoryExist
  , doesFileExist
  , getDirectoryContents
  , createDirectoryIfMissing
  ) where

import Prelude

import Control.Monad.Catch ()
import Data.Time.Clock (UTCTime)
import qualified System.Directory as D
import qualified System.Directory.MTime as M
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)

getModificationTime :: OsPath -> IO UTCTime
getModificationTime p = do
  fp <- decodeUtf p
  M.getModificationTime fp

doesDirectoryExist :: OsPath -> IO Bool
doesDirectoryExist p = decodeUtf p >>= D.doesDirectoryExist

doesFileExist :: OsPath -> IO Bool
doesFileExist p = decodeUtf p >>= D.doesFileExist

getDirectoryContents :: OsPath -> IO [OsPath]
getDirectoryContents p = do
  fp <- decodeUtf p
  map unsafeEncodeUtf <$> D.getDirectoryContents fp

createDirectoryIfMissing :: Bool -> OsPath -> IO ()
createDirectoryIfMissing parents p = do
  fp <- decodeUtf p
  D.createDirectoryIfMissing parents fp
