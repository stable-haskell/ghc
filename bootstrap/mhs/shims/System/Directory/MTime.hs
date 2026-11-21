-- MicroHs's System.Directory does not export getModificationTime.
-- Implemented portably via mhs_posix.c's mhs_mtime (a plain stat(2)
-- wrapper), so no struct-stat layout assumptions are needed.
module System.Directory.MTime (getModificationTime) where

import Prelude

import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign.C.String (CString, withCString)
import System.IO.Error (doesNotExistErrorType, ioError, mkIOError)

foreign import ccall "mhs_posix.h mhs_mtime" c_mhs_mtime :: CString -> IO Int64

getModificationTime :: FilePath -> IO UTCTime
getModificationTime path = withCString path $ \p -> do
  sec <- c_mhs_mtime p
  if sec < 0
    then ioError (mkIOError doesNotExistErrorType "getModificationTime" Nothing (Just path))
    else return (posixSecondsToUTCTime (fromIntegral sec))
