{-# LANGUAGE CPP #-}

module GHC.Utils.Touch (touch) where

import GHC.Prelude

#if defined(__MHS__)
-- MicroHs has no unix package.  mhs_touch wraps utimensat(AT_FDCWD,
-- ..., NULL, 0) and creates the file when missing, exactly what
-- touch(1) does.
import Foreign.C.String (CString, withCString)
#else
#if defined(mingw32_HOST_OS)
import System.Win32.File
import System.Win32.Time
#else
import System.Posix.Files
import System.Posix.IO
#endif
#endif

-- | Set the mtime of the given file to the current time.
touch :: FilePath -> IO ()
touch file = do
#if defined(__MHS__)
  r <- withCString file c_mhs_touch
  if r /= 0
    then ioError (userError ("touch " ++ file))
    else return ()
#else
#if defined(mingw32_HOST_OS)
    hdl <- createFile file gENERIC_WRITE fILE_SHARE_NONE Nothing oPEN_ALWAYS fILE_ATTRIBUTE_NORMAL Nothing
    t <- getSystemTimeAsFileTime
    setFileTime hdl Nothing Nothing (Just t)
    closeHandle hdl
#else
  let oflags = defaultFileFlags { noctty = True, creat = Just 0o666 }
  fd <- openFd file WriteOnly oflags
  touchFd fd
  closeFd fd
#endif
#endif

#if defined(__MHS__)
foreign import ccall "mhs_posix.h mhs_touch" c_mhs_touch :: CString -> IO Int
#endif
