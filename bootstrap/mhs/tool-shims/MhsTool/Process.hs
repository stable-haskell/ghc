-- Minimal `System.Process` replacement for MicroHs stage1 tool builds
-- (hsc2hs: Common.createProcess/waitForProcess, CrossCodegen.runProcess).
--
-- This module is deliberately NOT named `System.Process`: an installed
-- MicroHs package (`base`) already exports that name, and package
-- modules poison the compilation cache (every module implicitly imports
-- Prelude from `base`, so `base` is always loaded first), which means a
-- source file named `System/Process.hs` can never shadow it. The hsc2hs
-- scratch clone is therefore patched (see
-- bootstrap/mhs/hsc2hs-mhs-port.patch) to import `MhsTool.Process`.
--
-- Semantics: `posix_spawnp(3)` via cbits/mhs_tool_posix.c (PATH lookup,
-- argv passed verbatim -- never joined into a shell string), blocking
-- `waitpid(2)`. `CreatePipe` streams are backed by temporary files and
-- the child is reaped inside `createProcess` before the read `Handle`
-- is returned, so a fully-forced read before `waitForProcess` (exactly
-- what hsc2hs does) observes complete output. Pipes are therefore NOT
-- suitable for interactive streaming protocols. `CreatePipe` on stdin
-- is rejected: the child would observe EOF before the parent writes.
module MhsTool.Process
  ( ProcessHandle
  , StdStream(..)
  , CmdSpec(..)
  , CreateProcess(..)
  , proc
  , shell
  , createProcess
  , waitForProcess
  , runProcess
  , showCommandForUser
  , system
  ) where

import Prelude

import Control.Exception (bracket, catch)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.String (CString, newCAString, withCAString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca, allocaBytes, free)
import Foreign.Marshal.Array (withArray0)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, pokeElemOff, sizeOf)
import System.Directory (getCurrentDirectory, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hFlush, openFile, IOMode(..))
import System.IO.Error (IOException, catchIOError)
import System.IO.Internal (BFILE, withHandleAny)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "mhs_tool_posix.h mhs_tool_spawn" c_spawn
  :: CString -> Ptr CString -> CString -> CInt -> CString -> CInt
  -> CString -> CInt -> Ptr CString -> IO CInt
foreign import ccall "mhs_tool_posix.h mhs_tool_waitpid" c_waitpid
  :: CInt -> Ptr CInt -> IO CInt
foreign import ccall "mhs_tool_posix.h mhs_tool_handle_fd" c_handleFd
  :: Ptr BFILE -> IO CInt
foreign import ccall "mhs_tool_posix.h mhs_tool_chdir" c_chdir
  :: CString -> IO CInt

data StdStream
  = Inherit
  | UseHandle Handle
  | CreatePipe
  | NoStream

data CmdSpec
  = ShellCommand String
  | RawCommand FilePath [String]

data CreateProcess = CreateProcess
  { cmdspec :: CmdSpec
  , cwd     :: Maybe FilePath
  , env     :: Maybe [(String, String)]
  , std_in  :: StdStream
  , std_out :: StdStream
  , std_err :: StdStream
  }

proc :: FilePath -> [String] -> CreateProcess
proc cmd args =
  CreateProcess (RawCommand cmd args) Nothing Nothing Inherit Inherit Inherit

shell :: String -> CreateProcess
shell s = CreateProcess (ShellCommand s) Nothing Nothing Inherit Inherit Inherit

-- A spawned child: its pid, a cache for its exit code (repeated waits
-- are cheap and idempotent), and temporary files backing `CreatePipe`
-- streams, removed once the child is reaped.
data ProcessHandle = ProcessHandle !Int (IORef (Maybe ExitCode)) [FilePath]

-- | How one standard stream of the child is wired up.
data Redir
  = RInherit
  | RNull
  | RFd CInt
  | RFile FilePath
  | RPipe FilePath

withRedir :: Redir -> (CString -> CInt -> IO a) -> IO a
withRedir RInherit io   = io nullPtr (-1)
withRedir RNull io      = withCAString "/dev/null" $ \cnull -> io cnull (-1)
withRedir (RFd fd) io   = io nullPtr fd
withRedir (RFile fp) io = withCAString fp $ \cfp -> io cfp (-1)
withRedir (RPipe fp) io = withCAString fp $ \cfp -> io cfp (-1)

-- Marshal an argument vector as a NULL-terminated array.
withArgv :: [String] -> (Ptr CString -> IO a) -> IO a
withArgv args io =
  bracket
    (mapM newCAString args)
    (mapM_ free)
    ( \cstrs ->
        allocaBytes ((length cstrs + 1) * ptrSize) $ \arr -> do
          mapM_ (\(i, cs) -> pokeElemOff arr i cs) (zip [0 ..] cstrs)
          pokeElemOff arr (length cstrs) (nullPtr :: CString)
          io arr
    )
  where
    ptrSize = sizeOf (nullPtr :: CString)

-- Marshal an explicit environment for the child: 'Nothing' passes a
-- null pointer (the child inherits the parent environment).
withEnvp :: Maybe [(String, String)] -> (Ptr CString -> IO a) -> IO a
withEnvp Nothing io = io nullPtr
withEnvp (Just pairs) io =
  withCStrings (map (\(k, v) -> k ++ "=" ++ v) pairs) $ \cstrs ->
    withArray0 nullPtr cstrs io
  where
    withCStrings [] f = f []
    withCStrings (s : ss) f =
      withCAString s $ \cs ->
        withCStrings ss $ \css -> f (cs : css)

-- Run an action in a different working directory, restoring the
-- previous one afterwards.
withCwd :: Maybe FilePath -> IO a -> IO a
withCwd Nothing io = io
withCwd (Just dir) io =
  bracket
    getCurrentDirectory
    setCwd
    (\_ -> setCwd dir >> io)
  where
    setCwd d =
      withCAString d $ \cd ->
        throwErrnoIfMinus1_ ("MhsTool.Process: chdir " ++ d) (c_chdir cd)

-- Best-effort flush of a handle being handed to the child.
flushQuietly :: Handle -> IO ()
flushQuietly h = catch (hFlush h) ignoreIO
  where
    ignoreIO :: IOException -> IO ()
    ignoreIO _ = return ()

-- OS file descriptor for a MicroHs handle, via the FILE * inside its
-- BFILE (see cbits/mhs_tool_posix.c).
handleFd :: Handle -> IO CInt
handleFd h = withHandleAny h c_handleFd

{-# NOINLINE pipeCounter #-}
pipeCounter :: IORef Int
pipeCounter = unsafePerformIO (newIORef 0)

-- Fresh temporary file path for a `CreatePipe` stream. The counter
-- makes the name unique within this process run; the file is created
-- (truncating any stale leftover) so the child can open it without
-- O_CREAT.
tempPipeFile :: IO FilePath
tempPipeFile = do
  tmp <- getTemporaryDirectory
  n <- atomicModifyIORef' pipeCounter (\c -> let c' = c + 1 in (c', c'))
  let path = tmp ++ "/mhs-tool-pipe-" ++ show n
  bracket (openFile path WriteMode) hClose (\_ -> return ())
  return path

-- Prepare one stream: the child redirection plus an optional parent
-- `Handle` to return, optional temporary files to clean up on reap,
-- and whether the child was already reaped (`CreatePipe` outputs are
-- only served completely; see the module header). The flag marks the
-- stdin position: `CreatePipe` there is rejected.
prepareStream :: Bool -> StdStream -> IO (Redir, Maybe Handle, [FilePath], Bool)
prepareStream _ Inherit = return (RInherit, Nothing, [], False)
prepareStream _ NoStream = return (RNull, Nothing, [], False)
prepareStream _ (UseHandle h) = do
  flushQuietly h
  fd <- handleFd h
  if fd < 0
    then ioError (userError "MhsTool.Process: cannot get fd of handle")
    else return (RFd fd, Nothing, [], False)
prepareStream True CreatePipe =
  ioError (userError "MhsTool.Process: CreatePipe on stdin is not supported")
prepareStream False CreatePipe = do
  path <- tempPipeFile
  h <- openFile path ReadMode
  -- The child writes while the parent holds the read end open; the
  -- file is unlinked at reap time (POSIX keeps it alive for us).
  return (RPipe path, Just h, [path], True)

spawnRaw :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)]
         -> StdStream -> StdStream -> StdStream
         -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
spawnRaw cmd args mcwd menv si so se = withCwd mcwd $ do
  (ri, mhi, ti, wai) <- prepareStream True si
  (ro, mho, to, wao) <- prepareStream False so
  (re, mhe, te, wae) <- prepareStream False se
  let temps = ti ++ to ++ te
  withEnvp menv $ \cenvp ->
    withCAString cmd $ \ccmd ->
      withArgv (cmd : args) $ \cargv ->
        withRedir ri $ \cin fdin ->
          withRedir ro $ \cout fdout ->
            withRedir re $ \cerr fderr -> do
              pid <- throwErrnoIfMinus1 "MhsTool.Process.spawn"
                       (c_spawn ccmd cargv cin fdin cout fdout cerr fderr cenvp)
              st <- newIORef Nothing
              let ph = ProcessHandle (fromIntegral pid) st temps
              -- A pipe stream can only be served completely: reap the
              -- child now so the returned read handle observes all of
              -- its output (hsc2hs forces the content and then waits).
              if wai || wao || wae
                then waitForProcess ph >> return (mhi, mho, mhe, ph)
                else return (mhi, mho, mhe, ph)

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = case cmdspec cp of
  ShellCommand s  -> spawnRaw "/bin/sh" ["-c", s] (cwd cp) (env cp)
                       (std_in cp) (std_out cp) (std_err cp)
  RawCommand c as -> spawnRaw c as (cwd cp) (env cp)
                       (std_in cp) (std_out cp) (std_err cp)

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess (ProcessHandle pid st temps) = do
  mc <- readIORef st
  case mc of
    Just ec -> return ec
    Nothing -> do
      ec <- alloca $ \pcode -> do
        throwErrnoIfMinus1_ "MhsTool.Process.waitForProcess"
          (c_waitpid (fromIntegral pid) pcode)
        code <- peek pcode
        return (if code == 0 then ExitSuccess else ExitFailure (fromIntegral code))
      writeIORef st (Just ec)
      mapM_ removeQuietly temps
      return ec
  where
    removeQuietly :: FilePath -> IO ()
    removeQuietly fp = catchIOError (removeFile fp) (\_ -> return ())

runProcess :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)]
           -> Maybe Handle -> Maybe Handle -> Maybe Handle -> IO ProcessHandle
runProcess cmd args mcwd menv mhin mhout mherr = do
  (_, _, _, ph) <- spawnRaw cmd args mcwd menv
                     (mb mhin) (mb mhout) (mb mherr)
  return ph
  where
    mb Nothing  = Inherit
    mb (Just h) = UseHandle h

-- Quote one shell word, for log messages only.
showCommandForUser :: FilePath -> [String] -> String
showCommandForUser cmd args = unwords (map quote (cmd : args))
  where
    quote s
      | any (`elem` " \t\"'") s = show s
      | otherwise                = s

system :: String -> IO ExitCode
system cmd = do
  (_, _, _, ph) <- createProcess (shell cmd)
  waitForProcess ph
