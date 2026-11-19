{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Misc process handling code for SysTools
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------
module GHC.SysTools.Process
  ( readCreateProcessWithExitCode'
  , getGccEnv
  , runSomething
  , runSomethingResponseFile
  , runSomethingFiltered
  , runSomethingWith
#if defined(__MHS__)
  -- * MicroHs stand-ins for the System.Process API
  -- MicroHs ships only shell-style spawning, so the full process-handle
  -- API is unavailable. These keep the stage1 closure compiling; every
  -- runner fails loudly if executed (the stage1 build only typechecks).
  , CreateProcess(..)
  , CmdSpec(..)
  , StdStream(..)
  , ProcessHandle(..)
  , proc
  , createProcess
  , createProcess_
  , runInteractiveProcess
  , waitForProcess
  , getProcessExitCode
  , terminateProcess
#endif
  ) where

import GHC.Prelude

import GHC.Utils.Exception
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Utils.CliOption

import GHC.Types.SrcLoc ( SrcLoc, mkSrcLoc, mkSrcSpan )
import GHC.Data.FastString

#if !defined(__MHS__)
import GHC.IO.Encoding
#endif

#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows (handleToHANDLE)
import GHC.Event.Windows (associateHandle')
#endif

import Control.Concurrent
import Data.Char

import System.Exit
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error as IO
#if defined(__MHS__)
import System.Directory (getTemporaryDirectory, removeFile)
import qualified System.Process as MhsProc (system)
-- mhs does not propagate the `module CE` re-export of GHC.Utils.Exception
-- (a qualified-alias re-export), so import finally directly (same semantics).
import Control.Exception as CE (finally)

getLocaleEncoding :: IO TextEncoding
getLocaleEncoding = return localeEncoding

-- MicroHs System.Process is shell-only (system/readProcess): no handles
-- or OS pipes. Stand-ins mirror the subset of the API used across the
-- stage1 closure, while readCreateProcessWithExitCode' and builderMainLoop
-- execute external tools (cc, as, ld, ar, unlit, cpp) via MhsProc.system
-- with POSIX single-quote escaping and temporary output files.
data CmdSpec = ShellCommand String | RawCommand FilePath [String]
  deriving (Eq, Show)

data StdStream = Inherit | UseHandle Handle | CreatePipe | NoStream
  deriving (Eq, Show)

data CreateProcess = CreateProcess
  { cmdspec          :: CmdSpec
  , cwd              :: Maybe FilePath
  , env              :: Maybe [(String, String)]
  , std_in           :: StdStream
  , std_out          :: StdStream
  , std_err          :: StdStream
  , close_fds        :: Bool
  , create_group     :: Bool
  , delegate_ctlc    :: Bool
  , detach_console   :: Bool
  , create_new_console :: Bool
  , new_session      :: Bool
  , use_process_jobs :: Bool
  }

data ProcessHandle = MhsProcessHandle
  deriving (Eq, Show)

proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess
  { cmdspec = RawCommand cmd args
  , cwd = Nothing
  , env = Nothing
  , std_in = Inherit
  , std_out = Inherit
  , std_err = Inherit
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , detach_console = False
  , create_new_console = False
  , new_session = False
  , use_process_jobs = False
  }

showCommandForUser :: FilePath -> [String] -> String
showCommandForUser cmd args = unwords (map quote (cmd : args))
  where
    quote a | any (`elem` a) " \t\"'" = show a
            | otherwise = a

mhsQuoteShell :: String -> String
mhsQuoteShell s = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) s ++ "'"

mhsValidEnvName :: String -> Bool
mhsValidEnvName [] = False
mhsValidEnvName (c:cs) =
  (isAsciiUpper c || isAsciiLower c || c == '_') &&
  all (\x -> isAsciiUpper x || isAsciiLower x || isDigit x || x == '_') cs

mhsBuildShellCmd :: CmdSpec -> Maybe FilePath -> Maybe [(String, String)] -> String -> String
mhsBuildShellCmd spec mb_cwd mb_env redir =
  "( " ++ cwdPrefix ++ envPrefix ++ coreCmd ++ " " ++ redir ++ " )"
  where
    cwdPrefix = case mb_cwd of
      Nothing  -> ""
      Just dir -> "cd " ++ mhsQuoteShell dir ++ " && "
    envPrefix = case mb_env of
      Nothing  -> ""
      Just kvs -> concat
        [ "export " ++ k ++ "=" ++ mhsQuoteShell v ++ "; "
        | (k, v) <- kvs
        , mhsValidEnvName k
        , all (/= '\0') v
        ]
    coreCmd = case spec of
      ShellCommand s      -> s
      RawCommand pgm args -> unwords (map mhsQuoteShell (pgm : args))

noSpawn :: String -> IO a
noSpawn what =
  ioError (userError (what ++ ": process handle pipes are not supported in the MicroHs stage1 build"))

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess _ = noSpawn "createProcess"

createProcess_ :: String -> CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ _ _ = noSpawn "createProcess_"

runInteractiveProcess :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)]
                      -> IO (Handle, Handle, Handle, ProcessHandle)
runInteractiveProcess _ _ _ _ = noSpawn "runInteractiveProcess"

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess _ = noSpawn "waitForProcess"

getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode _ = noSpawn "getProcessExitCode"

terminateProcess :: ProcessHandle -> IO ()
terminateProcess _ = noSpawn "terminateProcess"
#else
import System.Process
#endif


-- | Enable process jobs support on Windows if it can be expected to work (e.g.
-- @process >= 1.6.9.0@).
enableProcessJobs :: CreateProcess -> CreateProcess
enableProcessJobs opts = opts { use_process_jobs = True }


-- Similar to System.Process.readCreateProcessWithExitCode, but stderr is
-- inherited from the parent process, and output to stderr is not captured.
readCreateProcessWithExitCode'
    :: CreateProcess
    -> IO (ExitCode, String)    -- ^ stdout
#if defined(__MHS__)
readCreateProcessWithExitCode' cp = do
    tmpDir <- getTemporaryDirectory
    (fout, hout) <- openTempFile tmpDir "ghc_read.out"
    hClose hout
    let run = do
          let fullCmd = mhsBuildShellCmd (cmdspec cp) (cwd cp) (env cp) ("> " ++ mhsQuoteShell fout)
          ex <- MhsProc.system fullCmd
          output <- readFile fout
          length output `seq` return (ex, output)
    CE.finally run (removeFile fout `catchIO` \_ -> return ())
#else
readCreateProcessWithExitCode' proc = do
    (_, Just outh, _, pid) <-
        createProcess $ enableProcessJobs $ proc{ std_out = CreatePipe }

    -- fork off a thread to start consuming the output
    outMVar <- newEmptyMVar
    let onError :: SomeException -> IO ()
        onError exc = putMVar outMVar (Left exc)
    _ <- forkIO $ handle onError $ do
      output <- hGetContents' outh
      putMVar outMVar $ Right output

    -- wait on the output
    result <- takeMVar outMVar
    hClose outh
    output <- case result of
      Left exc -> throwIO exc
      Right output -> return output

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, output)
#endif

-- If the -B<dir> option is set, add <dir> to PATH.  This works around
-- a bug in gcc on Windows Vista where it can't find its auxiliary
-- binaries (see bug #1110).
getGccEnv :: [Option] -> IO (Maybe [(String,String)])
getGccEnv opts =
  if null b_dirs
     then return Nothing
     else do env <- getEnvironment
             return (Just (mangle_paths env))
 where
  (b_dirs, _) = partitionWith get_b_opt opts

  get_b_opt (Option ('-':'B':dir)) = Left dir
  get_b_opt other = Right other

  -- Work around #1110 on Windows only (lest we stumble into #17266).
#if defined(mingw32_HOST_OS)
  mangle_paths = map mangle_path
  mangle_path (path,paths) | map toUpper path == "PATH"
        = (path, '\"' : head b_dirs ++ "\";" ++ paths)
  mangle_path other = other
#else
  mangle_paths = id
#endif


-----------------------------------------------------------------------------
-- Running an external program

runSomething :: Logger
             -> String          -- For -v message
             -> String          -- Command name (possibly a full path)
                                --      assumed already dos-ified
             -> [Option]        -- Arguments
                                --      runSomething will dos-ify them
             -> IO ()

runSomething logger phase_name pgm args =
  runSomethingFiltered logger id phase_name pgm args Nothing Nothing

-- | Run a command, placing the arguments in an external response file.
--
-- This command is used in order to avoid overlong command line arguments on
-- Windows. The command line arguments are first written to an external,
-- temporary response file, and then passed to the linker via @filepath.
-- response files for passing them in. See:
--
--     https://gcc.gnu.org/wiki/Response_Files
--     https://gitlab.haskell.org/ghc/ghc/issues/10777
runSomethingResponseFile
  :: Logger
  -> TmpFs
  -> TempDir
  -> ([String] -> [String])
  -> String
  -> String
  -> [Option]
  -> Maybe [(String,String)]
  -> IO ()
runSomethingResponseFile logger tmpfs tmp_dir filter_fn phase_name pgm args mb_env =
    runSomethingWith logger phase_name pgm args $ \real_args -> do
        fp <- getResponseFile real_args
        let args = ['@':fp]
        r <- builderMainLoop logger filter_fn pgm args Nothing mb_env
        return (r,())
  where
    getResponseFile args = do
      fp <- newTempName logger tmpfs tmp_dir TFL_CurrentModule "rsp"
      withFile fp WriteMode $ \h -> do
          hSetEncoding h utf8
          hPutStr h $ unlines $ map escape args
      return fp

    -- Note: Response files have backslash-escaping, double quoting, and are
    -- whitespace separated (some implementations use newline, others any
    -- whitespace character). Therefore, escape any backslashes, newlines, and
    -- double quotes in the argument, and surround the content with double
    -- quotes.
    --
    -- Another possibility that could be considered would be to convert
    -- backslashes in the argument to forward slashes. This would generally do
    -- the right thing, since backslashes in general only appear in arguments
    -- as part of file paths on Windows, and the forward slash is accepted for
    -- those. However, escaping is more reliable, in case somehow a backslash
    -- appears in a non-file.
    escape x = concat
        [ "\""
        , concatMap
            (\c ->
                case c of
                    '\\' -> "\\\\"
                    '\n' -> "\\n"
                    '\"' -> "\\\""
                    _    -> [c])
            x
        , "\""
        ]

runSomethingFiltered
  :: Logger -> ([String] -> [String]) -> String -> String -> [Option]
  -> Maybe FilePath -> Maybe [(String,String)] -> IO ()

runSomethingFiltered logger filter_fn phase_name pgm args mb_cwd mb_env =
    runSomethingWith logger phase_name pgm args $ \real_args -> do
        r <- builderMainLoop logger filter_fn pgm real_args mb_cwd mb_env
        return (r,())

runSomethingWith
  :: Logger -> String -> String -> [Option]
  -> ([String] -> IO (ExitCode, a))
  -> IO a

runSomethingWith logger phase_name pgm args io = do
  let real_args = filter notNull (map showOpt args)
      cmdLine = showCommandForUser pgm real_args
  traceCmd logger phase_name cmdLine $ handleProc pgm phase_name $ io real_args

handleProc :: String -> String -> IO (ExitCode, r) -> IO r
handleProc pgm phase_name proc = do
    (rc, r) <- proc `catchIO` handler
    case rc of
      ExitSuccess{} -> return r
      ExitFailure n -> throwGhcExceptionIO (
            ProgramError ("`" ++ takeFileName pgm ++ "'" ++
                          " failed in phase `" ++ phase_name ++ "'." ++
                          " (Exit code: " ++ show n ++ ")"))
  where
    handler err =
       if IO.isDoesNotExistError err
          then does_not_exist
          else throwGhcExceptionIO (ProgramError $ show err)

    does_not_exist =
      throwGhcExceptionIO $
        InstallationError (phase_name ++ ": could not execute: `" ++ pgm ++ "'")

withPipe :: ((Handle, Handle) -> IO a) -> IO a
#if defined(__MHS__)
withPipe _ = noSpawn "withPipe"
#else
withPipe = bracket createPipe $ \ (readEnd, writeEnd) -> do
  hClose readEnd
  hClose writeEnd
#endif

builderMainLoop :: Logger -> ([String] -> [String]) -> FilePath
                -> [String] -> Maybe FilePath -> Maybe [(String, String)]
                -> IO ExitCode
#if defined(__MHS__)
builderMainLoop logger filter_fn pgm real_args mb_cwd mb_env = do
    tmpDir <- getTemporaryDirectory
    (fout, hout) <- openTempFile tmpDir "ghc_proc.out"
    hClose hout
    let run = do
          let redir = "< /dev/null > " ++ mhsQuoteShell fout ++ " 2>&1"
              fullCmd = mhsBuildShellCmd (RawCommand pgm real_args) mb_cwd mb_env redir
          rc <- MhsProc.system fullCmd
          out <- readFile fout
          length out `seq` do
            let messages = parseBuildMessages (filter_fn (lines out))
            mapM_ processBuildMessage messages
            return rc
    CE.finally run (removeFile fout `catchIO` \_ -> return ())
  where
    processBuildMessage :: BuildMessage -> IO ()
    processBuildMessage msg =
      case msg of
        BuildMsg m ->
          logInfo logger $ withPprStyle defaultUserStyle m
        BuildError loc m ->
          logMsg logger errorDiagnostic (mkSrcSpan loc loc)
              $ withPprStyle defaultUserStyle m
#else
builderMainLoop logger filter_fn pgm real_args mb_cwd mb_env = withPipe $ \ (readEnd, writeEnd) -> do

#if defined(__IO_MANAGER_WINIO__)
  return () <!> do
    associateHandle' =<< handleToHANDLE readEnd
#endif

  -- We use a mask here rather than a bracket because we want
  -- to distinguish between cleaning up with and without an
  -- exception. This is to avoid calling terminateProcess
  -- unless an exception was raised.
  mask $ \restore -> do
        -- acquire
        -- On Windows due to how exec is emulated the old process will exit and
        -- a new process will be created. This means waiting for termination of
        -- the parent process will get you in a race condition as the child may
        -- not have finished yet.  This caused #16450.  To fix this use a
        -- process job to track all child processes and wait for each one to
        -- finish.
        let procdata =
              enableProcessJobs
              $ (proc pgm real_args) {
                cwd = mb_cwd
              , env = mb_env
              , std_in  = CreatePipe

              -- We used to treat stdout/stderr as separate streams, but this
              -- was racy (see #25517).  We now treat them as one stream and
              -- that is fine for our use-case.  We rely on upstream programs
              -- to serialize writes to the two streams appropriately (note
              -- that they already need to do that to produce deterministic
              -- output when used interactively / on the command-line).
              , std_out = UseHandle writeEnd
              , std_err = UseHandle writeEnd
              }
        (Just hStdIn, Nothing, Nothing, hProcess) <- restore $
          createProcess_ "builderMainLoop" procdata
        hClose writeEnd
        r <- try $ restore $ do
          getLocaleEncoding >>= hSetEncoding readEnd
          hSetNewlineMode readEnd nativeNewlineMode
          hSetBuffering readEnd LineBuffering
          messages <- parseBuildMessages . filter_fn . lines <$> hGetContents readEnd
          mapM_ processBuildMessage messages
          waitForProcess hProcess
        hClose hStdIn
        case r of
          Left (SomeException e) -> do
            terminateProcess hProcess
            throw e
          Right s -> do
            return s
  where
    processBuildMessage :: BuildMessage -> IO ()
    processBuildMessage msg = do
      case msg of
        BuildMsg msg -> do
          logInfo logger $ withPprStyle defaultUserStyle msg
        BuildError loc msg -> do
          logMsg logger errorDiagnostic (mkSrcSpan loc loc)
              $ withPprStyle defaultUserStyle msg
#endif

parseBuildMessages :: [String] -> [BuildMessage]
parseBuildMessages str = loop str Nothing
    where
        loop :: [String] -> Maybe BuildMessage -> [BuildMessage]
        loop []     Nothing    = []
        loop []     (Just err) = [err]
        loop (l:ls) in_err     =
                case in_err of
                  Just err@(BuildError srcLoc msg)
                    | leading_whitespace l ->
                        loop ls (Just (BuildError srcLoc (msg $$ text l)))
                    | otherwise ->
                        err : checkError l ls
                  Nothing ->
                        checkError l ls
                  _ -> panic "parseBuildMessages/loop"

        checkError :: String -> [String] -> [BuildMessage]
        checkError l ls
           = case parseError l of
                Nothing ->
                    BuildMsg (text l) : loop ls Nothing
                Just (srcLoc, msg) -> do
                    loop ls (Just (BuildError srcLoc (text msg)))

        leading_whitespace :: String -> Bool
        leading_whitespace []    = False
        leading_whitespace (x:_) = isSpace x

parseError :: String -> Maybe (SrcLoc, String)
parseError s0 = case breakColon s0 of
                Just (filename, s1) ->
                    case breakIntColon s1 of
                    Just (lineNum, s2) ->
                        case breakIntColon s2 of
                        Just (columnNum, s3) ->
                            Just (mkSrcLoc (mkFastString filename) lineNum columnNum, s3)
                        Nothing ->
                            Just (mkSrcLoc (mkFastString filename) lineNum 0, s2)
                    Nothing -> Nothing
                Nothing -> Nothing

-- | Break a line of an error message into a filename and the rest of the line,
-- taking care to ignore colons in Windows drive letters (as noted in #17786).
-- For instance,
--
-- * @"hi.c: ABCD"@ is mapped to @Just ("hi.c", \"ABCD\")@
-- * @"C:\\hi.c: ABCD"@ is mapped to @Just ("C:\\hi.c", \"ABCD\")@
breakColon :: String -> Maybe (String, String)
breakColon = go []
  where
    -- Don't break on Windows drive letters (e.g. @C:\@ or @C:/@)
    go accum  (':':'\\':rest) = go ('\\':':':accum) rest
    go accum  (':':'/':rest)  = go ('/':':':accum) rest
    go accum  (':':rest)      = Just (reverse accum, rest)
    go accum  (c:rest)        = go (c:accum) rest
    go _accum []              = Nothing

breakIntColon :: String -> Maybe (Int, String)
breakIntColon xs = case break (':' ==) xs of
                       (ys, _:zs)
                        | not (null ys) && all isAscii ys && all isDigit ys ->
                           Just (read ys, zs)
                       _ -> Nothing

data BuildMessage
  = BuildMsg   !SDoc
  | BuildError !SrcLoc !SDoc
