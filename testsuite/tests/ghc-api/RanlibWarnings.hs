module Main where

import Control.Exception (bracket_, try)
import Control.Monad (forM_, unless)
import Data.IORef
import Data.List (isInfixOf)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import GHC.SysTools.Tasks (RanlibConfig(..), runRanlib)
import GHC.Utils.CliOption (Option(..))
import GHC.Utils.Logger (initLogger, pushLogHook)
import GHC.Utils.Outputable (showSDocUnsafe)
import GHC.Utils.Panic (GhcException(..))

-- Act as the external tool as well as its caller, avoiding shell scripts and
-- exercising the real process runner and diagnostic logger on every host.
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--mock-ranlib", program, mode] -> do
            mapM_ (hPutStrLn stderr . snd) (diagnostics program)
            whenFailure mode
        [] -> do
            executable <- getExecutablePath
            directory <- getCurrentDirectory
            let tool = directory </> ("ranlib with spaces" ++ takeExtension executable)
                install = do
                    copyFile executable tool
                    getPermissions executable >>= setPermissions tool
            path <- lookupEnv "PATH"
            bracket_ install (removeFile tool) $
                bracket_ (setEnv "PATH" (directory ++ maybe "" ([searchPathSeparator] ++) path))
                         (maybe (unsetEnv "PATH") (setEnv "PATH") path) $
                    -- A launcher found through PATH can report a full tool path.
                    forM_ [tool, takeFileName tool] $ \program ->
                        forM_ ["success", "failure"] (check program tool)
        _ -> error "unexpected tool arguments"
  where
    whenFailure "success" = pure ()
    whenFailure "failure" = exitWith (ExitFailure 7)
    whenFailure _ = error "unexpected tool mode"

check :: FilePath -> FilePath -> String -> IO ()
check program tool mode = do
    messages <- newIORef []
    logger0 <- initLogger
    let logger = pushLogHook (const (\_ _ _ message ->
            modifyIORef' messages (++ [showSDocUnsafe message]))) logger0
    result <- try (runRanlib logger (RanlibConfig program)
                   [Option "--mock-ranlib", Option tool, Option mode])
        :: IO (Either GhcException ())
    actual <- readIORef messages
    let expected = [message | (False, message) <- diagnostics tool]
    unless (actual == expected) $
        error ("changed neighboring diagnostics: " ++ show actual)
    case (mode, result) of
        ("success", Right ()) -> pure ()
        ("failure", Left (ProgramError message))
            | "Exit code: 7" `isInfixOf` message -> pure ()
        _ -> error ("changed tool exit result: " ++ show result)

-- True marks the complete Apple empty-member diagnostics. Keep lookalikes,
-- other programs' messages, and genuine warnings/errors in their original order.
diagnostics :: FilePath -> [(Bool, String)]
diagnostics program =
    [ (False, "diagnostic before the warnings")
    , (True, program ++ ": file: library with spaces.a(empty.o) has no symbols")
    , (True, takeFileName program ++ ": file: liba.a(empty.o) has no symbols")
    , (True, program ++ ": warning: 'liba.a(empty.o)' has no symbols")
    , (True, takeFileName program ++ ": warning: 'liba.a(empty.o)' has no symbols")
    , (False, program ++ ": error: file: liba.a(empty.o) has no symbols")
    , (False, "error: " ++ program ++ ": file: liba.a(empty.o) has no symbols")
    , (False, program ++ ": file: liba.a(empty.o) has no symbols; malformed object")
    , (False, program ++ ": file: not-an-archive-member.o has no symbols")
    , (False, program ++ ": file: empty.o) has no symbols")
    , (False, program ++ ": file: (empty.o) has no symbols")
    , (False, program ++ ": file: liba.a() has no symbols")
    , (False, program ++ ": warning: liba.a(empty.o) has no symbols")
    , (False, program ++ "-other: file: liba.a(empty.o) has no symbols")
    , (False, ("relative toolchain" </> takeFileName program) ++ ": file: liba.a(empty.o) has no symbols")
    , (False, "diagnostic after the warnings")
    ]
