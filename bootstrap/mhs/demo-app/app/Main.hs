module Main (main) where

import Data.List (isPrefixOf)
import Demo.Report
  ( buildReport,
    checkReport,
    renderReport,
  )
import System.Exit (exitFailure, exitSuccess)
import System.Info (compilerName, fullCompilerVersion)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  putStrLn ("compiler: " ++ compilerName ++ " " ++ fullCompilerVersion)
  let report = buildReport 30
  putStr (renderReport report)
  let results = checkReport report
  mapM_ putStrLn results
  if all ("PASS:" `isPrefixOf`) results
    then putStrLn "All self-checks passed." >> exitSuccess
    else hPutStrLn stderr "Self-checks FAILED." >> exitFailure
