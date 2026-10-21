module SetupHostPlatformRegression (main) where

import Distribution.System
  ( Arch (OtherArch, X86_64)
  , OS (Linux, OtherOS)
  , Platform (Platform)
  , platformFromTriple
  )
import Main (renderPlatformHost)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (writeFile)

main :: IO ()
main = do
  [otherOsDir, otherArchDir, linuxDir] <- getArgs
  writeHost otherOsDir (Platform X86_64 (OtherOS "linux-gnu"))
  writeHost otherArchDir (Platform (OtherArch "new-cpu") Linux)
  linux <- maybe (die "Cabal rejected x86_64-unknown-linux-gnu") pure
    (platformFromTriple "x86_64-unknown-linux-gnu")
  if linux /= Platform X86_64 Linux
    then die ("Cabal classified Linux triple unexpectedly: " ++ show linux)
    else writeHost linuxDir linux
  where
    writeHost directory platform = do
      let path = directory </> "GHC" </> "Platform" </> "Host.hs"
      createDirectoryIfMissing True (directory </> "GHC" </> "Platform")
      writeFile path (renderPlatformHost platform)
