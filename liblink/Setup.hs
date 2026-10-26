{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- The Cabal library is an inert marker. The linker implementation is built
-- separately so native clients link a library whose basename is "link".
import Control.Monad (forM_, unless, when)
import Data.Char (isAlphaNum)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (intercalate, isPrefixOf)
import qualified Distribution.InstalledPackageInfo as InstalledInfo
import Distribution.PackageDescription
  ( BuildInfo (..)
  , Library (..)
  , PackageDescription (..)
  , updatePackageDescription
  )
import Distribution.Pretty (prettyShow)
import Distribution.Simple
  ( autoconfUserHooks
  , defaultMainWithHooks
  , simpleUserHooks
  )
import Distribution.Simple.Compiler (interpretPackageDBStack)
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (..)
  , absoluteComponentInstallDirs
  , buildDir
  , interpretSymbolicPathLBI
  , libdir
  , localUnitId
  , mbWorkDirLBI
  , packageRoot
  , dynlibdir
  , withLibLBI
  )
import Distribution.Simple.PackageIndex (lookupUnitId)
import Distribution.Simple.Program
  ( ConfiguredProgram
  , ProgramInvocation (..)
  , arProgram
  , ghcProgram
  , requireProgram
  , runProgram
  , runProgramInvocation
  , simpleProgramInvocation
  )
import Distribution.Simple.Program.GHC (packageDbArgsDb)
import Distribution.Simple.Setup
  ( BuildFlags (..)
  , ConfigFlags (..)
  , CopyDest (..)
  , CopyFlags (..)
  , InstallFlags (..)
  , configCommonFlags
  , configureArgs
  , configProgramPathExtra
  , configProgramPaths
  , fromFlag
  , setupVerbosity
  )
import Distribution.Simple.PackageDescription (readHookedBuildInfo)
import Distribution.Simple.UserHooks (UserHooks (..))
import Distribution.Simple.Utils
  ( findHookedPackageDesc
  , withEnvOverrides
  , withExtraPathEnv
  )
import Distribution.System (OS (..), Platform (..), buildPlatform)
import Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo (..))
import Distribution.Types.Flag (showFlagValue, unFlagAssignment, unFlagName)
import Distribution.Types.HookedBuildInfo (emptyHookedBuildInfo)
import Distribution.Types.UnitId (UnitId)
import Distribution.Utils.NubList (fromNubList)
import Distribution.Utils.Path
  ( getSymbolicPath
  , makeRelativePathEx
  , makeSymbolicPath
  , (</>)
  )
import Distribution.Verbosity (Verbosity, defaultVerbosityHandles, mkVerbosity)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesFileExist
  , makeAbsolute
  , removeFile
  )
import qualified System.FilePath as FP

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks =
  autoconfUserHooks
    { postConf = nativePostConfigure
    , buildHook = nativeBuildPackage
    , copyHook = nativeCopyPackage
    , instHook = nativeInstallPackage
    }

nativePostConfigure :: [String] -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
nativePostConfigure args flags pkg lbi = do
  let common = configCommonFlags flags
      verbosity = mkVerbosity defaultVerbosityHandles (fromFlag (setupVerbosity common))
      workDir = mbWorkDirLBI lbi
  let configurePath = packageRoot common FP.</> "configure"
  buildPath <- makeAbsolute (interpretSymbolicPathLBI lbi (buildDir lbi))
  createDirectoryIfMissing True buildPath
  configureFile <- toUnix <$> makeAbsolute configurePath
  let flagEnv =
        [ ("CABAL_FLAG_" ++ map flagEnvChar (unFlagName flag), Just (if enabled then "1" else "0"))
        | (flag, enabled) <- unFlagAssignment (flagAssignment lbi)
        ]
      cabalFlags = [("CABAL_FLAGS", Just (unwords (map showFlagValue (unFlagAssignment (flagAssignment lbi)))))]
      programEnv =
        [ (envName, canonicalisePathSeparator <$> lookup programName (configProgramPaths flags))
        | (envName, programName) <- [("CC", "gcc"), ("CXX", "gpp"), ("GHC", "ghc"), ("GHC_PKG", "ghc-pkg")]
        ]
      configureInvocation =
        (simpleProgramInvocation "sh" (configureFile : configureArgs flags ++ ["--host=" ++ platformTriple (hostPlatform lbi), "--build=" ++ platformTriple buildPlatform]))
          { progInvokeCwd = Just buildPath }
  withExtraPathEnv verbosity (fromNubList (configProgramPathExtra flags)) $
    withEnvOverrides verbosity (flagEnv ++ cabalFlags ++ programEnv) $
      runProgramInvocation verbosity configureInvocation
  maybeInfo <- findHookedPackageDesc verbosity workDir (buildDir lbi)
  hooked <- maybe (pure emptyHookedBuildInfo) (readHookedBuildInfo verbosity workDir) maybeInfo
  let configuredPkg = updatePackageDescription hooked pkg
  buildNative verbosity configuredPkg lbi
  (checkPkg, checkLbi) <- withBuildLibraryPath configuredPkg lbi
  postConf simpleUserHooks args flags checkPkg checkLbi

flagEnvChar :: Char -> Char
flagEnvChar c
  | isAlphaNum c = c
  | otherwise = '_'

canonicalisePathSeparator :: FilePath -> FilePath
canonicalisePathSeparator = map replaceSeparator
  where
    replaceSeparator c
      | FP.isPathSeparator c = '/'
      | otherwise = c

toUnix :: FilePath -> FilePath
#ifdef mingw32_HOST_OS
toUnix path =
  let normalized = FP.normalise path
      (drive, rest) = FP.splitDrive normalized
      prefix = case drive of
        [] -> "/"
        first : _ -> ['/', first, '/']
   in prefix ++ intercalate "/" (FP.splitDirectories rest)
#else
toUnix = id
#endif

platformTriple :: Platform -> String
platformTriple (Platform arch os) = prettyShow arch ++ "-" ++ osTriple os
  where
    osTriple Linux = "unknown-linux"
    osTriple Windows = "unknown-mingw32"
    osTriple OSX = "apple-darwin"
    osTriple FreeBSD = "unknown-freebsd"
    osTriple OpenBSD = "unknown-openbsd"
    osTriple NetBSD = "unknown-netbsd"
    osTriple DragonFly = "unknown-dragonflybsd"
    osTriple Solaris = "sun-solaris"
    osTriple AIX = "ibm-aix"
    osTriple HPUX = "hp-hpux"
    osTriple IRIX = "sgi-irix"
    osTriple HaLVM = "unknown-halvm"
    osTriple Hurd = "unknown-gnu"
    osTriple IOS = "apple-ios"
    osTriple Android = "unknown-linux-android"
    osTriple Ghcjs = "unknown-ghcjs"
    osTriple Wasi = "unknown-wasi"
    osTriple Haiku = "unknown-haiku"
    osTriple (OtherOS other) = "unknown-" ++ other

nativeBuildPackage :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
nativeBuildPackage pkg lbi hooksForBuild flags = do
  let verbosity = mkVerbosity defaultVerbosityHandles (fromFlag (setupVerbosity (buildCommonFlags flags)))
  buildNative verbosity pkg lbi
  (buildPkg, buildLbi) <- withBuildLibraryPath pkg lbi
  buildHook simpleUserHooks buildPkg buildLbi hooksForBuild flags

nativeCopyPackage :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
nativeCopyPackage pkg lbi hooksForCopy flags = do
  copyHook simpleUserHooks pkg lbi hooksForCopy flags
  copyNativeArtifacts pkg lbi (fromFlag (copyDest flags))

nativeInstallPackage :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
nativeInstallPackage pkg lbi hooksForInstall flags = do
  instHook simpleUserHooks pkg lbi hooksForInstall flags
  copyNativeArtifacts pkg lbi (fromFlag (installDest flags))

-- The temporary path lets Cabal check the configured external library at
-- configure time and lets the marker component link at build time. It is
-- deliberately never written back to setup-config or package registration.
withBuildLibraryPath :: PackageDescription -> LocalBuildInfo -> IO (PackageDescription, LocalBuildInfo)
withBuildLibraryPath pkg lbi = do
  absoluteNativeDir <- makeAbsolute (nativeBuildDir lbi)
  let dir = makeSymbolicPath absoluteNativeDir
      pkg' = pkg {library = fmap addPath (library pkg)}
      addPath lib =
        let bi = libBuildInfo lib
         in lib
              { libBuildInfo =
                  bi
                    { extraLibDirs = dir : extraLibDirs bi
                    , extraLibDirsStatic = dir : extraLibDirsStatic bi
                    }
              }
  pure (pkg', lbi {localPkgDescr = pkg'})

buildNative :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
buildNative verbosity pkg lbi = do
  sources <- nativeSources lbi
  let bi = maybe mempty libBuildInfo (library pkg)
  outDir <- makeAbsolute (nativeBuildDir lbi)
  configInc <- makeAbsolute (interpretSymbolicPathLBI lbi (buildDir lbi </> makeRelativePathEx "include"))
  sourceRoot <- makeAbsolute (interpretSymbolicPathLBI lbi (makeRelativePathEx "src"))
  publicInc <- makeAbsolute (interpretSymbolicPathLBI lbi (makeRelativePathEx "include"))
  fsIncludeDirs <- mapM makeAbsolute =<< rtsFsIncludeDirs pkg lbi
  nativeLibDirs <- mapM (makeAbsolute . getSymbolicPath) (extraLibDirs bi)
  let objDir = outDir FP.</> "obj"
  createDirectoryIfMissing True objDir
  (ghc, programs) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  (ar, _) <- requireProgram verbosity arProgram programs
  objects <- mapM (compileOne verbosity ghc objDir lbi bi (configInc : publicInc : sourceRoot : fsIncludeDirs)) sources
  when (withVanillaLib lbi || withStaticLib lbi) $ do
    let archive = outDir FP.</> "liblink.a"
    removeIfPresent archive
    runProgram verbosity ar (["rcs", archive] ++ objects)
  when (withSharedLib lbi) $ do
    let shared = outDir FP.</> sharedLibraryName (hostPlatform lbi)
    fsUnit <- rtsFsUnitId pkg lbi
    let dbArgs = packageDbArgsDb (interpretPackageDBStack (mbWorkDirLBI lbi) (withPackageDB lbi))
        -- This C library needs only the explicit rts-fs dependency. Hiding
        -- packages does not disable GHC's automatic runtime dependencies.
        linkFlags =
          ["-shared", "-dynamic", "-no-rts", "-hide-all-packages", "-no-auto-link-packages"]
            ++ dbArgs
            ++ ["-package-id", prettyShow fsUnit, "-o", shared]
            ++ concatMap (\d -> ["-L" ++ d]) nativeLibDirs
            ++ concatMap (\l -> ["-l" ++ l]) (filter (/= "link") (extraLibs bi))
            ++ concatMap (\o -> ["-optl" ++ o]) (ldOptions bi)
            ++ sharedLinkerArgs (hostPlatform lbi)
            ++ objects
    removeIfPresent shared
    runProgram verbosity ghc linkFlags

compileOne
  :: Verbosity
  -> ConfiguredProgram
  -> FilePath
  -> LocalBuildInfo
  -> BuildInfo
  -> [FilePath]
  -> FilePath
  -> IO FilePath
compileOne verbosity ghc objDir lbi bi includeRoots source = do
  root <- makeAbsolute (packageRoot (configCommonFlags (configFlags lbi)))
  let relativeSource = FP.makeRelative root source
      obj = objDir FP.</> FP.replaceExtension relativeSource "o"
      picFlags = case hostPlatform lbi of
        Platform _ Windows -> []
        _ -> ["-optc-fPIC"]
      args =
        ["-c", "-no-rts"]
          ++ concatMap (\p -> ["-optc-I" ++ p]) includeRoots
          ++ concatMap (\o -> ["-optc" ++ o]) (ccOptions bi ++ cppOptions bi)
          ++ picFlags
          ++ ["-o", obj, source]
  createDirectoryIfMissing True (FP.takeDirectory obj)
  runProgram verbosity ghc args
  makeAbsolute obj

nativeSources :: LocalBuildInfo -> IO [FilePath]
nativeSources lbi = do
  root <- makeAbsolute (packageRoot (configCommonFlags (configFlags lbi)))
  let listFile = root FP.</> "native-sources.list"
  contents <- readFile listFile
  mapM (makeAbsolute . (root FP.</>))
    [ line
    | raw <- lines contents
    , let line = trim raw
    , not (null line)
    , not ("#" `isPrefixOf` line)
    ]

rtsFsUnitId :: PackageDescription -> LocalBuildInfo -> IO UnitId
rtsFsUnitId pkg lbi = do
  ref <- newIORef []
  withLibLBI pkg lbi (\_ clbi -> modifyIORef' ref (<> map fst (componentPackageDeps clbi)))
  deps <- readIORef ref
  case deps of
    [uid] -> pure uid
    other -> ioError (userError ("liblink expected exactly one rts-fs library dependency, found " ++ show other))

rtsFsIncludeDirs :: PackageDescription -> LocalBuildInfo -> IO [FilePath]
rtsFsIncludeDirs pkg lbi = do
  uid <- rtsFsUnitId pkg lbi
  case lookupUnitId (installedPkgs lbi) uid of
    Nothing -> ioError (userError ("liblink could not find installed rts-fs unit " ++ show uid))
    Just (ipi :: InstalledInfo.InstalledPackageInfo) -> pure (InstalledInfo.includeDirs ipi)

copyNativeArtifacts :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
copyNativeArtifacts pkg lbi dest = do
  let dirs = absoluteComponentInstallDirs pkg lbi (localUnitId lbi) dest
  outDir <- makeAbsolute (nativeBuildDir lbi)
  let artifacts =
        [(outDir FP.</> "liblink.a", libdir dirs) | withVanillaLib lbi || withStaticLib lbi]
          ++ [(outDir FP.</> sharedLibraryName (hostPlatform lbi), dynlibdir dirs) | withSharedLib lbi]
  forM_ artifacts $ \(src, dir) -> do
    exists <- doesFileExist src
    unless exists (ioError (userError ("liblink native artifact is missing: " ++ src)))
    createDirectoryIfMissing True dir
    copyFile src (dir FP.</> FP.takeFileName src)

nativeBuildDir :: LocalBuildInfo -> FilePath
nativeBuildDir lbi =
  interpretSymbolicPathLBI lbi (buildDir lbi </> makeRelativePathEx "native-linker")

sharedLibraryName :: Platform -> FilePath
sharedLibraryName (Platform _ OSX) = "liblink.dylib"
sharedLibraryName (Platform _ Windows) = "link.dll"
sharedLibraryName _ = "liblink.so"

sharedLinkerArgs :: Platform -> [String]
sharedLinkerArgs (Platform _ OSX) = ["-optl-Wl,-install_name,@rpath/liblink.dylib", "-optl-Wl,-rpath,@loader_path"]
sharedLinkerArgs (Platform _ Windows) = []
sharedLinkerArgs _ = ["-optl-Wl,-soname,liblink.so", "-optl-Wl,-rpath,$ORIGIN"]

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  exists <- doesFileExist path
  when exists (removeFile path)

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
