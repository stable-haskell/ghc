module GHC.Linker.Static
   ( linkBinary
   , linkStaticLib
   )
where

import GHC.Driver.DynFlags (ExecutableLinkMode(..))
import GHC.Prelude
import GHC.Platform
import GHC.Platform.Ways
import GHC.Settings

import GHC.SysTools
import GHC.SysTools.Ar

import GHC.Unit.Env
import GHC.Unit.Types
import GHC.Unit.Info
import GHC.Unit.State

import GHC.Utils.Logger
import GHC.Utils.Monad
import GHC.Utils.Misc
import GHC.Utils.TmpFs

import GHC.Linker.MacOS
import GHC.Linker.Unit
import GHC.Linker.Dynamic
import GHC.Linker.Executable (initExecutableLinkOpts, mkExtraObjToLinkIntoBinary, mkNoteObjsToLinkIntoBinary)
import GHC.Linker.External
import GHC.Linker.Windows
import GHC.Linker.Static.Utils

import GHC.Driver.Config.Linker
import GHC.Driver.Session

import GHC.Data.FastString

import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe

-----------------------------------------------------------------------------
-- Static linking, of .o files

-- The list of packages passed to link is the list of packages on
-- which this program depends, as discovered by the compilation
-- manager.  It is combined with the list of packages that the user
-- specifies on the command line with -package flags.
--
-- In one-shot linking mode, we can't discover the package
-- dependencies (because we haven't actually done any compilation or
-- read any interface files), so the user must explicitly specify all
-- the packages.

{-
Note [-Xlinker -rpath vs -Wl,-rpath]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-Wl takes a comma-separated list of options which in the case of
-Wl,-rpath -Wl,some,path,with,commas parses the path with commas
as separate options.
Buck, the build system, produces paths with commas in them.

-Xlinker doesn't have this disadvantage and as far as I can tell
it is supported by both gcc and clang. Anecdotally nvcc supports
-Xlinker, but not -Wl.
-}

{-
Note [Export dynamic symbols for GHC API programs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Programs linking against the ghc package need to export symbols from
the RTS to dynamically loaded libraries. When running GHCi or Template
Haskell, these programs load Haskell shared libraries via dlopen() that
reference RTS symbols like stg_INTLIKE_closure. Without exporting these
symbols from the executable, dlopen will fail with "undefined symbol".

Platform-specific solutions:
  Linux/FreeBSD: -rdynamic (passes --export-dynamic to ld)
  macOS: -flat_namespace (makes all symbols visible across namespaces)
  Windows: not needed (--enable-auto-import handles this)

We apply this unconditionally for non-static executables linking against the
ghc package, regardless of whether -dynamic is passed. This is because the
GHC API may load shared libraries at runtime (via dlopen) even when the
executable itself wasn't compiled with -dynamic. We only skip this for
FullyStatic executables since they won't be loading dynamic libraries.

This is the same issue that ghc-iserv faces, and is documented in
utils/ghc-iserv/ghc-iserv.cabal.in as Note [ghc-iserv and dynamic symbol export].
-}

{-
Note [RTS sublibrary rpath injection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC selects RTS sublibraries (rts:nonthreaded-nodebug, rts:threaded-nodebug, etc.)
at link time based on flags like -threaded and -debug. This selection happens
inside GHC, not through Cabal's dependency resolution.

When Cabal uses -dynload deploy, it tells GHC "don't inject rpaths, I'll handle
them via -optl-Wl,-rpath,...". However, Cabal cannot inject rpaths for:
  - RTS sublibraries: Cabal doesn't know which sublibrary GHC will select
  - RTS dependencies: e.g., libffi-clib is a dependency of the RTS

This is not just a workaround for Cabal issue #11221. Even if Cabal passes
-rpath to GHC and lets GHC handle all rpaths, GHC would still need special
handling for RTS-related libraries because Cabal fundamentally cannot see them.

We handle this by always injecting rpaths for:
  1. Any package named "rts" (all sublibraries share the package name)
  2. Any direct dependency of an RTS package

This is computed dynamically from the package database rather than hardcoded,
so it remains correct if the RTS gains additional library dependencies.
-}

-- | Check if a package is the RTS or a direct dependency of the RTS.
-- See Note [RTS sublibrary rpath injection]
isRtsOrRtsDep :: [UnitInfo] -> UnitInfo -> Bool
isRtsOrRtsDep pkgs pkg = isRts || isRtsDep
  where
    isRts = unitPackageNameString pkg == "rts"
    -- Collect all direct dependencies of RTS packages
    rtsDeps = [ dep | rtsPkg <- pkgs
                    , unitPackageNameString rtsPkg == "rts"
                    , dep <- unitDepends rtsPkg ]
    isRtsDep = unitId pkg `elem` rtsDeps

linkBinary :: Logger -> TmpFs -> DynFlags -> ExecutableLinkMode -> UnitEnv -> [FilePath] -> [UnitId] -> IO ()
linkBinary = linkBinary' False

linkBinary' :: Bool -> Logger -> TmpFs -> DynFlags -> ExecutableLinkMode -> UnitEnv -> [FilePath] -> [UnitId] -> IO ()
linkBinary' staticLink logger tmpfs dflags blm unit_env o_files dep_units = do
    let platform   = ue_platform unit_env
        unit_state = ue_homeUnitState unit_env
        toolSettings' = toolSettings dflags
        verbFlags = getVerbFlags dflags
        arch_os   = platformArchOS platform
        output_fn = exeFileName arch_os staticLink (outputFile_ dflags)
        namever   = ghcNameVersion dflags
        supportsVerbatim = toolSettings_ldSupportsVerbatimNamespace (toolSettings dflags)
        ways_ = ways dflags

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.
    pkgs <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_units)

    -- Collect per-package library dirs (deduplicated, non-empty)
    let pkg_lib_paths     = collectLibraryDirs ways_ pkgs
    -- See Note [RTS sublibrary rpath injection]
    -- Build linker options per (pkg, libdir)
    let pkg_lib_path_opts =
          concat
          [ get_pkg_lib_path_opts pkg l
          | pkg <- pkgs
          , l <- collectLibraryDirs ways_ [pkg]
          ]
        get_pkg_lib_path_opts pkg l
         | osElfTarget (platformOS platform) &&
           (dynLibLoader dflags == SystemDependent || isRtsOrRtsDep pkgs pkg) &&
           ways_ `hasWay` WayDyn
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                  rpath = if useXLinkerRPath dflags (platformOS platform)
                          then ["-Xlinker", "-rpath", "-Xlinker", libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if (platformOS platform) == OSSolaris2
                              then []
                              else ["-Xlinker", "-rpath-link", "-Xlinker", l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | osMachOTarget (platformOS platform) &&
           (dynLibLoader dflags == SystemDependent || isRtsOrRtsDep pkgs pkg) &&
           ways_ `hasWay` WayDyn &&
           useXLinkerRPath dflags (platformOS platform)
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "@loader_path" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
              in ["-L" ++ l] ++ ["-Xlinker", "-rpath", "-Xlinker", libpath]
         | otherwise = ["-L" ++ l]

    pkg_lib_path_opts <-
      if gopt Opt_SingleLibFolder dflags
      then do
        libs <- getLibs namever ways_ unit_env dep_units
        tmpDir <- newTempSubDir logger tmpfs (tmpDir dflags)
        sequence_ [ copyFile lib (tmpDir </> basename)
                  | (lib, basename) <- libs]
        return [ "-L" ++ tmpDir ]
      else pure pkg_lib_path_opts

    let
      dead_strip
        | gopt Opt_WholeArchiveHsLibs dflags = []
        | otherwise = if osSubsectionsViaSymbols (platformOS platform)
                        then ["-Wl,-dead_strip"]
                        else []
    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    let execOpts = initExecutableLinkOpts dflags
    extraLinkObj <- maybeToList <$> mkExtraObjToLinkIntoBinary logger tmpfs execOpts unit_state
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary logger tmpfs execOpts unit_env dep_units

    let
      (pre_hs_libs, post_hs_libs)
        | gopt Opt_WholeArchiveHsLibs dflags
        = if platformOS platform == OSDarwin
            then (["-Wl,-all_load"], [])
              -- OS X does not have a flag to turn off -all_load
            else (["-Wl,--whole-archive"], ["-Wl,--no-whole-archive"])
        | otherwise
        = ([],[])

    pkg_link_opts <- do
        -- If we link fully statically, we just append @-static@ at the end of the linker line.
        -- If we link declared external libraries statically only, we have to adjust each of them
        -- carefully (see 'getUnitLinkOpts'). We don't need to do the latter if we link fully static.
        unit_link_opts <- getUnitLinkOpts namever ways_ (Just (blm, supportsVerbatim)) unit_env dep_units
        return $ otherFlags unit_link_opts ++ dead_strip
                  ++ pre_hs_libs ++ hsLibs unit_link_opts ++ post_hs_libs
                  ++ extraLibs unit_link_opts
                  ++ (if blm == FullyStatic then ["-static"] else [])
                 -- -Wl,-u,<sym> contained in other_flags
                 -- needs to be put before -l<package>,
                 -- otherwise Solaris linker fails linking
                 -- a binary with unresolved symbols in RTS
                 -- which are defined in base package
                 -- the reason for this is a note in ld(1) about
                 -- '-u' option: "The placement of this option
                 -- on the command line is significant.
                 -- This option must be placed before the library
                 -- that defines the symbol."

    -- frameworks
    pkg_framework_opts <- getUnitFrameworkOpts unit_env dep_units
    let framework_opts = getFrameworkOpts (initFrameworkOpts dflags) platform

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    rc_objs <- case platformOS platform of
      OSMinGW32 | gopt Opt_GenManifest dflags -> maybeCreateManifest logger tmpfs dflags output_fn
      _                                       -> return []

    let require_cxx = any ((==) (PackageName (fsLit "system-cxx-std-lib")) . unitPackageName) pkgs

    let linker_config = initLinkerConfig dflags require_cxx
    let link dflags args = do
          runLink logger tmpfs linker_config args
          -- Make sure to honour -fno-use-rpaths if set on darwin as well; see #20004
          when (platformOS platform == OSDarwin && gopt Opt_RPath dflags) $
            GHC.Linker.MacOS.runInjectRPaths logger (toolSettings dflags) pkg_lib_paths output_fn

    link dflags (
                       map GHC.SysTools.Option verbFlags
                      ++ [ GHC.SysTools.Option "-o"
                         , GHC.SysTools.FileOption "" output_fn
                         ]
                      ++ libmLinkOpts platform
                      ++ map GHC.SysTools.Option (
                         []

                      -- See Note [No PIE when linking]
                      ++ pieCCLDOpts dflags

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (if platformOS platform == OSMinGW32
                          then ["-Wl,--enable-auto-import"]
                          else [])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ (if not (gopt Opt_CompactUnwind dflags) &&
                             toolSettings_ldSupportsCompactUnwind toolSettings' &&
                             (platformOS platform == OSDarwin) &&
                             case platformArch platform of
                               ArchX86_64  -> True
                               ArchAArch64 -> True
                               _ -> False
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                          -- We should rather be asking does it support --gc-sections?
                      ++ (if toolSettings_ldIsGnuLd toolSettings' &&
                             not (gopt Opt_WholeArchiveHsLibs dflags)
                          then ["-Wl,--gc-sections"]
                          else [])

                      -- See Note [Export dynamic symbols for GHC API programs]
                      ++ (if blm /= FullyStatic &&
                             any ((== "ghc") . unitPackageNameString) pkgs
                          then case platformOS platform of
                                 os | osElfTarget os -> ["-rdynamic"]
                                 OSDarwin -> ["-Wl,-flat_namespace"]
                                 -- OSMinGW32 already has --export-all-symbols via --enable-auto-import
                                 _ -> []
                          else [])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map GHC.SysTools.Option (
                         rc_objs
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj
                      ++ noteLinkObjs
                      -- See Note [RTS/ghc-internal interface]
                      -- (-u<sym> must come before -lghc-internal...!)
                      ++ (if ghcInternalUnitId `elem` map unitId pkgs
                          then [concat [ "-Wl,-u,"
                                       , ['_' | platformLeadingUnderscore platform]
                                       , "init_ghc_hs_iface" ]]
                          else [])
                      ++ pkg_link_opts
                      ++ pkg_framework_opts
                      ++ (if platformOS platform == OSDarwin
                          --  dead_strip_dylibs, will remove unused dylibs, and thus save
                          --  space in the load commands. The -headerpad is necessary so
                          --  that we can inject more @rpath's later for the left over
                          --  libraries during runInjectRpaths phase.
                          --
                          --  See Note [Dynamic linking on macOS].
                          then [ "-Wl,-dead_strip_dylibs", "-Wl,-headerpad,8000" ]
                          else [])
                    ))

-- | Linking a static lib will not really link anything. It will merely produce
-- a static archive of all dependent static libraries. The resulting library
-- will still need to be linked with any remaining link flags.
linkStaticLib :: Logger -> DynFlags -> UnitEnv -> [String] -> [UnitId] -> IO ()
linkStaticLib logger dflags unit_env o_files dep_units = do
  let platform  = ue_platform unit_env
      extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
      modules = o_files ++ extra_ld_inputs
      arch_os = platformArchOS platform
      output_fn = exeFileName arch_os True (outputFile_ dflags)
      namever = ghcNameVersion dflags
      ways_   = ways dflags

  full_output_fn <- if isAbsolute output_fn
                    then return output_fn
                    else do d <- getCurrentDirectory
                            return $ normalise (d </> output_fn)
  output_exists <- doesFileExist full_output_fn
  (when output_exists) $ removeFile full_output_fn

  pkg_cfgs_init <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_units)

  let pkg_cfgs
        | gopt Opt_LinkRts dflags
        = pkg_cfgs_init
        | otherwise
        = filter ((/= PackageName (fsLit "rts")) . unitPackageName) pkg_cfgs_init

  archives <- concatMapM (collectArchives namever ways_) pkg_cfgs

  ar <- foldl mappend
        <$> (Archive <$> mapM loadObj modules)
        <*> mapM loadAr archives

  if toolSettings_ldIsGnuLd (toolSettings dflags)
    then writeGNUAr output_fn $ afilter (not . isGNUSymdef) ar
    else writeBSDAr output_fn $ afilter (not . isBSDSymdef) ar

  -- run ranlib over the archive. write*Ar does *not* create the symbol index.
  runRanlib logger dflags [GHC.SysTools.FileOption "" output_fn]
