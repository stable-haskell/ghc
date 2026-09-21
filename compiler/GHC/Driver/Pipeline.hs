{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module GHC.Driver.Pipeline (
   -- * Run a series of compilation steps in a pipeline, for a
   -- collection of source files.
   oneShot, compileFile,

   -- * Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess,
   compileOne, compileOne',
   compileForeign, compileEmptyStub,
   mkRtsCmmObjs, rtsCmmObjsForLink,

   -- * Linking
   link,
#if defined(HAVE_INTERPRETER)
   linkingNeeded, checkLinkInfo,
#endif

   -- * PipeEnv
   PipeEnv(..), mkPipeEnv, phaseOutputFilenameNew,

   -- * Running individual phases
   TPhase(..), runPhase,
   hscPostBackendPhase,

   -- * Constructing Pipelines
   TPipelineClass, MonadUse(..),

   preprocessPipeline, fullPipeline, hscPipeline, hscBackendPipeline, hscPostBackendPipeline,
   hscGenBackendPipeline, asPipeline, viaCPipeline, cmmCppPipeline, cmmPipeline, jsPipeline,
   llvmPipeline, llvmLlcPipeline, llvmManglePipeline, pipelineStart,

   -- * Default method of running a pipeline
   runPipeline
) where


import GHC.Prelude
import GHC.Builtin.Names

import GHC.Platform

import GHC.Utils.Monad ( MonadIO(liftIO), mapMaybeM )

import GHC.Driver.Main
import GHC.Driver.Env hiding ( Hsc )
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Phases
import GHC.Driver.Pipeline.Execute
import GHC.Driver.Pipeline.Phases
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Ppr
import GHC.Driver.Hooks

import GHC.Platform.Ways

import GHC.SysTools
import GHC.SysTools.Cpp
import GHC.Utils.TmpFs

import GHC.Linker.Executable
import GHC.Linker.Static
import GHC.Linker.Static.Utils
import GHC.Linker.Types
import GHC.Linker.Dynamic      ( dynLibLinksRts )
import GHC.Linker.RtsCmm ( GenFile, genFiles, genFile, genFileVecWidth )
import GHC.Settings            ( ToolSettings(..) )
import GHC.Driver.IncludeSpecs ( addGlobalInclude )
import qualified GHC.Data.ShortText as ST

#if defined(HAVE_JS_BACKEND)
import GHC.Driver.Config.StgToJS
import GHC.StgToJS.Linker.Linker
#endif

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Exception as Exception
import GHC.Utils.Logger

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.FastString     ( mkFastString )
import GHC.Data.StringBuffer   ( hPutStringBuffer )
import GHC.Data.Maybe          ( expectJust )

import GHC.Iface.Make          ( mkFullIface )
import GHC.Iface.Load          ( getGhcPrimIface )
#if defined(HAVE_INTERPRETER)
import GHC.Runtime.Loader      ( initializePlugins )
#endif


import GHC.Types.Basic       ( SuccessFlag(..), ForeignSrcLang(..) )
import GHC.Types.Error       ( singleMessage, getMessages, mkSimpleUnknownDiagnostic, defaultDiagnosticOpts )
import GHC.Types.ForeignStubs (ForeignStubs (NoStubs))
import GHC.Types.Target
import GHC.Types.SrcLoc
import GHC.Types.SourceFile
import GHC.Types.SourceError

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Home.ModInfo
import GHC.Unit.Home.PackageTable

import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import qualified Control.Monad.Catch as MC (handle)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Time        ( getCurrentTime )
import GHC.Iface.Recomp
import GHC.Types.Unique.DSet

-- Simpler type synonym for actions in the pipeline monad
type P m = TPipelineClass TPhase m

-- ---------------------------------------------------------------------------
-- Pre-process

-- | Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).
--
-- We return the augmented DynFlags, because they contain the result
-- of slurping in the OPTIONS pragmas

preprocess :: HscEnv
           -> FilePath -- ^ input filename
           -> Maybe InputFileBuffer
           -- ^ optional buffer to use instead of reading the input file
           -> Maybe Phase -- ^ starting phase
           -> IO (Either DriverMessages (DynFlags, FilePath))
preprocess hsc_env input_fn mb_input_buf mb_phase =
  handleSourceError (\err -> return $ Left $ to_driver_messages $ srcErrorMessages err) $
  MC.handle handler $
  fmap Right $ do
  massertPpr (isJust mb_phase || isHaskellSrcFilename input_fn) (text input_fn)
  input_fn_final <- mkInputFn
  let preprocess_pipeline = preprocessPipeline pipe_env (setDumpPrefix pipe_env hsc_env) input_fn_final
  runPipeline (hsc_hooks hsc_env) preprocess_pipeline

  where
    srcspan = srcLocSpan $ mkSrcLoc (mkFastString input_fn) 1 1
    handler (ProgramError msg) =
      return $ Left $ singleMessage $
        mkPlainErrorMsgEnvelope srcspan $
        DriverUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError noHints $ text msg
    handler ex = throwGhcExceptionIO ex

    to_driver_messages :: Messages GhcMessage -> Messages DriverMessage
    to_driver_messages msgs = case traverse to_driver_message msgs of
      Nothing    -> pprPanic "non-driver message in preprocess"
                             -- MP: Default config is fine here as it's just in a panic.
                             (vcat $ pprMsgEnvelopeBagWithLoc (defaultDiagnosticOpts @GhcMessage) (getMessages msgs))
      Just msgs' -> msgs'

    to_driver_message = \case
      GhcDriverMessage msg
        -> Just msg
      GhcPsMessage (PsHeaderMessage msg)
        -> Just (DriverPsHeaderMessage (PsHeaderMessage msg))
      _ -> Nothing

    pipe_env = mkPipeEnv StopPreprocess input_fn mb_phase (Temporary TFL_GhcSession)
    mkInputFn  =
      case mb_input_buf of
        Just input_buf -> do
          fn <- newTempName (hsc_logger hsc_env)
                            (hsc_tmpfs hsc_env)
                            (tmpDir (hsc_dflags hsc_env))
                            TFL_CurrentModule
                            ("buf_" ++ src_suffix pipe_env)
          hdl <- openBinaryFile fn WriteMode
          -- Add a LINE pragma so reported source locations will
          -- mention the real input file, not this temp file.
          hPutStrLn hdl $ "{-# LINE 1 \""++ input_fn ++ "\"#-}"
          hPutStringBuffer hdl input_buf
          hClose hdl
          return fn
        Nothing -> return input_fn

-- ---------------------------------------------------------------------------

-- | Compile
--
-- Compile a single module, under the control of the compilation manager.
--
-- This is the interface between the compilation manager and the
-- compiler proper (hsc), where we deal with tedious details like
-- reading the OPTIONS pragma from the source file, converting the
-- C or assembly that GHC produces into an object file, and compiling
-- FFI stub files.
--
-- NB.  No old interface can also mean that the source has changed.


compileOne :: HscEnv
           -> ModSummary      -- ^ summary for module being compiled
           -> Int             -- ^ module N ...
           -> Int             -- ^ ... of M
           -> Maybe ModIface  -- ^ old interface, if we have one
           -> HomeModLinkable  -- ^ old linkable, if we have one
           -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne = compileOne' (Just batchMsg)

compileOne' :: Maybe Messager
            -> HscEnv
            -> ModSummary      -- ^ summary for module being compiled
            -> Int             -- ^ module N ...
            -> Int             -- ^ ... of M
            -> Maybe ModIface  -- ^ old interface, if we have one
            -> HomeModLinkable
            -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne' mHscMessage
            hsc_env0 summary mod_index nmods mb_old_iface mb_old_linkable
 = do

   debugTraceMsg logger 2 (text "compile: input file" <+> text input_fnpp)

   unless (gopt Opt_KeepHiFiles lcl_dflags) $
             addFilesToClean tmpfs TFL_CurrentModule $
                 [ml_hi_file $ ms_location summary]
   unless (gopt Opt_KeepOFiles lcl_dflags) $
             addFilesToClean tmpfs TFL_GhcSession $
                 [ml_obj_file $ ms_location summary]

   -- Initialise plugins here for any plugins enabled locally for a module.
#if defined(HAVE_INTERPRETER)
   plugin_hsc_env <- initializePlugins hsc_env
#else
   let plugin_hsc_env = hsc_env
#endif
   let pipe_env = mkPipeEnv NoStop input_fn Nothing pipelineOutput
   status <- hscRecompStatus mHscMessage plugin_hsc_env upd_summary
                mb_old_iface mb_old_linkable (mod_index, nmods)
   let pipeline = hscPipeline pipe_env (setDumpPrefix pipe_env plugin_hsc_env, upd_summary, status)
   (iface, linkable) <- runPipeline (hsc_hooks plugin_hsc_env) pipeline
   -- See Note [ModDetails and --make mode]
   details <- initModDetails plugin_hsc_env iface
   linkable' <- traverse (initWholeCoreBindings plugin_hsc_env iface details) (homeMod_bytecode linkable)
   return $! HomeModInfo iface details (linkable { homeMod_bytecode = linkable' })

 where lcl_dflags  = ms_hspp_opts summary
       location    = ms_location summary
       input_fn    = expectJust (ml_hs_file location)
       input_fnpp  = ms_hspp_file summary

       pipelineOutput = backendPipelineOutput bcknd

       logger = hsc_logger hsc_env0
       tmpfs  = hsc_tmpfs hsc_env0

       basename = dropExtension input_fn

       -- We add the directory in which the .hs files resides) to the import
       -- path.  This is needed when we try to compile the .hc file later, if it
       -- imports a _stub.h file that we created here.
       current_dir = takeDirectory basename
       old_paths   = includePaths lcl_dflags
       loadAsByteCode
         | Just Target { targetAllowObjCode = obj } <- findTarget summary (hsc_targets hsc_env0)
         , not obj
         = True
         | otherwise = False
       -- Figure out which backend we're using
       (bcknd, dflags3)
         -- #8042: When module was loaded with `*` prefix in ghci, but DynFlags
         -- suggest to generate object code (which may happen in case -fobject-code
         -- was set), force it to generate byte-code. This is NOT transitive and
         -- only applies to direct targets.
         | loadAsByteCode
         = ( interpreterBackend
           , gopt_set (lcl_dflags { backend = interpreterBackend }) Opt_ForceRecomp
           )

         | otherwise
         = (backend dflags, lcl_dflags)
       -- See Note [Filepaths and Multiple Home Units]
       dflags  = dflags3 { includePaths = offsetIncludePaths dflags3 $ addImplicitQuoteInclude old_paths [current_dir] }
       upd_summary = summary { ms_hspp_opts = dflags }
       hsc_env = hscSetFlags dflags hsc_env0


-- ---------------------------------------------------------------------------
-- Link
--
-- Note [Dynamic linking on macOS]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Since macOS Sierra (10.14), the dynamic system linker enforces
-- a limit on the Load Commands.  Specifically the Load Command Size
-- Limit is at 32K (32768).  The Load Commands contain the install
-- name, dependencies, runpaths, and a few other commands.  We however
-- only have control over the install name, dependencies and runpaths.
--
-- The install name is the name by which this library will be
-- referenced.  This is such that we do not need to bake in the full
-- absolute location of the library, and can move the library around.
--
-- The dependency commands contain the install names from of referenced
-- libraries.  Thus if a libraries install name is @rpath/libHS...dylib,
-- that will end up as the dependency.
--
-- Finally we have the runpaths, which informs the linker about the
-- directories to search for the referenced dependencies.
--
-- The system linker can do recursive linking, however using only the
-- direct dependencies conflicts with ghc's ability to inline across
-- packages, and as such would end up with unresolved symbols.
--
-- Thus we will pass the full dependency closure to the linker, and then
-- ask the linker to remove any unused dynamic libraries (-dead_strip_dylibs).
--
-- We still need to add the relevant runpaths, for the dynamic linker to
-- lookup the referenced libraries though.  The linker (ld64) does not
-- have any option to dead strip runpaths; which makes sense as runpaths
-- can be used for dependencies of dependencies as well.
--
-- The solution we then take in GHC is to not pass any runpaths to the
-- linker at link time, but inject them after the linking.  For this to
-- work we'll need to ask the linker to create enough space in the header
-- to add more runpaths after the linking (-headerpad 8000).
--
-- After the library has been linked by $LD (usually ld64), we will use
-- otool to inspect the libraries left over after dead stripping, compute
-- the relevant runpaths, and inject them into the linked product using
-- the install_name_tool command.
--
-- This strategy should produce the smallest possible set of load commands
-- while still retaining some form of relocatability via runpaths.
--
-- The only way I can see to reduce the load command size further would be
-- by shortening the library names, or start putting libraries into the same
-- folders, such that one runpath would be sufficient for multiple/all
-- libraries.
link :: GhcLink                 -- ^ interactive or batch
     -> HscEnv
     -> Bool                    -- ^ attempt linking in batch mode?
     -> Maybe (RecompileRequired -> IO ())
     -> HomePackageTable        -- ^ what to link
     -> IO SuccessFlag

-- For the moment, in the batch linker, we don't bother to tell doLink
-- which packages to link -- it just tries all that are available.
-- batch_attempt_linking should only be *looked at* in batch mode.  It
-- should only be True if the upsweep was successful and someone
-- exports main, i.e., we have good reason to believe that linking
-- will succeed.

link ghcLink hsc_env batch_attempt_linking mHscMessage hpt =
  case linkHook (hsc_hooks hsc_env) of
      Nothing -> case ghcLink of
        NoLink        -> return Succeeded
        LinkExecutable _  -> normal_link
        LinkStaticLib -> normal_link
        LinkDynLib    -> normal_link
        LinkMergedObj -> normal_link
        LinkInMemory
          | platformMisc_ghcWithInterpreter $ platformMisc dflags
           -- Not Linking...(demand linker will do the job)
            -> return Succeeded
          | otherwise
            -> panicBadLink LinkInMemory
      Just h  -> h ghcLink dflags batch_attempt_linking hpt
  where
    dflags = hsc_dflags hsc_env
    normal_link = link' hsc_env batch_attempt_linking mHscMessage hpt


panicBadLink :: GhcLink -> a
panicBadLink other = panic ("link: GHC not built to link this way: " ++
                            show other)

link' :: HscEnv
      -> Bool                    -- ^ attempt linking in batch mode?
      -> Maybe (RecompileRequired -> IO ())
      -> HomePackageTable        -- ^ what to link
      -> IO SuccessFlag

link' hsc_env batch_attempt_linking mHscMessager hpt
   | batch_attempt_linking
   = do
        let
            staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> False

        -- the packages we depend on
        -- TODO: This should be a query on the 'ModuleGraph', since we need to
        -- know which packages are actually needed at the runtime stage.
        pkg_deps <- map snd . Set.toList <$> hptCollectDependencies hpt

        -- the linkables to link
        linkables <- hptCollectObjects hpt

        -- the home modules, for tracing
        home_modules <- hptCollectModules hpt

        debugTraceMsg logger 3 (text "link: hmi ..." $$ vcat (map ppr home_modules))
        debugTraceMsg logger 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))
        debugTraceMsg logger 3 (text "link: pkg deps are ..." $$ vcat (map ppr pkg_deps))

        -- check for the -no-link flag
        if isNoLink (ghcLink dflags)
          then do debugTraceMsg logger 3 (text "link(batch): linking omitted (-c flag given).")
                  return Succeeded
          else do

        let obj_files = concatMap linkableObjs linkables
            platform  = targetPlatform dflags
            arch_os   = platformArchOS platform
            exe_file  = exeFileName arch_os staticLink (outputFile_ dflags)

        linking_needed <- linkingNeeded logger dflags unit_env staticLink linkables pkg_deps

        forM_ mHscMessager $ \hscMessage -> hscMessage linking_needed
        if not (gopt Opt_ForceRecomp dflags) && (linking_needed == UpToDate)
           then do debugTraceMsg logger 2 (text exe_file <+> text "is up to date, linking not required.")
                   return Succeeded
           else do


        -- Don't showPass in Batch mode; doLink will do that for us.
        case ghcLink dflags of
          LinkExecutable blm
#if defined(HAVE_JS_BACKEND)
            | backendUseJSLinker (backend dflags) -> linkJSBinary logger tmpfs fc dflags unit_env obj_files pkg_deps
#endif
            | otherwise -> do
              let opts = initExecutableLinkOpts dflags
              -- See Note [Linking the generic apply code]
              apply_objs <- rtsCmmObjsForLink hsc_env pkg_deps
              linkExecutable logger tmpfs opts unit_env (obj_files ++ apply_objs) pkg_deps
          LinkStaticLib -> do
            apply_objs <- rtsCmmObjsForLink hsc_env pkg_deps
            linkStaticLib logger dflags unit_env (obj_files ++ apply_objs) pkg_deps
          LinkDynLib    -> do
            apply_objs <- rtsCmmObjsForLink hsc_env pkg_deps
            linkDynLibCheck logger tmpfs dflags unit_env (obj_files ++ apply_objs) pkg_deps
          other         -> panicBadLink other

        debugTraceMsg logger 3 (text "link: done")

        -- linkExecutable only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg logger 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded
   where
     logger   = hsc_logger hsc_env
     tmpfs    = hsc_tmpfs hsc_env
     dflags   = hsc_dflags hsc_env
     unit_env = hsc_unit_env hsc_env
#if defined(HAVE_JS_BACKEND)
     fc       = hsc_FC hsc_env
#endif

#if defined(HAVE_JS_BACKEND)
linkJSBinary :: Logger -> TmpFs -> FinderCache -> DynFlags -> UnitEnv -> [FilePath] -> [UnitId] -> IO ()
linkJSBinary logger tmpfs fc dflags unit_env obj_files pkg_deps = do
  -- we use the default configuration for now. In the future we may expose
  -- settings to the user via DynFlags.
  let lc_cfg   = initJSLinkConfig dflags
  let cfg      = initStgToJSConfig dflags
  jsLinkBinary fc lc_cfg cfg logger tmpfs dflags unit_env obj_files pkg_deps
#endif

linkingNeeded :: Logger -> DynFlags -> UnitEnv -> Bool -> [Linkable] -> [UnitId] -> IO RecompileRequired
linkingNeeded logger dflags unit_env staticLink linkables pkg_deps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let platform   = ue_platform unit_env
      unit_state = ue_homeUnitState unit_env
      arch_os    = platformArchOS platform
      exe_file   = exeFileName arch_os staticLink (outputFile_ dflags)
  e_exe_time <- tryIO $ getModificationUTCTime exe_file
  case e_exe_time of
    Left _  -> return $ NeedsRecompile MustCompile
    Right t -> do
        -- first check object files and extra_ld_inputs
        let extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
        (errs,extra_times) <- partitionWithM (tryIO . getModificationUTCTime) extra_ld_inputs
        let obj_times =  map linkableTime linkables ++ extra_times
        if not (null errs) || any (t <) obj_times
            then return $ needsRecompileBecause ObjectsChanged
            else do

        -- next, check libraries. XXX this only checks Haskell libraries,
        -- not extra_libraries or -l things from the command line.
        -- pkg_deps is just the direct dependencies so take the transitive closure here
        -- to decide if we need to relink or not.
        let pkg_hslibs acc uid
              | uid `elementOfUniqDSet` acc = acc
              | Just c <- lookupUnitId unit_state uid =
                  foldl' @[] pkg_hslibs (addOneToUniqDSet acc uid) (unitDepends c)
              | otherwise = acc

            all_pkg_deps = foldl' @[] pkg_hslibs emptyUniqDSet pkg_deps

        let pkg_hslibs  = [ (collectLibraryDirs (ways dflags) [c], lib)
                          | Just c <- map (lookupUnitId unit_state) (uniqDSetToList all_pkg_deps),
                            lib <- unitHsLibs (ghcNameVersion dflags) (ways dflags) c ]

        pkg_libfiles <- mapM (uncurry (findHSLib platform (ways dflags))) pkg_hslibs
        if any isNothing pkg_libfiles then return $ needsRecompileBecause LibraryChanged else do
        (lib_errs,lib_times) <- partitionWithM (tryIO . getModificationUTCTime) (catMaybes pkg_libfiles)
        if not (null lib_errs) || any (t <) lib_times
           then return $ needsRecompileBecause LibraryChanged
           else do
            let opts = initExecutableLinkOpts dflags
            res <- checkLinkInfo logger opts unit_env pkg_deps exe_file
            if res
              then return $ needsRecompileBecause FlagsChanged
              else return UpToDate


findHSLib :: Platform -> Ways -> [String] -> String -> IO (Maybe FilePath)
findHSLib platform ws dirs lib = do
  let batch_lib_file = if ws `hasNotWay` WayDyn
                      then "lib" ++ lib <.> "a"
                      else platformSOName platform lib
  found <- filterM doesFileExist (map (</> batch_lib_file) dirs)
  case found of
    [] -> return Nothing
    (x:_) -> return (Just x)

-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

oneShot :: HscEnv -> StopPhase -> [(String, Maybe Phase)] -> IO ()
oneShot orig_hsc_env stop_phase srcs = do
  -- In oneshot mode, initialise plugins specified on command line
  -- we also initialise in ghc/Main but this might be used as an entry point by API clients who
  -- should initialise their own plugins but may not.
  -- See Note [Timing of plugin initialization]
#if defined(HAVE_INTERPRETER)
  hsc_env <- initializePlugins orig_hsc_env
#else
  let hsc_env = orig_hsc_env
#endif
  o_files <- mapMaybeM (compileFile hsc_env stop_phase) srcs
  case stop_phase of
    StopPreprocess -> return ()
    StopC  -> return ()
    StopAs -> return ()
    NoStop -> doLink hsc_env o_files

compileFile :: HscEnv -> StopPhase -> (FilePath, Maybe Phase) -> IO (Maybe FilePath)
compileFile hsc_env stop_phase (src, mb_phase) = do
   let offset_file = augmentByWorkingDirectory dflags src
       dflags    = hsc_dflags hsc_env
       mb_o_file = outputFile dflags
       ghc_link  = ghcLink dflags      -- Set by -c or -no-link
       notStopPreprocess | StopPreprocess <- stop_phase = False
                         | _              <- stop_phase = True
       -- When linking, the -o argument refers to the linker's output.
       -- otherwise, we use it as the name for the pipeline's output.
       output
        | not (backendGeneratesCode (backend dflags)), notStopPreprocess = NoOutputFile
               -- avoid -E -fno-code undesirable interactions. see #20439
        | NoStop <- stop_phase, not (isNoLink ghc_link) = Persistent
               -- -o foo applies to linker
        | isJust mb_o_file = SpecificFile
               -- -o foo applies to the file we are compiling now
        | otherwise = Persistent
       pipe_env = mkPipeEnv stop_phase offset_file mb_phase output
       pipeline = pipelineStart pipe_env (setDumpPrefix pipe_env hsc_env) offset_file mb_phase

   exists <- doesFileExist offset_file
   when (not exists) $
        throwGhcExceptionIO (CmdLineError ("does not exist: " ++ offset_file))
   runPipeline (hsc_hooks hsc_env) pipeline


doLink :: HscEnv -> [FilePath] -> IO ()
doLink hsc_env o_files = do
  let
    dflags   = hsc_dflags   hsc_env
    logger   = hsc_logger   hsc_env
    unit_env = hsc_unit_env hsc_env
    tmpfs    = hsc_tmpfs    hsc_env
    fc       = hsc_FC       hsc_env

  case ghcLink dflags of
    NoLink        -> return ()
    LinkExecutable blm
#if defined(HAVE_JS_BACKEND)
      | backendUseJSLinker (backend dflags)
                  -> linkJSBinary logger tmpfs fc dflags unit_env o_files []
#endif
      | otherwise -> do
          let opts = initExecutableLinkOpts dflags
          -- See Note [Linking the generic apply code]
          apply_objs <- rtsCmmObjsForLink hsc_env []
          linkExecutable logger tmpfs opts unit_env (o_files ++ apply_objs) []
    LinkStaticLib -> do
          apply_objs <- rtsCmmObjsForLink hsc_env []
          linkStaticLib      logger       dflags unit_env (o_files ++ apply_objs) []
    LinkDynLib    -> do
          apply_objs <- rtsCmmObjsForLink hsc_env []
          linkDynLibCheck    logger tmpfs dflags unit_env (o_files ++ apply_objs) []
    LinkMergedObj
      | Just out <- outputFile dflags
      , let objs = [ f | FileOption _ f <- ldInputs dflags ]
                  -> joinObjectFiles hsc_env (o_files ++ objs) out
      | otherwise -> panic "Output path must be specified for LinkMergedObj"
    other         -> panicBadLink other

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support), and cc files.

-- The _stub.c file is derived from the haskell source file, possibly taking
-- into account the -stubdir option.
--
-- The object file created by compiling the _stub.c file is put into a
-- temporary file, which will be later combined with the main .o file
-- (see the MergeForeign phase).
--
-- Moreover, we also let the user emit arbitrary C/C++/ObjC/ObjC++ files
-- from TH, that are then compiled and linked to the module. This is
-- useful to implement facilities such as inline-c.

compileForeign :: HscEnv -> ForeignSrcLang -> FilePath -> IO FilePath
compileForeign _ RawObject object_file = return object_file
compileForeign hsc_env lang stub_c = do
        let pipeline = case lang of
              LangC      -> viaCPipeline Cc
              LangCxx    -> viaCPipeline Ccxx
              LangObjc   -> viaCPipeline Cobjc
              LangObjcxx -> viaCPipeline Cobjcxx
              LangAsm    -> \pe hsc_env ml fp -> asPipeline True pe hsc_env ml fp
              LangJs     -> \pe hsc_env ml fp -> Just <$> foreignJsPipeline pe hsc_env ml fp
            pipe_env = mkPipeEnv NoStop stub_c Nothing (Temporary TFL_GhcSession)
        res <- runPipeline (hsc_hooks hsc_env) (pipeline pipe_env hsc_env Nothing stub_c)
        case res of
          -- This should never happen as viaCPipeline should only return `Nothing` when the stop phase is `StopC`.
          -- and the same should never happen for asPipeline
          -- Future refactoring to not check StopC for this case
          Nothing -> pprPanic "compileForeign" (text stub_c)
          Just fp -> return fp

{- Note [Linking the generic apply code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The RTS's generic application code (the stg_ap_*_fast entry points, the
stg_ap_* return frames, stg_ap_stk_*, stg_stk_save_* and the ARG_*-indexed
tables; see GHC.StgToCmm.AutoApply) is not part of libHSrts.  The RTS only
*refers* to those symbols.  GHC generates the Cmm from the compiler's own
tables when a program is linked, compiles it, and adds the object to the
link, in the same way the JS backend emits rts.js at link time.  The
references in the RTS are resolved at that final link, like
init_ghc_hs_iface (Note [RTS/ghc-internal interface] in rts/RtsToHsIface.c).

The same goes for the register-saving frames stg_stack_underflow_frame_* and
stg_restore_cccs_* of rts/Jumps.h: they exist once per argument-register
width for the same reason the apply code does, need the same per-file -mavx
flags, and are referred to by the RTS in the same way, so their four
instantiations are generated and compiled here too.  See
Note [Link-time RTS Cmm files] in GHC.Linker.RtsCmm for the list.

The object goes wherever the RTS goes:

  * into every executable, including -no-hs-main ones.  When the RTS is a
    shared library, its undefined stg_ap_* references bind to the
    executable's definitions; the static linker exports them from the
    executable because a shared object on the link line references them,
    so no -rdynamic is needed.  RtsSymbols.c takes their addresses, which
    also keeps them alive under --gc-sections and lets the RTS linker
    resolve them for GHCi.

  * into a shared library only when the library contains the RTS itself:
    -flink-rts, or Windows and wasm where linkDynLib always links the RTS
    (see dynLibLinksRts).  A Haskell shared library that depends on
    libHSrts.so and is loaded by a program that GHC did not link has no
    provider for these symbols; such libraries must be built with -flink-rts,
    as the users guide already recommends for foreign hosts.

  * into a -staticlib archive unless -fno-link-rts.

The generated code follows the RTS flavour being linked: -DPROFILING comes
through wayOptc as for any Cmm file, and we add THREADED_RTS for -threaded
and DEBUG/TICKY_TICKY for -debug, matching the defines Hadrian uses for
those RTS ways.  The V16/V32/V64 vector routines are compiled as separate
objects, see Note [AutoApply.cmm for vectors] in GHC.StgToCmm.AutoApply.
Their instruction-set level is fixed per variant (baseline, -mavx2,
-mavx512f on x86) rather than taken from the link command line: the
baseline objects go into every program and must run on any target CPU,
exactly as when they were part of libHSrts.  Everything else about the
compilation (ways, optimisation level, backend) does follow the link
command line, so that the objects match the program they join.

-fno-link-autoapply disables all this; "ghc --gen-apply" prints the same
Cmm for anyone who needs to link the code by other means.
-}

-- | Generate and compile the RTS Cmm that is linked into programs rather
-- than into libHSrts (the generic apply code and the Jumps.h frames, see
-- Note [Link-time RTS Cmm files] in GHC.Linker.RtsCmm) for the RTS flavour
-- and target of the given session; see Note [Linking the generic apply code].
--
-- The second argument lists the units being linked (in addition to the
-- preload units).  Nothing is generated when the RTS is not among them, for
-- instance when GHC merely links a C program.
mkRtsCmmObjs :: HscEnv -> [UnitId] -> IO [FilePath]
mkRtsCmmObjs hsc_env dep_units
  | not (gopt Opt_LinkAutoApply dflags) = return []
  | otherwise = do
      units <- mayThrowUnitErr (preloadUnitsInfo' (hsc_unit_env hsc_env) dep_units)
      case [ u | u <- units, unitId u == rtsUnitId ] of
        [] -> return []
        rts_unit : _ -> do
          let rts_incs = map ST.unpack (unitIncludeDirs rts_unit)
          dir <- newTempSubDir logger tmpfs (tmpDir dflags)
          mapM (gen dir rts_incs) variants
  where
    dflags   = hsc_dflags hsc_env
    logger   = hsc_logger hsc_env
    tmpfs    = hsc_tmpfs hsc_env
    platform = targetPlatform dflags
    ws       = ways dflags
    x86      = platformArch platform `elem` [ArchX86, ArchX86_64]

    -- See Note [Link-time RTS Cmm files] in GHC.Linker.RtsCmm and
    -- Note [AutoApply.cmm for vectors] in GHC.StgToCmm.AutoApply
    variants = genFiles

    gen :: FilePath -> [FilePath] -> (FilePath, GenFile) -> IO FilePath
    gen dir rts_incs (file_name, gf) = do
      let src    = dir </> file_name
          mb_vec = genFileVecWidth gf
      writeFile src (genFile platform gf)
      addFilesToClean tmpfs TFL_GhcSession [src]
      let pipe_env = mkPipeEnv NoStop src Nothing (Temporary TFL_GhcSession)
          hsc_env' = setDumpPrefix pipe_env (hscUpdateFlags (variant_flags rts_incs mb_vec) hsc_env)
      res <- runPipeline (hsc_hooks hsc_env) (cmmCppPipeline pipe_env hsc_env' src)
      case res of
        Just obj -> return obj
        Nothing  -> pprPanic "mkRtsCmmObjs: no object produced for" (text src)

    variant_flags rts_incs mb_vec d0 =
      let ts = toolSettings d0
          d1 = d0 { ghcLink = NoLink
                    -- Keep the temporary path out of the object.
                  , debugLevel = 0
                  , toolSettings = ts { toolSettings_opt_CmmP = way_defines ++ toolSettings_opt_CmmP ts }
                    -- Cmm.h and the generated RTS headers live in the rts
                    -- unit's include directories, which are not on the
                    -- include path when the rts is not a preload unit
                    -- (-hide-all-packages).
                  , includePaths = addGlobalInclude (includePaths d0) rts_incs
                  }
          d2 = foldl' (flip unSetGeneralFlag') d1
                 [Opt_InfoTableMap, Opt_InfoTableMapWithStack, Opt_InfoTableMapWithFallback]
          -- The vector ISA level is fixed per variant and does not follow
          -- the flags of the link command line (a link with -mavx2 must
          -- not turn the baseline variants, which every program shares,
          -- into AVX code).  So reset the x86 ISA knobs to the target's
          -- baseline first and enable only what each variant needs.
          d3 | x86 = d2 { sseVersion = Nothing, bmiVersion = Nothing
                        , avx = False, avx2 = False
                        , avx512cd = False, avx512er = False
                        , avx512f = False, avx512pf = False
                        , fma = False }
             | otherwise = d2
      in case mb_vec of
           Just 32 | x86 -> d3 { avx2 = True }
           Just 64 | x86 -> d3 { avx512f = True }
           _             -> d3

    -- The defines Hadrian uses for the corresponding RTS ways (see wayCcArgs
    -- and rtsPackageArgs in hadrian); PROFILING is already added by wayOptc.
    -- Ticky counters only exist in the non-threaded debug RTS.
    way_defines = [ "-DTHREADED_RTS" | ws `hasWay` WayThreaded ]
               ++ [ "-DDEBUG"        | ws `hasWay` WayDebug ]
               ++ [ "-DTICKY_TICKY"  | ws `hasWay` WayDebug, not (ws `hasWay` WayThreaded) ]

-- | The generic apply objects to add to the current link, if this kind of
-- link includes the RTS; see Note [Linking the generic apply code].
rtsCmmObjsForLink :: HscEnv -> [UnitId] -> IO [FilePath]
rtsCmmObjsForLink hsc_env dep_units
  | backendUseJSLinker (backend dflags) = return []
  | links_rts                           = mkRtsCmmObjs hsc_env dep_units
  | otherwise                           = return []
  where
    dflags = hsc_dflags hsc_env
    links_rts = case ghcLink dflags of
      LinkExecutable {} -> True
      LinkStaticLib     -> gopt Opt_LinkRts dflags
      LinkDynLib        -> dynLibLinksRts dflags (targetPlatform dflags)
      _                 -> False


compileEmptyStub :: DynFlags -> HscEnv -> FilePath -> ModLocation -> ModuleName -> IO ()
compileEmptyStub dflags hsc_env basename location mod_name = do
  -- To maintain the invariant that every Haskell file
  -- compiles to object code, we make an empty (but
  -- valid) stub object file for signatures.  However,
  -- we make sure this object file has a unique symbol,
  -- so that ranlib on OS X doesn't complain, see
  -- https://gitlab.haskell.org/ghc/ghc/issues/12673
  -- and https://github.com/haskell/cabal/issues/2257
  let logger = hsc_logger hsc_env
  let tmpfs  = hsc_tmpfs hsc_env
  let home_unit = hsc_home_unit hsc_env

  case backendCodeOutput (backend dflags) of
    JSCodeOutput -> do
      empty_stub <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "js"
      let src = ppr (mkHomeModule home_unit mod_name) <+> text "= 0;"
      writeFile empty_stub (showSDoc dflags (pprCode src))
      let pipe_env = (mkPipeEnv NoStop empty_stub Nothing Persistent) { src_basename = basename}
          pipeline = Just <$> foreignJsPipeline pipe_env hsc_env (Just location) empty_stub
      _ <- runPipeline (hsc_hooks hsc_env) pipeline
      pure ()

    _ -> do
      empty_stub <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "c"
      let src = text "int" <+> ppr (mkHomeModule home_unit mod_name) <+> text "= 0;"
      writeFile empty_stub (showSDoc dflags (pprCode src))
      let pipe_env = (mkPipeEnv NoStop empty_stub Nothing Persistent) { src_basename = basename}
          pipeline = viaCPipeline HCc pipe_env hsc_env (Just location) empty_stub
      _ <- runPipeline (hsc_hooks hsc_env) pipeline
      pure ()



{- Environment Initialisation -}

mkPipeEnv :: StopPhase -- End phase
          -> FilePath -- input fn
          -> Maybe Phase
          -> PipelineOutput -- Output
          -> PipeEnv
mkPipeEnv stop_phase  input_fn start_phase output =
  let (basename, suffix) = splitExtension input_fn
      suffix' = drop 1 suffix -- strip off the .
      env = PipeEnv{ stop_phase,
                     src_filename = input_fn,
                     src_basename = basename,
                     src_suffix = suffix',
                     start_phase = fromMaybe (startPhase suffix') start_phase,
                     output_spec = output }
  in env

setDumpPrefix :: PipeEnv -> HscEnv -> HscEnv
setDumpPrefix pipe_env hsc_env =
  hscUpdateFlags (\dflags -> dflags { dumpPrefix = src_basename pipe_env ++ "."}) hsc_env

{- The Pipelines -}

phaseIfFlag :: Monad m
            => HscEnv
            -> (DynFlags -> Bool)
            -> a
            -> m a
            -> m a
phaseIfFlag hsc_env flag def action =
  if flag (hsc_dflags hsc_env)
    then action
    else return def

-- | Check if the start is *before* the current phase, otherwise skip with a default
phaseIfAfter :: P m => Platform -> Phase -> Phase -> a -> m a -> m a
phaseIfAfter platform start_phase cur_phase def action =
  if start_phase `eqPhase` cur_phase
         || happensBefore platform start_phase cur_phase

    then action
    else return def

-- | The preprocessor pipeline
preprocessPipeline :: P m => PipeEnv -> HscEnv -> FilePath -> m (DynFlags, FilePath)
preprocessPipeline pipe_env hsc_env input_fn = do
  unlit_fn <-
    runAfter (Unlit HsSrcFile) input_fn $ do
      use (T_Unlit pipe_env hsc_env input_fn)


  (dflags1, p_warns1, warns1) <- use (T_FileArgs hsc_env unlit_fn)
  let hsc_env1 = hscSetFlags dflags1 hsc_env

  (cpp_fn, hsc_env2)
    <- runAfterFlag hsc_env1 (Cpp HsSrcFile) (xopt LangExt.Cpp) (unlit_fn, hsc_env1) $ do
          cpp_fn <- use (T_Cpp pipe_env hsc_env1 unlit_fn)
          (dflags2, _, _) <- use (T_FileArgs hsc_env1 cpp_fn)
          let hsc_env2 = hscSetFlags dflags2 hsc_env1
          return (cpp_fn, hsc_env2)


  pp_fn <- runAfterFlag hsc_env2 (HsPp HsSrcFile) (gopt Opt_Pp) cpp_fn $
            use (T_HsPp pipe_env hsc_env2 input_fn cpp_fn)

  (dflags3, p_warns3, warns3)
    <- if pp_fn == unlit_fn
          -- Didn't run any preprocessors so don't need to reparse, would be nicer
          -- if `T_FileArgs` recognised this.
          then return (dflags1, p_warns1, warns1)
          else do
            -- Reparse with original hsc_env so that we don't get duplicated options
            use (T_FileArgs hsc_env pp_fn)

  let print_config = initPrintConfig dflags3
  liftIO (printOrThrowDiagnostics (hsc_logger hsc_env) print_config (initDiagOpts dflags3) (GhcPsMessage <$> p_warns3))
  liftIO (printOrThrowDiagnostics (hsc_logger hsc_env) print_config (initDiagOpts dflags3) (GhcDriverMessage <$> warns3))
  return (dflags3, pp_fn)


  -- This won't change through the compilation pipeline
  where platform = targetPlatform (hsc_dflags hsc_env)
        runAfter :: P p => Phase
                  -> a -> p a -> p a
        runAfter = phaseIfAfter platform (start_phase pipe_env)
        runAfterFlag :: P p
                  => HscEnv
                  -> Phase
                  -> (DynFlags -> Bool)
                  -> a
                  -> p a
                  -> p a
        runAfterFlag hsc_env phase flag def action =
          runAfter phase def
           $ phaseIfFlag hsc_env flag def action

-- | The complete compilation pipeline, from start to finish
fullPipeline :: P m => PipeEnv -> HscEnv -> FilePath -> HscSource -> m (ModIface, HomeModLinkable)
fullPipeline pipe_env hsc_env pp_fn src_flavour = do
  (dflags, input_fn) <- preprocessPipeline pipe_env hsc_env pp_fn
  let hsc_env' = hscSetFlags dflags hsc_env
  (hsc_env_with_plugins, mod_sum, hsc_recomp_status)
    <- use (T_HscRecomp pipe_env hsc_env' input_fn src_flavour)
  hscPipeline pipe_env (hsc_env_with_plugins, mod_sum, hsc_recomp_status)

-- | Everything after preprocess
hscPipeline :: P m => PipeEnv ->  ((HscEnv, ModSummary, HscRecompStatus)) -> m (ModIface, HomeModLinkable)
hscPipeline pipe_env (hsc_env_with_plugins, mod_sum, hsc_recomp_status) = do
  case hsc_recomp_status of
    HscUpToDate iface mb_linkable -> return (iface, mb_linkable)
    HscRecompNeeded mb_old_hash -> do
      (tc_result, warnings) <- use (T_Hsc hsc_env_with_plugins mod_sum)
      hscBackendAction <- use (T_HscPostTc hsc_env_with_plugins mod_sum tc_result warnings mb_old_hash )
      hscBackendPipeline pipe_env hsc_env_with_plugins mod_sum hscBackendAction

hscBackendPipeline :: P m => PipeEnv -> HscEnv -> ModSummary -> HscBackendAction -> m (ModIface, HomeModLinkable)
hscBackendPipeline pipe_env hsc_env mod_sum result =
  if backendGeneratesCode (backend (hsc_dflags hsc_env)) then
    do
      res <- hscGenBackendPipeline pipe_env hsc_env mod_sum result
      -- Only run dynamic-too if the backend generates object files
      -- See Note [Writing interface files]
      -- If we are writing a simple interface (not . backendWritesFiles), then
      -- hscMaybeWriteIface in the regular pipeline will write both the hi and
      -- dyn_hi files. This way we can avoid running the pipeline twice and
      -- generating a duplicate linkable.
      -- We must not run the backend a second time with `dynamicNow` enable because
      -- all the work has already been done in the first pipeline.
      when (gopt Opt_BuildDynamicToo (hsc_dflags hsc_env) && backendWritesFiles (backend (hsc_dflags hsc_env)) ) $ do
          let dflags' = setDynamicNow (hsc_dflags hsc_env) -- set "dynamicNow"
          () <$ hscGenBackendPipeline pipe_env (hscSetFlags dflags' hsc_env) mod_sum result
      return res
  else
    case result of
      HscUpdate iface ->  return (iface, emptyHomeModInfoLinkable)
      HscRecomp {} -> (,) <$> liftIO (mkFullIface hsc_env (hscs_partial_iface result) Nothing Nothing NoStubs []) <*> pure emptyHomeModInfoLinkable

hscGenBackendPipeline :: P m
  => PipeEnv
  -> HscEnv
  -> ModSummary
  -> HscBackendAction
  -> m (ModIface, HomeModLinkable)
hscGenBackendPipeline pipe_env hsc_env mod_sum result = do
  let mod_name = moduleName (ms_mod mod_sum)
      src_flavour = (ms_hsc_src mod_sum)
  let location = ms_location mod_sum
  (fos, miface, mlinkable, o_file) <- use (T_HscBackend pipe_env hsc_env mod_name src_flavour location result)
  final_fp <- hscPostBackendPipeline pipe_env hsc_env (ms_hsc_src mod_sum) (backend (hsc_dflags hsc_env)) (Just location) o_file
  final_linkable <-
    case final_fp of
      -- No object file produced, bytecode or NoBackend
      Nothing -> return mlinkable
      Just o_fp -> do
        part_time <- liftIO getCurrentTime
        final_object <- use (T_MergeForeign pipe_env hsc_env o_fp fos)
        let !linkable = Linkable part_time (ms_mod mod_sum) (NE.singleton (DotO final_object ModuleObject))
        -- Add the object linkable to the potential bytecode linkable which was generated in HscBackend.
        return (mlinkable { homeMod_object = Just linkable })

  -- when building ghc-internal with --make (e.g. with cabal-install), we want
  -- the virtual interface for gHC_PRIM in the cache, not the empty one.
  let miface_final
        | ms_mod mod_sum == gHC_PRIM = getGhcPrimIface (hsc_hooks hsc_env)
        | otherwise                  = miface
  return (miface_final, final_linkable)

asPipeline :: P m => Bool -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe ObjFile)
asPipeline use_cpp pipe_env hsc_env location input_fn =
  case stop_phase pipe_env of
    StopAs -> return Nothing
    _ -> Just <$> use (T_As use_cpp pipe_env hsc_env location input_fn)

lasPipeline :: P m => Bool -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe ObjFile)
lasPipeline use_cpp pipe_env hsc_env location input_fn =
  case stop_phase pipe_env of
    StopAs -> return Nothing
    _ -> Just <$> use (T_LlvmAs use_cpp pipe_env hsc_env location input_fn)

viaCPipeline :: P m => Phase -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
viaCPipeline c_phase pipe_env hsc_env location input_fn = do
  out_fn <- use (T_Cc c_phase pipe_env hsc_env location input_fn)
  case stop_phase pipe_env of
    StopC -> return Nothing
    _ -> return $ Just out_fn

llvmPipeline :: P m => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
llvmPipeline pipe_env hsc_env location fp = do
  opt_fn <- use (T_LlvmOpt pipe_env hsc_env fp)
  llvmLlcPipeline pipe_env hsc_env location opt_fn

llvmLlcPipeline :: P m => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
llvmLlcPipeline pipe_env hsc_env location opt_fn = do
  llc_fn <- use (T_LlvmLlc pipe_env hsc_env opt_fn)
  llvmManglePipeline pipe_env hsc_env location llc_fn

llvmManglePipeline :: P m  => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
llvmManglePipeline pipe_env hsc_env location llc_fn = do
  mangled_fn <-
    if gopt Opt_NoLlvmMangler (hsc_dflags hsc_env)
      then return llc_fn
      else use (T_LlvmMangle pipe_env hsc_env llc_fn)
  lasPipeline False pipe_env hsc_env location mangled_fn

cmmCppPipeline :: P m => PipeEnv -> HscEnv -> FilePath -> m (Maybe FilePath)
cmmCppPipeline pipe_env hsc_env input_fn = do
  output_fn <- use (T_CmmCpp pipe_env hsc_env input_fn)
  cmmPipeline pipe_env hsc_env output_fn

cmmPipeline :: P m => PipeEnv -> HscEnv -> FilePath -> m (Maybe FilePath)
cmmPipeline pipe_env hsc_env input_fn = do
  (fos, output_fn) <- use (T_Cmm pipe_env hsc_env input_fn)
  mo_fn <- hscPostBackendPipeline pipe_env hsc_env HsSrcFile (backend (hsc_dflags hsc_env)) Nothing output_fn
  case mo_fn of
    Nothing -> return Nothing
    Just mo_fn -> Just <$> use (T_MergeForeign pipe_env hsc_env mo_fn fos)

jsPipeline :: P m => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m FilePath
jsPipeline pipe_env hsc_env location input_fn = do
  use (T_Js pipe_env hsc_env location input_fn)

foreignJsPipeline :: P m => PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m FilePath
foreignJsPipeline pipe_env hsc_env location input_fn = do
  use (T_ForeignJs pipe_env hsc_env location input_fn)

hscPostBackendPipeline :: P m => PipeEnv -> HscEnv -> HscSource -> Backend -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
hscPostBackendPipeline _ _ (HsBootOrSig _) _ _ _ = return Nothing
hscPostBackendPipeline pipe_env hsc_env HsSrcFile bcknd ml input_fn =
  applyPostHscPipeline (backendPostHscPipeline bcknd) pipe_env hsc_env ml input_fn

applyPostHscPipeline
    :: TPipelineClass TPhase m
    => DefunctionalizedPostHscPipeline
    -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> m (Maybe FilePath)
applyPostHscPipeline NcgPostHscPipeline =
    \pe he ml fp -> asPipeline False pe he ml fp
applyPostHscPipeline ViaCPostHscPipeline = viaCPipeline HCc
applyPostHscPipeline LlvmPostHscPipeline =
    \pe he ml fp -> llvmPipeline pe he ml fp
applyPostHscPipeline JSPostHscPipeline =
    \pe he ml fp -> Just <$> jsPipeline pe he ml fp
applyPostHscPipeline NoPostHscPipeline = \_ _ _ _ -> return Nothing

-- Pipeline from a given suffix
pipelineStart :: P m => PipeEnv -> HscEnv -> FilePath -> Maybe Phase -> m (Maybe FilePath)
pipelineStart pipe_env hsc_env input_fn mb_phase =
  fromPhase (fromMaybe (startPhase $ src_suffix pipe_env)  mb_phase)
  where
   stop_after = stop_phase pipe_env
   frontend :: P m => HscSource -> m (Maybe FilePath)
   frontend sf = case stop_after of
                    StopPreprocess -> do
                      -- The actual output from preprocessing
                      (_, out_fn) <- preprocessPipeline pipe_env hsc_env input_fn
                      let logger = hsc_logger hsc_env
                      -- Sometimes, a compilation phase doesn't actually generate any output
                      -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
                      -- stage, but we wanted to keep the output, then we have to explicitly
                      -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
                      -- further compilation stages can tell what the original filename was.
                      -- File name we expected the output to have
                      final_fn <- liftIO $ phaseOutputFilenameNew (Hsc HsSrcFile) pipe_env hsc_env Nothing
                      when (final_fn /= out_fn) $ do
                        let msg = "Copying `" ++ out_fn ++"' to `" ++ final_fn ++ "'"
                            line_prag = "{-# LINE 1 \"" ++ src_filename pipe_env ++ "\" #-}\n"
                        liftIO (showPass logger msg)
                        liftIO (copyWithHeader line_prag out_fn final_fn)
                      return Nothing
                    _ -> objFromLinkable <$> fullPipeline pipe_env hsc_env input_fn sf
   c :: P m => Phase -> m (Maybe FilePath)
   c phase = viaCPipeline phase pipe_env hsc_env Nothing input_fn
   as :: P m => Bool -> m (Maybe FilePath)
   as use_cpp = asPipeline use_cpp pipe_env hsc_env Nothing input_fn

   objFromLinkable (_, homeMod_object -> Just (Linkable _ _ (DotO lnk _ :| []))) = Just lnk
   objFromLinkable _ = Nothing

   fromPhase :: P m => Phase -> m (Maybe FilePath)
   fromPhase (Unlit p)  = frontend p
   fromPhase (Cpp p)    = frontend p
   fromPhase (HsPp p)   = frontend p
   fromPhase (Hsc p)    = frontend p
   fromPhase HCc        = c HCc
   fromPhase Cc         = c Cc
   fromPhase Ccxx       = c Ccxx
   fromPhase Cobjc      = c Cobjc
   fromPhase Cobjcxx    = c Cobjcxx
   fromPhase (As p)     = as p
   fromPhase LlvmOpt    = llvmPipeline pipe_env hsc_env Nothing input_fn
   fromPhase LlvmLlc    = llvmLlcPipeline pipe_env hsc_env Nothing input_fn
   fromPhase LlvmMangle = llvmManglePipeline pipe_env hsc_env Nothing input_fn
   fromPhase StopLn     = return (Just input_fn)
   fromPhase CmmCpp     = cmmCppPipeline pipe_env hsc_env input_fn
   fromPhase Cmm        = cmmPipeline pipe_env hsc_env input_fn
   fromPhase Js         = Just <$> foreignJsPipeline pipe_env hsc_env Nothing input_fn
   fromPhase MergeForeign = panic "fromPhase: MergeForeign"

{-
Note [The Pipeline Monad]
~~~~~~~~~~~~~~~~~~~~~~~~~
The pipeline is represented as a free monad by the `TPipelineClass` type synonym,
which stipulates the general monadic interface for the pipeline and `MonadUse`, instantiated
to `TPhase`, which indicates the actions available in the pipeline.

The `TPhase` actions correspond to different compiled phases, they are executed by
the 'runPhase' function which interprets each action into IO.

The idea in the future is that we can now implement different instiations of
`TPipelineClass` to give different behaviours that the default `HookedPhase` implementation:

* Additional logging of different phases
* Automatic parallelism (in the style of shake)
* Easy consumption by external tools such as ghcide
* Easier to create your own pipeline and extend existing pipelines.

The structure of the code as a free monad also means that the return type of each
phase is a lot more flexible.

-}
