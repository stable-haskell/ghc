{-# LANGUAGE CPP #-}

module GHC.Driver.Config.Interpreter
  ( initInterpOpts
  )
where

#if defined(HAVE_INTERPRETER)

import GHC.Prelude
import GHC.Runtime.Interpreter.Init
import GHC.Driver.DynFlags
import GHC.Driver.Session
import GHC.Driver.Config.Finder
import GHC.Driver.Config.StgToJS
import GHC.SysTools.Tasks
import GHC.Linker.Executable

import System.FilePath
import System.Directory

initInterpOpts :: DynFlags -> IO InterpOpts
initInterpOpts dflags = do
  wasm_dyld <- makeAbsolute $ topDir dflags </> "dyld.mjs"
  js_interp <- makeAbsolute $ topDir dflags </> "ghc-interp.js"
  pure $ InterpOpts
    { interpExternal = gopt Opt_ExternalInterpreter dflags
    , interpProg = pgm_i dflags
    , interpOpts = getOpts dflags opt_i
    , interpWays = ways dflags
    , interpNameVer = ghcNameVersion dflags
    , interpCreateProcess = Nothing
    , interpWasmDyld = wasm_dyld
    , interpBrowser = gopt Opt_GhciBrowser dflags
    , interpBrowserHost = ghciBrowserHost dflags
    , interpBrowserPort = ghciBrowserPort dflags
    , interpBrowserRedirectWasiConsole = gopt Opt_GhciBrowserRedirectWasiConsole dflags
    , interpBrowserPuppeteerLaunchOpts = ghciBrowserPuppeteerLaunchOpts dflags
    , interpBrowserPlaywrightBrowserType = ghciBrowserPlaywrightBrowserType dflags
    , interpBrowserPlaywrightLaunchOpts = ghciBrowserPlaywrightLaunchOpts dflags
    , interpJsInterp = js_interp
    , interpTmpDir = tmpDir dflags
    , interpJsCodegenCfg = initStgToJSConfig dflags
    , interpFinderOpts  = initFinderOpts dflags
    , interpVerbosity = verbosity dflags
    , interpLdConfig = configureLd dflags
    , interpCcConfig = configureCc dflags
    , interpExecutableLinkOpts = initExecutableLinkOpts dflags
    }

#else
-- Stub version when HAVE_INTERPRETER is not defined

import GHC.Prelude
import GHC.Runtime.Interpreter.Init (InterpOpts(..))
import GHC.Driver.Session (DynFlags)

-- | Stub initInterpOpts - returns empty InterpOpts when interpreter is disabled
initInterpOpts :: DynFlags -> IO InterpOpts
initInterpOpts _ = pure InterpOpts

#endif
