-- | Llvm code generator configuration
module GHC.CmmToLlvm.Config
  ( LlvmCgConfig(..)
  , LlvmConfig(..)
  , LlvmTarget(..)
  , initLlvmConfig
  , llvmAddTargetFeatures
  )
where

import GHC.Prelude
import GHC.Platform

import GHC.Utils.Outputable
import GHC.Settings.Utils
import GHC.Utils.Panic
import GHC.CmmToLlvm.Version.Type (LlvmVersion(..))

import Data.List.NonEmpty (NonEmpty(..))
import System.FilePath

data LlvmCgConfig = LlvmCgConfig
  { llvmCgPlatform          :: !Platform     -- ^ Target platform
  , llvmCgContext           :: !SDocContext  -- ^ Context for LLVM code generation
  , llvmCgFillUndefWithGarbage :: !Bool      -- ^ Fill undefined literals with garbage values
  , llvmCgSplitSection      :: !Bool         -- ^ Split sections
  , llvmCgBmiVersion        :: Maybe BmiVersion  -- ^ (x86) BMI instructions
  , llvmCgLlvmVersion       :: Maybe LlvmVersion -- ^ version of Llvm we're using
  , llvmCgDoWarn            :: !Bool         -- ^ True ==> warn unsupported Llvm version
  , llvmCgLlvmTarget        :: !String       -- ^ target triple passed to LLVM
  , llvmCgLlvmConfig        :: !LlvmConfig   -- ^ Supported LLVM configurations.
                                             -- see Note [LLVM configuration]
  , llvmCgTargetFeatures    :: ![String]     -- ^ The module-wide LLVM target
                                             -- features, as passed to llc via
                                             -- -mattr.  A per-procedure
                                             -- "target-features" attribute
                                             -- replaces -mattr for that
                                             -- function, so it has to repeat
                                             -- these.  See
                                             -- Note [Cmm target attributes]
                                             -- in GHC.Cmm.
  }

data LlvmTarget = LlvmTarget
  { lDataLayout :: String
  , lCPU        :: String
  , lAttributes :: [String]
  }

-- | Add requirements to an ordered LLVM feature list. LLVM gives the last
-- occurrence of a feature precedence, so do not remove duplicates: in
-- @+avx,-avx,+avx@ the final requirement must override the disabled default.
--
-- LLVM 18 and later need +evex512 alongside +avx512f on macOS (#26410).
-- Apply the same version-dependent requirement to command-line features and
-- Cmm procedure attributes.
llvmAddTargetFeatures :: Maybe LlvmVersion -> [String] -> [String] -> [String]
llvmAddTargetFeatures version base requested = base ++ concatMap add requested
  where
    add feature = feature : [ "+evex512"
                            | feature == "+avx512f"
                            , maybe False (>= LlvmVersion (18 :| [])) version
                            ]

-- Note [LLVM configuration]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The `llvm-targets` and `llvm-passes` files are shipped with GHC and contain
-- information needed by the LLVM backend to invoke `llc` and `opt`.
-- Specifically:
--
--  * llvm-targets maps autoconf host triples to the corresponding LLVM
--    `data-layout` declarations. This information is extracted from clang using
--    the script in utils/llvm-targets/gen-data-layout.sh and should be updated
--    whenever we target a new version of LLVM.
--
--  * llvm-passes maps GHC optimization levels to sets of LLVM optimization
--    flags that GHC should pass to `opt`.
--
-- This information is contained in files rather the GHC source to allow users
-- to add new targets to GHC without having to recompile the compiler.
--

initLlvmConfig :: FilePath -> IO LlvmConfig
initLlvmConfig top_dir
  = do
      targets <- readAndParse "llvm-targets"
      passes <- readAndParse "llvm-passes"
      return $ LlvmConfig
        { llvmTargets = fmap mkLlvmTarget <$> targets
        , llvmPasses = passes
        }
  where
    readAndParse :: Read a => String -> IO a
    readAndParse name = do
      let f = top_dir </> name
      llvmConfigStr <- readFile f
      case maybeReadFuzzy llvmConfigStr of
        Just s -> return s
        Nothing -> pgmError ("Can't parse LLVM config file: " ++ show f)

    mkLlvmTarget :: (String, String, String) -> LlvmTarget
    mkLlvmTarget (dl, cpu, attrs) = LlvmTarget dl cpu (words attrs)

data LlvmConfig = LlvmConfig
  { llvmTargets :: [(String, LlvmTarget)]
  , llvmPasses  :: [(Int, String)]
  }
