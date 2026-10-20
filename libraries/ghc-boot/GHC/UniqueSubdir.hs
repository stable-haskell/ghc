module GHC.UniqueSubdir
  ( uniqueSubdir
  , uniqueSubdirWithAbi
  ) where

import Prelude -- See Note [Why do we import Prelude here?]

import Data.List (intercalate)

import GHC.Platform.ArchOS
import GHC.Version (cProjectVersion)

-- | A filepath like @x86_64-linux-7.6.3@ with the platform string to use when
-- constructing platform-version-dependent files that need to co-exist.
uniqueSubdir :: ArchOS -> FilePath
uniqueSubdir (ArchOS arch os) = intercalate "-"
  [ stringEncodeArch arch
  , stringEncodeOS os
  , cProjectVersion
  ]
  -- NB: This functionality is reimplemented in Cabal, so if you
  -- change it, be sure to update Cabal.
  -- TODO make Cabal use this now that it is in ghc-boot.

-- | Keep same-architecture target ABIs out of each other's user databases and
-- package environments. The empty ABI preserves existing native paths.
uniqueSubdirWithAbi :: ArchOS -> String -> FilePath
uniqueSubdirWithAbi platform abi = uniqueSubdir platform ++ case abi of
  "" -> ""
  _ -> "-" ++ abi
