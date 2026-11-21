-- MicroHs has no directory-ospath package, so no
-- System.Directory.Internal.  compiler/GHC/Data/OsPath.hs imports 'os'
-- from here and re-exports it.  Under MicroHs, OsPath is filepath's
-- type, so 'os' is just 'unsafeEncodeUtf'.
module System.Directory.Internal (os) where

import Prelude

import System.OsPath (OsPath, unsafeEncodeUtf)

-- | Build an 'OsPath' from a 'String' literal fragment.
os :: String -> OsPath
os = unsafeEncodeUtf
