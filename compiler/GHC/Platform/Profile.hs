-- | Platform profiles
module GHC.Platform.Profile
   ( Profile (..)
   , profileBuildTag
   , profileConstants
   , profileIsProfiling
   , profileWordSizeInBytes
   )
where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Ways

-- | A platform profile fully describes the kind of objects that are generated
-- for a platform.
--
-- 'Platform' doesn't fully describe the ABI of an object. The target ABI
-- selects its closure layout; compiler ways additionally select dynamic linking.
--
data Profile = Profile
   { profilePlatform  :: !Platform -- ^ Platform
   , profileWays      :: !Ways     -- ^ Ways
   , profileTargetABI :: !TargetABI -- ^ ABI intrinsic to the target installation
   }
  deriving (Eq, Ord, Show, Read)

-- | Get platform constants
profileConstants :: Profile -> PlatformConstants
{-# INLINE profileConstants #-}
profileConstants profile = platformConstants (profilePlatform profile)

-- | Is profiling enabled
profileIsProfiling :: Profile -> Bool
{-# INLINE profileIsProfiling #-}
profileIsProfiling profile = profileTargetABI profile == ProfilingABI

-- | Word size in bytes
profileWordSizeInBytes :: Profile -> Int
{-# INLINE profileWordSizeInBytes #-}
profileWordSizeInBytes profile = platformWordSizeInBytes (profilePlatform profile)

-- | Unique build tag for the profile
profileBuildTag :: Profile -> String
profileBuildTag profile
    -- profiles using unregisterised convention are not binary compatible with
    -- those that don't. Make sure to make it apparent in the tag so that our
    -- interface files can't be mismatched by mistake.
  | platformUnregisterised platform = 'u':wayTag
  | otherwise                       =     wayTag
  where
   platform = profilePlatform profile
   -- Interface compatibility includes the ABI even though filenames do not.
   wayTag = case profileTargetABI profile of
     DefaultABI -> waysBuildTag (profileWays profile)
     ProfilingABI -> "profiling:" ++ waysBuildTag (profileWays profile)
