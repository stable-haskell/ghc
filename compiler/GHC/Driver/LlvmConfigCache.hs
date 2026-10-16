{-# LANGUAGE CPP #-}

-- | LLVM config cache
module GHC.Driver.LlvmConfigCache
  ( LlvmConfigCache
  , initLlvmConfigCache
#if defined(HAVE_LLVM_BACKEND)
  , readLlvmConfigCache
#endif
  )
where

import GHC.Prelude

#if defined(HAVE_LLVM_BACKEND)
import GHC.CmmToLlvm.Config
import System.IO.Unsafe

-- | Cache LLVM configuration read from files in top_dir
--
-- See Note [LLVM configuration] in GHC.CmmToLlvm.Config
--
-- Currently implemented with unsafe lazy IO. But it could be implemented with
-- an IORef as the exposed interface is in IO.
data LlvmConfigCache = LlvmConfigCache LlvmConfig

initLlvmConfigCache :: FilePath -> IO LlvmConfigCache
initLlvmConfigCache top_dir = pure $ LlvmConfigCache (unsafePerformIO $ initLlvmConfig top_dir)

readLlvmConfigCache :: LlvmConfigCache -> IO LlvmConfig
readLlvmConfigCache (LlvmConfigCache !config) = pure config

#else

-- | Stub when LLVM backend is not available. The type must exist because
-- it is a field in HscEnv (GHC.Driver.Env.Types).
data LlvmConfigCache = LlvmConfigCache

initLlvmConfigCache :: FilePath -> IO LlvmConfigCache
initLlvmConfigCache _ = pure LlvmConfigCache

#endif
