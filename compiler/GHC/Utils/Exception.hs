{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

module GHC.Utils.Exception
    (
    module CE,
    module GHC.Utils.Exception
    )
    where

import GHC.Prelude.Basic

#if defined(__MHS__)
-- ghc-compat's GHC.IO has no catchException.  It is GHC's strict catch.
import Control.Exception as CE hiding (assert)
#else
import GHC.IO (catchException)
import Control.Exception as CE hiding (assert)
#endif
import Control.Monad.IO.Class
import Control.Monad.Catch

#if defined(__MHS__)
catchException :: CE.Exception e => IO a -> (e -> IO a) -> IO a
catchException io handler = CE.catch io handler

-- Installed MicroHs base has no AssertionFailed.  The MicroHs source tree
-- does; this bootstrap does not rebuild that package.
data AssertionFailed = AssertionFailed String
instance CE.Exception AssertionFailed
instance Show AssertionFailed where
  showsPrec _ (AssertionFailed err) = showString err
#endif

-- Monomorphised versions of exception-handling utilities
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catchException

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

tryIO :: IO a -> IO (Either IOException a)
tryIO = CE.try

type ExceptionMonad m = (MonadCatch m, MonadThrow m, MonadMask m, MonadIO m)
