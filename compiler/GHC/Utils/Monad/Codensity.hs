{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module GHC.Utils.Monad.Codensity
  ( Codensity(..), toCodensity, fromCodensity )
  where

import Data.Kind ( Type )

import GHC.Prelude
#if defined(__MHS__)
-- ghc-compat's GHC.Exts has no oneShot.  It is only an inlining hint.
oneShot :: a -> a
oneShot x = x
#else
import GHC.Exts ( oneShot )
#endif

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Concurrent.MVar ( newEmptyMVar, readMVar, putMVar )
import Control.Exception
import GHC.IO.Exception
#if defined(__MHS__)
-- MicroHs System.IO exports FixIOException abstractly and ghc-compat's
-- GHC.IO.Exception omits it; provide the local exception constructor here.
data MhsFixIOException = FixIOException
instance Show MhsFixIOException where
  showsPrec _ FixIOException = showString "cyclic evaluation in fixIO"
instance Exception MhsFixIOException
#endif
#if defined(__MHS__)
-- MicroHs has no GHC.IO.Unsafe and no unsafeDupableInterleaveIO;
-- on the single-threaded MicroHs RTS unsafeInterleaveIO is exactly it.
import System.IO.Unsafe ( unsafeInterleaveIO )
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO = unsafeInterleaveIO
#else
import GHC.IO.Unsafe ( unsafeDupableInterleaveIO )
#endif

--------------------------------------------------------------------------------

type Codensity :: (Type -> Type) -> Type -> Type
newtype Codensity m a = Codensity { runCodensity :: forall r. (a -> m r) -> m r }
instance Functor (Codensity k) where
  fmap f (Codensity m) = Codensity $ oneShot (\k -> m $ oneShot (\x -> k $ f x))
  {-# INLINE fmap #-}
instance Applicative (Codensity f) where
  pure x = Codensity $ oneShot (\k -> k x)
  {-# INLINE pure #-}
  Codensity f <*> Codensity g =
    Codensity $ oneShot (\bfr -> f $ oneShot (\ab -> g $ oneShot (\x -> bfr (ab x))))
  {-# INLINE (<*>) #-}
instance Monad (Codensity f) where
  return = pure
  {-# INLINE return #-}
  m >>= k =
    Codensity $ oneShot (\c -> runCodensity m $ oneShot (\a -> runCodensity (k a) c))
  {-# INLINE (>>=) #-}
instance MonadTrans Codensity where
  lift m = Codensity $ oneShot (m >>=)
  {-# INLINE lift #-}
instance MonadIO m => MonadIO (Codensity m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}
instance MonadIO m => MonadFix (Codensity m) where
  mfix f = Codensity $ oneShot $ \ k -> do
    promise <- liftIO $ newEmptyMVar
    ans     <- liftIO $ unsafeDupableInterleaveIO
                      $ readMVar promise
                          `catch`
                        (\ BlockedIndefinitelyOnMVar -> throwIO FixIOException)
    runCodensity (f ans) $ oneShot $ \ a -> do
      liftIO $ putMVar promise a
      k a
  {-# INLINE mfix #-}

toCodensity :: Monad m => m a -> Codensity m a
toCodensity m = Codensity $ oneShot (m >>=)
{-# INLINE toCodensity #-}

fromCodensity :: Monad m => Codensity m a -> m a
fromCodensity c = runCodensity c return
{-# INLINE fromCodensity #-}
