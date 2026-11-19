{-
(c) The University of Glasgow, 2000-2006
-}

{-# LANGUAGE CPP #-}
#if !defined(__MHS__)
{-# LANGUAGE MagicHash, UnboxedTuples #-}
#endif

module GHC.Utils.IO.Unsafe
   ( inlinePerformIO,
   )
where

import GHC.Prelude.Basic ()

#if defined(__MHS__)
-- MicroHs has no RealWorld# or unboxed tuples.  unsafePerformIO is the
-- same operation; GHC still inlines the unboxed case below.
import System.IO.Unsafe (unsafePerformIO)
#else
import GHC.Exts
import GHC.IO   (IO(..))
#endif

-- Just like unsafeDupablePerformIO, but we inline it.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__MHS__)
inlinePerformIO = unsafePerformIO
#else
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r
#endif
