{-# LANGUAGE CPP #-}
#if !defined(__MHS__)
{-# LANGUAGE MagicHash, UnboxedTuples #-}
#endif
{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected
--
-- (c) The University of Glasgow 2002-2006
--
-- Unboxed mutable Ints

module GHC.Data.FastMutInt(
        FastMutInt, newFastMutInt,
        readFastMutInt, writeFastMutInt,
        atomicFetchAddFastMut
  ) where

import GHC.Prelude.Basic

#if defined(__MHS__)
import Data.IORef

-- MicroHs has no MutableByteArray#.  An IORef keeps the same operations.
newtype FastMutInt = FastMutInt (IORef Int)

newFastMutInt :: Int -> IO FastMutInt
newFastMutInt n = fmap FastMutInt (newIORef n)

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt r) = readIORef r

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt r) i = writeIORef r i

-- Returns the value before the addition, matching fetchAddIntArray#.
atomicFetchAddFastMut :: FastMutInt -> Int -> IO Int
atomicFetchAddFastMut (FastMutInt r) i =
  atomicModifyIORef' r (\n -> (n + i, n))
#else
import GHC.Base

data FastMutInt = FastMutInt !(MutableByteArray# RealWorld)

newFastMutInt :: Int -> IO FastMutInt
newFastMutInt n = do
    x <- create
    writeFastMutInt x n
    return x
  where
    !(I# size) = finiteBitSize (0 :: Int) `unsafeShiftR` 3
    create = IO $ \s ->
      case newByteArray# size s of
        (# s, arr #) -> (# s, FastMutInt arr #)

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of
    (# s, i #) -> (# s, I# i #)

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of
    s -> (# s, () #)

atomicFetchAddFastMut :: FastMutInt -> Int -> IO Int
atomicFetchAddFastMut (FastMutInt arr) (I# i) = IO $ \s ->
  case fetchAddIntArray# arr 0# i s of
    (# s, n #) -> (# s, I# n #)
#endif
