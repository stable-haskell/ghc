
{-# LANGUAGE CPP #-}
#if defined(__MHS__)
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- ----------------------------------------------------------------------------
--
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning.
--
-- https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance
--
-- ----------------------------------------------------------------------------

module GHC.Utils.Fingerprint (
        readHexFingerprint,
        fingerprintByteString,
        -- * Re-exported from GHC.Fingerprint
        Fingerprint(..), fingerprint0,
        fingerprintFingerprints,
        fingerprintData,
        fingerprintString,
        fingerprintStrings,
        getFileHash
   ) where

import GHC.Prelude.Basic

import Foreign
#if defined(__MHS__)
-- ghc-compat's GHC.IO does not export unsafeDupablePerformIO.
-- System.IO.Unsafe does.
import System.IO.Unsafe (unsafeDupablePerformIO)
#else
import GHC.IO
#endif
import Numeric          ( readHex )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

#if defined(__MHS__)
import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import qualified Data.Bits as DBits
import Data.Bits ((.|.))
import Data.Char (ord)
import Foreign.C.Types (CSize(..))
-- Installed ghc-compat's GHC.Fingerprint exports only Fingerprint(..)
-- and fingerprint0 (the fuller MicroHs shim is shadowed by the installed
-- package); the rest is defined locally below.
import GHC.Fingerprint (Fingerprint(..), fingerprint0)

instance NFData Fingerprint where
  rnf (Fingerprint a b) = rnf a `seq` rnf b

deriving instance Data Fingerprint

-- MicroHs runtime MD5 (src/runtime/md5.c, registered as "md5Array" in
-- eval.c's ffi_table). Parameter order: input, 16-byte result, length.
foreign import ccall "md5Array" c_md5Array :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
#else
import GHC.Fingerprint
#endif

-- useful for parsing the output of 'md5sum', should we want to do that.
readHexFingerprint :: String -> Fingerprint
readHexFingerprint s = Fingerprint w1 w2
 where (s1,s2) = splitAt 16 s
       [(w1,"")] = readHex s1
       [(w2,"")] = readHex (take 16 s2)

fingerprintByteString :: BS.ByteString -> Fingerprint
fingerprintByteString bs = unsafeDupablePerformIO $
  BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> fingerprintData (castPtr ptr) len

-- See Note [Repeated -optP hashing]
fingerprintStrings :: [String] -> Fingerprint
fingerprintStrings ss = fingerprintFingerprints $ map fingerprintString ss

#if defined(__MHS__)
-- Real MD5 digests via the MicroHs runtime's md5Array, decoded in the
-- fixed big-endian 128-bit format of Storable Fingerprint (bytes[0..7]
-- -> high, bytes[8..15] -> low), bit-identical to what GHC computes.
fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData ptr len =
  allocaBytes 16 $ \pdigest -> do
    -- Never pass a null input pointer: hash the digest buffer itself
    -- with length 0 when there is nothing to hash.
    let (pin, n) | len <= 0  = (pdigest, 0)
                 | otherwise = (ptr, len)
    c_md5Array pin pdigest (fromIntegral n)
    hi <- peekBE64 pdigest
    lo <- peekBE64 (pdigest `plusPtr` 8)
    return (Fingerprint hi lo)
  where
    peekBE64 :: Ptr Word8 -> IO Word64
    peekBE64 p = do
      b0 <- peekElemOff p 0
      b1 <- peekElemOff p 1
      b2 <- peekElemOff p 2
      b3 <- peekElemOff p 3
      b4 <- peekElemOff p 4
      b5 <- peekElemOff p 5
      b6 <- peekElemOff p 6
      b7 <- peekElemOff p 7
      return (fromIntegral b0 `DBits.shiftL` 56 .|.
              fromIntegral b1 `DBits.shiftL` 48 .|.
              fromIntegral b2 `DBits.shiftL` 40 .|.
              fromIntegral b3 `DBits.shiftL` 32 .|.
              fromIntegral b4 `DBits.shiftL` 24 .|.
              fromIntegral b5 `DBits.shiftL` 16 .|.
              fromIntegral b6 `DBits.shiftL` 8 .|.
              fromIntegral b7)

fingerprintString :: String -> Fingerprint
fingerprintString str = unsafeDupablePerformIO $ do
  let n = length str
  allocaBytes (max 1 (4 * n)) $ \pbuf -> do
    pokeChars pbuf str 0
    fingerprintData pbuf (4 * n)
  where
    pokeChars :: Ptr Word8 -> String -> Int -> IO ()
    pokeChars _ [] _ = return ()
    pokeChars p (c : cs) o = do
      let w :: Word32
          w = fromIntegral (ord c)
      pokeElemOff p o     (fromIntegral (w `DBits.shiftR` 24) :: Word8)
      pokeElemOff p (o+1) (fromIntegral (w `DBits.shiftR` 16) :: Word8)
      pokeElemOff p (o+2) (fromIntegral (w `DBits.shiftR` 8) :: Word8)
      pokeElemOff p (o+3) (fromIntegral w :: Word8)
      pokeChars p cs (o + 4)

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints fs = unsafeDupablePerformIO $ do
  let n = length fs
  allocaBytes (max 1 (16 * n)) $ \pbuf -> do
    pokeFPs pbuf fs 0
    fingerprintData pbuf (16 * n)
  where
    pokeFPs :: Ptr Word8 -> [Fingerprint] -> Int -> IO ()
    pokeFPs _ [] _ = return ()
    pokeFPs p (Fingerprint a b : rest) o = do
      pokeBE64 p o a
      pokeBE64 p (o + 8) b
      pokeFPs p rest (o + 16)
    pokeBE64 :: Ptr Word8 -> Int -> Word64 -> IO ()
    pokeBE64 p o w = do
      pokeElemOff p o     (fromIntegral (w `DBits.shiftR` 56) :: Word8)
      pokeElemOff p (o+1) (fromIntegral (w `DBits.shiftR` 48) :: Word8)
      pokeElemOff p (o+2) (fromIntegral (w `DBits.shiftR` 40) :: Word8)
      pokeElemOff p (o+3) (fromIntegral (w `DBits.shiftR` 32) :: Word8)
      pokeElemOff p (o+4) (fromIntegral (w `DBits.shiftR` 24) :: Word8)
      pokeElemOff p (o+5) (fromIntegral (w `DBits.shiftR` 16) :: Word8)
      pokeElemOff p (o+6) (fromIntegral (w `DBits.shiftR` 8) :: Word8)
      pokeElemOff p (o+7) (fromIntegral w :: Word8)

getFileHash :: FilePath -> IO Fingerprint
getFileHash path = do
  bs <- BS.readFile path
  return $! fingerprintByteString bs
#endif
