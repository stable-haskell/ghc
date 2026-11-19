
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
import Data.Char (ord)
-- Installed ghc-compat's GHC.Fingerprint exports only Fingerprint(..)
-- and fingerprint0 (the fuller MicroHs shim is shadowed by the installed
-- package); the rest is defined locally below.
import GHC.Fingerprint (Fingerprint(..), fingerprint0)

instance NFData Fingerprint where
  rnf (Fingerprint a b) = rnf a `seq` rnf b

deriving instance Data Fingerprint
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
-- Deterministic FNV-1a substitutes (cf. GHC.Iface.Tidy.StaticPtrTable).
-- Only determinism within a stage1 run matters: these hashes never leave
-- the build.
fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData ptr len = do
  ws <- mapM (peekElemOff ptr) [0 .. len - 1]
  return (fingerprintBytes ws)

fingerprintString :: String -> Fingerprint
fingerprintString = fingerprintBytes . concatMap enc
  where
    enc c = let w :: Word32
                w = fromIntegral (ord c)
            in [ fromIntegral (w `DBits.shiftR` 24)
               , fromIntegral (w `DBits.shiftR` 16)
               , fromIntegral (w `DBits.shiftR` 8)
               , fromIntegral w
               ]

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints = fingerprintBytes . concatMap fpBytes
  where
    fpBytes (Fingerprint a b) = word64BE a ++ word64BE b
    word64BE w = [ fromIntegral (w `DBits.shiftR` s) | s <- [56, 48, 40, 32, 24, 16, 8, 0] ]

getFileHash :: FilePath -> IO Fingerprint
getFileHash path = do
  bs <- BS.readFile path
  return (fingerprintByteString bs)

fingerprintBytes :: [Word8] -> Fingerprint
fingerprintBytes bs = Fingerprint (fnv 0xcbf29ce484222325) (fnv 0x811c9dc5fb100215)
  where
    fnv :: Word64 -> Word64
    fnv seed = foldl step seed (map fromIntegral bs)
    step h b = (h `DBits.xor` b) * 0x100000001b3
#endif
