-- MicroHs has no GHC.Fingerprint.  This provides the same API as
-- libraries/base/src/GHC/Fingerprint.hs, but computed with a clean,
-- self-contained RFC 1321 MD5 over the exact same bytes GHC hashes
-- (big-endian digest halves, big-endian String bytes, big-endian
-- serialised fingerprints), so stage1 interface and flag fingerprints
-- are real MD5 digests, bit-identical to what GHC computes.
module GHC.Fingerprint
  ( Fingerprint(..)
  , fingerprint0
  , fingerprintData
  , fingerprintString
  , fingerprintFingerprints
  , getFileHash
  ) where

import Prelude

import Data.Bits
import Data.Char (ord)
import Data.Word (Word8, Word32, Word64)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff)
import qualified Data.ByteString as BS

import GHC.Fingerprint.Type

fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData buf len = do
  ws <- mapM (\i -> peekElemOff buf i) [0 .. len - 1]
  let (hi, lo) = md5 ws
  return (Fingerprint hi lo)

fingerprintString :: String -> Fingerprint
fingerprintString str = Fingerprint hi lo
  where
    (hi, lo) = md5 (concatMap enc str)
    enc c = let w32 :: Word32
                w32 = fromIntegral (ord c)
            in [ fromIntegral (w32 `shiftR` 24)
               , fromIntegral (w32 `shiftR` 16)
               , fromIntegral (w32 `shiftR` 8)
               , fromIntegral w32
               ]

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints fs = Fingerprint hi lo
  where
    (hi, lo) = md5 (concatMap fpBytes fs)
    fpBytes (Fingerprint a b) = word64BE a ++ word64BE b
    word64BE w = [ fromIntegral (w `shiftR` s) | s <- [56, 48, 40, 32, 24, 16, 8, 0] ]

-- | Computes the hash of a given file.
getFileHash :: FilePath -> IO Fingerprint
getFileHash path = do
  bs <- BS.readFile path
  let (hi, lo) = md5 (BS.unpack bs)
  return (Fingerprint hi lo)

-- ---------------------------------------------------------------------------
-- Pure RFC 1321 MD5.  Returns the digest as two big-endian Word64 halves,
-- matching the Storable Fingerprint instance (fixed big-endian 128-bit).
-- ---------------------------------------------------------------------------

md5 :: [Word8] -> (Word64, Word64)
md5 msg = (be64 (take 8 d), be64 (drop 8 d))
  where
    d = finalise (processBlocks msg)
    be64 bytes = foldl (\a b -> (a `shiftL` 8) .|. fromIntegral b) 0 bytes

processBlocks :: [Word8] -> (Word32, Word32, Word32, Word32)
processBlocks msg = foldl process (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476) (blocks msg)

finalise :: (Word32, Word32, Word32, Word32) -> [Word8]
finalise (a, b, c, d) = concatMap word32LE [a, b, c, d]
  where
    word32LE w = [ fromIntegral w
                 , fromIntegral (w `shiftR` 8)
                 , fromIntegral (w `shiftR` 16)
                 , fromIntegral (w `shiftR` 24)
                 ]

blocks :: [Word8] -> [[Word32]]
blocks msg = map blockWords (chunksOf 64 (pad msg))
  where
    -- Word i holds bytes [4i .. 4i+3], little-endian.
    blockWords blk = [ word32LEAt blk (4 * i) | i <- [0 .. 15] ]
    word32LEAt blk o =
      fromIntegral (blk !! o) .|.
      (fromIntegral (blk !! (o + 1)) `shiftL` 8) .|.
      (fromIntegral (blk !! (o + 2)) `shiftL` 16) .|.
      (fromIntegral (blk !! (o + 3)) `shiftL` 24)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

pad :: [Word8] -> [Word8]
pad msg = with1 ++ replicate padLen 0 ++ lenBytes
  where
    with1 = msg ++ [0x80]
    padLen = (56 - (length with1 `mod` 64)) `mod` 64
    bitLen :: Word64
    bitLen = fromIntegral (length msg) * 8
    lenBytes = [ fromIntegral (bitLen `shiftR` s) :: Word8 | s <- [0, 8, 16, 24, 32, 40, 48, 56] ]

process :: (Word32, Word32, Word32, Word32) -> [Word32] -> (Word32, Word32, Word32, Word32)
process (a0, b0, c0, d0) m = (a0 + a, b0 + b, c0 + c, d0 + d)
  where
    (a, b, c, d) = foldl step (a0, b0, c0, d0) [0 .. 63]
    step (a, b, c, d) i = (d, b + rol (a + f + kTable !! i + m !! g) (sTable !! i), b, c)
      where
        (f, g)
          | i < 16 = ((b .&. c) .|. (complement b .&. d), i)
          | i < 32 = ((d .&. b) .|. (complement d .&. c), (5 * i + 1) `mod` 16)
          | i < 48 = (b `xor` c `xor` d, (3 * i + 5) `mod` 16)
          | otherwise = (c `xor` (b .|. complement d), (7 * i) `mod` 16)

rol :: Word32 -> Int -> Word32
rol x n = (x `shiftL` n) .|. (x `shiftR` (32 - n))

sTable :: [Int]
sTable =
  [ 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22
  , 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20
  , 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23
  , 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
  ]

kTable :: [Word32]
kTable =
  [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
  , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
  , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
  , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
  , 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
  , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
  , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
  , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
  , 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
  , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
  , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
  , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
  , 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
  , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
  , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
  , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
  ]
