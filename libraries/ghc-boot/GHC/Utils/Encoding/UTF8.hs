{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, MultiWayIf #-}
{-# OPTIONS_GHC -O2 -fno-warn-name-shadowing #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected. This module used to live in the `ghc`
-- package but has been moved to `ghc-boot` because the definition
-- of the package database (needed in both ghc and in ghc-pkg) lives in
-- `ghc-boot` and uses ShortText, which in turn depends on this module.

-- | Simple, non-streaming Modified UTF-8 codecs.
--
-- This is one of several UTF-8 implementations provided by GHC; see Note
-- [GHC's many UTF-8 implementations] in "GHC.Encoding.UTF8" for an
-- overview.
--
module GHC.Utils.Encoding.UTF8
    (
#if !defined(__MHS__)
      -- * Decoding single characters
      utf8DecodeCharAddr#
    , utf8DecodeCharPtr
    , utf8DecodeCharByteArray#
    , utf8PrevChar
    , utf8CharStart
    , utf8UnconsByteString
      -- * Decoding strings
    , utf8DecodeByteString
    , utf8DecodeShortByteString
    , utf8DecodeForeignPtr
    , utf8DecodeByteArray#
      -- * Counting characters
    , utf8CountCharsShortByteString
    , utf8CountCharsByteArray#
      -- * Comparison
    , utf8CompareByteArray#
    , utf8CompareShortByteString
      -- * Encoding strings
    , utf8EncodeByteArray#
    , utf8EncodePtr
    , utf8EncodeByteString
    , utf8EncodeShortByteString
    , utf8EncodedLength
#else
      -- MicroHs has no unboxed byte arrays.  The boxed entry points are
      -- the ones ghc-boot and the compiler's FastString path call.
      utf8DecodeCharPtr
    , utf8PrevChar
    , utf8CharStart
    , utf8UnconsByteString
    , utf8DecodeByteString
    , utf8DecodeShortByteString
    , utf8DecodeForeignPtr
    , utf8CountCharsShortByteString
    , utf8CompareShortByteString
    , utf8EncodePtr
    , utf8EncodeByteString
    , utf8EncodeShortByteString
    , utf8EncodedLength
#endif
    ) where


import Prelude

import Foreign
#if !defined(__MHS__)
import GHC.IO
import GHC.Encoding.UTF8

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.ByteString.Short.Internal (ShortByteString(..))
#else
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Char (chr, ord)
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)
#endif

#if defined(__MHS__)
-- Standard UTF-8.  GHC's own codec is Modified UTF-8 (NUL is C0 80).
-- Bootstrap sources do not contain NUL, so the two agree on the strings
-- stage1 reads and writes.

encodeChar :: Char -> [Word8]
encodeChar c
  | n <= 0x7F  = [fromIntegral n]
  | n <= 0x7FF = [b 0xC0 6, cont 0]
  | n <= 0xFFFF = [b 0xE0 12, cont 6, cont 0]
  | otherwise  = [b 0xF0 18, cont 12, cont 6, cont 0]
  where
    n = ord c
    b mark sh = fromIntegral (mark + (n `shiftR` sh))
    cont sh = fromIntegral (0x80 + ((n `shiftR` sh) .&. 0x3F))

utf8EncodeByteString :: String -> ByteString
utf8EncodeByteString = BS.pack . concatMap encodeChar

utf8EncodeShortByteString :: String -> ShortByteString
utf8EncodeShortByteString = toShort . utf8EncodeByteString

utf8EncodedLength :: String -> Int
utf8EncodedLength = BS.length . utf8EncodeByteString

utf8DecodeByteString :: ByteString -> String
utf8DecodeByteString = go . BS.unpack
  where
    go [] = []
    go (w:ws)
      | w < 0x80 = chr (fromIntegral w) : go ws
      | w < 0xE0 = one 0x1F 1 w ws
      | w < 0xF0 = one 0x0F 2 w ws
      | otherwise = one 0x07 3 w ws
    one mask n w ws =
      let (cs, rest) = splitAt n ws
          acc = foldl (\a x -> (a `shiftL` 6) + fromIntegral (x .&. 0x3F))
                      (fromIntegral (w .&. mask)) cs
      in if length cs < n then '\xfffd' : go rest
         else chr acc : go rest

utf8DecodeShortByteString :: ShortByteString -> String
utf8DecodeShortByteString = utf8DecodeByteString . fromShort

utf8CountCharsShortByteString :: ShortByteString -> Int
utf8CountCharsShortByteString = length . utf8DecodeShortByteString

utf8CompareShortByteString :: ShortByteString -> ShortByteString -> Ordering
utf8CompareShortByteString a b = compare (BS.unpack (fromShort a)) (BS.unpack (fromShort b))

utf8UnconsByteString :: ByteString -> Maybe (Char, ByteString)
utf8UnconsByteString bs
  | BS.null bs = Nothing
  | otherwise =
      let c = head (utf8DecodeByteString (BS.take 4 bs))
          n = length (encodeChar c)
      in Just (c, BS.drop n bs)

utf8EncodePtr :: Ptr Word8 -> String -> IO ()
utf8EncodePtr p s = pokeBytes p (BS.unpack (utf8EncodeByteString s))
  where
    pokeBytes _ [] = return ()
    pokeBytes q (w:ws) = poke q w >> pokeBytes (q `plusPtr` 1) ws

utf8DecodeCharPtr :: Ptr Word8 -> (Char, Int)
utf8DecodeCharPtr p = unsafePerformIO $ do
  bytes <- peekBytes 4 p
  let c = head (utf8DecodeByteString (BS.pack bytes))
  return (c, length (encodeChar c))
  where
    peekBytes 0 _ = return []
    peekBytes n q = do
      w <- peek q
      ws <- peekBytes (n - 1) (q `plusPtr` 1)
      return (w:ws)

utf8DecodeForeignPtr :: ForeignPtr Word8 -> Int -> Int -> IO String
utf8DecodeForeignPtr fptr offset len =
  withForeignPtr fptr $ \ptr -> do
    bytes <- peekN len (ptr `plusPtr` offset)
    return (utf8DecodeByteString (BS.pack bytes))
  where
    peekN 0 _ = return []
    peekN n q = do
      w <- peek q
      ws <- peekN (n - 1) (q `plusPtr` 1)
      return (w:ws)

utf8PrevChar :: Ptr Word8 -> IO (Ptr Word8)
utf8PrevChar p = utf8CharStart (p `plusPtr` (-1))

utf8CharStart :: Ptr Word8 -> IO (Ptr Word8)
utf8CharStart p = go p
  where
    go q = do
      w <- peek q
      if w >= 0x80 && w < 0xC0 then go (q `plusPtr` (-1)) else return q

#else
-- | Find the start of the codepoint preceding the codepoint at the given
-- 'Ptr'. This is undefined if there is no previous valid codepoint.
utf8PrevChar :: Ptr Word8 -> IO (Ptr Word8)
utf8PrevChar p = utf8CharStart (p `plusPtr` (-1))

-- | Find the start of the codepoint at the given 'Ptr'. This is undefined if
-- there is no previous valid codepoint.
utf8CharStart :: Ptr Word8 -> IO (Ptr Word8)
utf8CharStart p = go p
 where go p = do w <- peek p
                 if w >= 0x80 && w < 0xC0
                        then go (p `plusPtr` (-1))
                        else return p

utf8CountCharsShortByteString :: ShortByteString -> Int
utf8CountCharsShortByteString (SBS ba) = utf8CountCharsByteArray# ba

utf8DecodeShortByteString :: ShortByteString -> [Char]
utf8DecodeShortByteString (SBS ba#) = utf8DecodeByteArray# ba#

-- | Decode a 'ByteString' containing a UTF-8 string.
utf8DecodeByteString :: ByteString -> [Char]
utf8DecodeByteString (BS.PS fptr offset len)
  = utf8DecodeForeignPtr fptr offset len

utf8EncodeShortByteString :: String -> ShortByteString
utf8EncodeShortByteString str = SBS (utf8EncodeByteArray# str)

-- | Encode a 'String' into a 'ByteString'.
utf8EncodeByteString :: String -> ByteString
utf8EncodeByteString s =
  unsafePerformIO $ do
    let len = utf8EncodedLength s
    buf <- mallocForeignPtrBytes len
    withForeignPtr buf $ \ptr -> do
      utf8EncodePtr ptr s
      pure (BS.fromForeignPtr buf 0 len)

utf8UnconsByteString :: ByteString -> Maybe (Char, ByteString)
utf8UnconsByteString (BS.PS _ _ 0) = Nothing
utf8UnconsByteString (BS.PS fptr offset len)
  = unsafeDupablePerformIO $
      withForeignPtr fptr $ \ptr -> do
        let (c,n) = utf8DecodeCharPtr (ptr `plusPtr` offset)
        return $ Just (c, BS.PS fptr (offset + n) (len - n))

utf8CompareShortByteString :: ShortByteString -> ShortByteString -> Ordering
utf8CompareShortByteString (SBS a1) (SBS a2) = utf8CompareByteArray# a1 a2
#endif
