module Data.ByteString.Internal
  ( ByteString
  , create
  , fromForeignPtr
  , memcpy
  ) where

import Prelude

import Data.ByteString (ByteString, packCStringLen)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import System.IO.Unsafe (unsafePerformIO)

create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
create len fill =
  allocaBytes len $ \p -> do
    fill p
    packCStringLen (castPtr p, len)

fromForeignPtr :: ForeignPtr Word8 -> Int -> Int -> ByteString
fromForeignPtr fp off len = unsafePerformIO $
  allocaBytes len $ \p -> do
    withForeignPtr fp $ \q -> copyBytes p (q `plusPtr` off) len
    packCStringLen (castPtr p, len)

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy = copyBytes
