module GHC.ForeignPtr
  ( module Foreign.ForeignPtr
  , unsafeWithForeignPtr
  ) where
import Prelude
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr)

unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
