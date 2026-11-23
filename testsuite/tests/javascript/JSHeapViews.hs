{-# LANGUAGE JavaScriptFFI #-}
module Main (main) where

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CSize(..))

-- Use a real C object and the JS wrapper's explicit heap marshalling. A
-- ccall to strlen would use the JS RTS implementation without Emscripten.
foreign import javascript unsafe "js_heap_views_length_wrapper"
  heapLength :: CString -> IO CSize

foreign import javascript unsafe
  "(() => Module.HEAP8 instanceof Int8Array && Module.HEAPU8 instanceof Uint8Array && Module.HEAP8.buffer === Module.HEAPU8.buffer)"
  heapViews :: IO Bool

main :: IO ()
main = do
  len <- withCString "heap views" heapLength
  print (len == 10)
  heapViews >>= print
