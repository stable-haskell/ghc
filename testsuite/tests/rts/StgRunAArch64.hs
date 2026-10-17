{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Foreign.C.Types
import Control.Exception (evaluate)

foreign import ccall safe "stgRunAArch64Probe" probe :: IO CInt
foreign export ccall "stgRunAArch64Callback" callback :: IO CInt

-- Enter STG again from a C call, allocate, and return through StgReturn.
callback :: IO CInt
callback = evaluate (fromIntegral (sum [1 .. 10000 :: Int] `mod` 97))

main :: IO ()
main = probe >>= print
