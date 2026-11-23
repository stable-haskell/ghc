{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Monad (unless)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (FunPtr, nullFunPtr)

foreign import ccall "initLinker" initLinker :: IO ()
foreign import ccall "loadObj" loadObj :: CString -> IO CInt
foreign import ccall "resolveObjs" resolveObjs :: IO CInt
foreign import ccall "lookupSymbol" lookupSymbol :: CString -> IO (FunPtr (IO CInt))
foreign import ccall "dynamic" callEntry :: FunPtr (IO CInt) -> IO CInt

main :: IO ()
main = do
    initLinker
    loaded <- withCString "elf_local_got_fixture.o" loadObj
    unless (loaded == 1) $ error "loadObj failed"
    resolved <- resolveObjs
    unless (resolved == 1) $ error "resolveObjs failed"
    entry <- withCString "elf_local_got_entry" lookupSymbol
    unless (entry /= nullFunPtr) $ error "lookupSymbol failed"
    result <- callEntry entry
    unless (result == 47) $ error ("incorrect local GOT result: " ++ show result)
    putStrLn "OK"
