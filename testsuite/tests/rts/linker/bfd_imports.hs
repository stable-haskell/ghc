{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Monad (unless)
import Foreign.C.String (CString, CWString, withCString, withCWString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, poke)

foreign import ccall "initLinker" initLinker :: IO ()
foreign import ccall "loadArchive" loadArchive :: CWString -> IO CInt
foreign import ccall "loadObj" loadObj :: CWString -> IO CInt
foreign import ccall "resolveObjs" resolveObjs :: IO CInt
foreign import ccall "lookupSymbol" lookupFunction :: CString -> IO (FunPtr (IO CInt))
foreign import ccall "lookupSymbol" lookupData :: CString -> IO (Ptr CInt)
foreign import ccall "dynamic" callFunction :: FunPtr (IO CInt) -> IO CInt

getFunction :: String -> IO (IO CInt)
getFunction name = do
    function <- withCString name lookupFunction
    unless (function /= nullFunPtr) $ error ("lookupSymbol failed: " ++ name)
    pure (callFunction function)

main :: IO ()
main = do
    initLinker
    -- The BFD loader uses the archive's directory to find the adjacent DLL.
    loadedArchive <- withCWString "./libbfd_imports.a" loadArchive
    unless (loadedArchive == 1) $ error "loadArchive failed"
    -- Public lookupSymbol passes NULL for the optional type output.
    nullable <- getFunction "bfd_imports_nullable"
    nullableResult <- nullable
    unless (nullableResult == 123) $ error "lookup with NULL type output failed"
    loadedObject <- withCWString "bfd_imports_client.o" loadObj
    unless (loadedObject == 1) $ error "loadObj failed"
    -- The function and data imports are still unresolved here, so relocation
    -- exercises their types on the first lookup, before they are cached.
    resolved <- resolveObjs
    unless (resolved == 1) $ error "resolveObjs failed"
    entry <- getFunction "bfd_imports_entry"
    result <- entry
    unless (result == 42) $ error ("incorrect import result: " ++ show result)
    address <- withCString "bfd_imports_data" lookupData
    unless (address /= nullPtr) $ error "data lookup failed"
    value <- peek address
    unless (value == 17) $ error "incorrect imported data"
    poke address 19
    changed <- entry
    unless (changed == 44) $ error "imported data does not share storage"
    cached <- getFunction "bfd_imports_function"
    cachedResult <- cached
    unless (cachedResult == 25) $ error "cached import lookup failed"
    putStrLn "OK"
