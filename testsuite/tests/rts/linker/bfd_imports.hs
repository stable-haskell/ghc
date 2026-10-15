{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Monad (unless)
import Foreign.C.String (CString, CWString, withCString, withCWString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, poke)

checkpoint :: String -> IO ()
checkpoint message = appendFile "bfd-trace.txt" (message ++ "\n")

foreign import ccall "initLinker" initLinker :: IO ()
foreign import ccall "loadArchive" loadArchive :: CWString -> IO CInt
foreign import ccall "loadObj" loadObj :: CWString -> IO CInt
foreign import ccall "resolveObjs" resolveObjs :: IO CInt
foreign import ccall "lookupSymbol" lookupFunction :: CString -> IO (FunPtr (IO CInt))
foreign import ccall "lookupSymbol" lookupData :: CString -> IO (Ptr CInt)
foreign import ccall "dynamic" callFunction :: FunPtr (IO CInt) -> IO CInt

getFunction :: String -> IO (IO CInt)
getFunction name = do
    checkpoint ("lookupFunction before: " ++ name)
    function <- withCString name lookupFunction
    checkpoint ("lookupFunction after: " ++ name ++ " " ++ show function)
    unless (function /= nullFunPtr) $ error ("lookupSymbol failed: " ++ name)
    pure (callFunction function)

main :: IO ()
main = do
    checkpoint "initLinker before"
    initLinker
    checkpoint "loadArchive before"
    loadedArchive <- withCWString "libbfd_imports.a" loadArchive
    checkpoint ("loadArchive after: " ++ show loadedArchive)
    unless (loadedArchive == 1) $ error "loadArchive failed"
    -- Public lookupSymbol passes NULL for the optional type output.
    nullable <- getFunction "bfd_imports_nullable"
    checkpoint "nullable call before"
    nullableResult <- nullable
    checkpoint ("nullable call after: " ++ show nullableResult)
    unless (nullableResult == 123) $ error "lookup with NULL type output failed"
    loadedObject <- withCWString "bfd_imports_client.o" loadObj
    checkpoint ("loadObj after: " ++ show loadedObject)
    unless (loadedObject == 1) $ error "loadObj failed"
    -- The function and data imports are still unresolved here, so relocation
    -- exercises their types on the first lookup, before they are cached.
    checkpoint "resolveObjs before"
    resolved <- resolveObjs
    checkpoint ("resolveObjs after: " ++ show resolved)
    unless (resolved == 1) $ error "resolveObjs failed"
    entry <- getFunction "bfd_imports_entry"
    checkpoint "entry call before"
    result <- entry
    checkpoint ("entry call after: " ++ show result)
    unless (result == 42) $ error ("incorrect import result: " ++ show result)
    address <- withCString "bfd_imports_data" lookupData
    checkpoint ("lookupData after: " ++ show address)
    unless (address /= nullPtr) $ error "data lookup failed"
    value <- peek address
    checkpoint ("peek data after: " ++ show value)
    unless (value == 17) $ error "incorrect imported data"
    checkpoint "poke data before"
    poke address 19
    checkpoint "changed entry call before"
    changed <- entry
    checkpoint ("changed entry call after: " ++ show changed)
    unless (changed == 44) $ error "imported data does not share storage"
    cached <- getFunction "bfd_imports_function"
    checkpoint "cached call before"
    cachedResult <- cached
    checkpoint ("cached call after: " ++ show cachedResult)
    unless (cachedResult == 25) $ error "cached import lookup failed"
    putStrLn "OK"
