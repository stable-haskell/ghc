{-# LANGUAGE CPP #-}
-- | See GHC #10762 and #15021.
module GHC.HandleEncoding (configureHandleEncoding) where

import Prelude -- See note [Why do we import Prelude here?]
#if !defined(__MHS__)
import GHC.IO.Encoding (textEncodingName)
import System.Environment
#endif
import System.IO

-- | Handle GHC-specific character encoding flags, allowing us to control how
-- GHC produces output regardless of OS.
configureHandleEncoding :: IO ()
configureHandleEncoding = do
#if defined(__MHS__)
   -- MicroHs has a single TextEncoding and no "//TRANSLIT" encodings.
   -- Force UTF-8 so the bootstrap compiler can print its own diagnostics.
   hSetEncoding stdout utf8
   hSetEncoding stderr utf8
#else
   mb_val <- lookupEnv "GHC_CHARENC"
   case mb_val of
    Just "UTF-8" -> do
     hSetEncoding stdout utf8
     hSetEncoding stderr utf8
    _ -> do
     -- Avoid GHC erroring out when trying to display unhandled characters
     hSetTranslit stdout
     hSetTranslit stderr
#endif

-- | Change the character encoding of the given Handle to transliterate
-- on unsupported characters instead of throwing an exception
#if !defined(__MHS__)
hSetTranslit :: Handle -> IO ()
hSetTranslit h = do
    menc <- hGetEncoding h
    case fmap textEncodingName menc of
        Just name | '/' `notElem` name -> do
            enc' <- mkTextEncoding $ name ++ "//TRANSLIT"
            hSetEncoding h enc'
        _ -> return ()
#endif
