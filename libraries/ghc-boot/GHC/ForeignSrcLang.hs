{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | See @GHC.LanguageExtensions@ for an explanation
-- on why this is needed
module GHC.ForeignSrcLang
  ( module GHC.ForeignSrcLang.Type
  ) where

import Data.Binary
import GHC.ForeignSrcLang.Type

#if defined(__MHS__)
-- binary-mhs has no Generic defaults.
instance Binary ForeignSrcLang where
  put LangC    = putWord8 0
  put LangCxx  = putWord8 1
  put LangObjc = putWord8 2
  get = do
    tag <- getWord8
    case tag of
      0 -> return LangC
      1 -> return LangCxx
      2 -> return LangObjc
      _ -> fail ("ForeignSrcLang: bad tag " ++ show tag)
#else
instance Binary ForeignSrcLang
#endif
