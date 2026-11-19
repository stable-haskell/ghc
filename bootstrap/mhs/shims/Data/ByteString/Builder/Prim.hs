-- MicroHs has no Data.ByteString.Builder.Prim (only
-- Data.ByteString.Builder).  This provides the exact subset used by
-- compiler/GHC/CoreToStg/Prep.hs, backed by Data.ByteString.Builder.
module Data.ByteString.Builder.Prim
  ( FixedPrim(..)
  , primUnfoldrFixed
  , word32LE
  , word32BE
  , word64LE
  , word64BE
  ) where

import Prelude

import Data.Word (Word32, Word64)
import qualified Data.ByteString.Builder as BB

-- | A fixed-size serialisation of @a@: a function to a 'BB.Builder'.
newtype FixedPrim a = FixedPrim { runFixedPrim :: a -> BB.Builder }

-- | Unfold a sequence of fixed-size values into a 'BB.Builder'.
primUnfoldrFixed :: FixedPrim a -> (b -> Maybe (a, b)) -> b -> BB.Builder
primUnfoldrFixed (FixedPrim p) f = go
  where
    go b = case f b of
      Nothing -> mempty
      Just (a, b') -> p a `mappend` go b'

word32LE :: FixedPrim Word32
word32LE = FixedPrim BB.word32LE

word32BE :: FixedPrim Word32
word32BE = FixedPrim BB.word32BE

word64LE :: FixedPrim Word64
word64LE = FixedPrim BB.word64LE

word64BE :: FixedPrim Word64
word64BE = FixedPrim BB.word64BE
