-- MicroHs's Data.ByteString.Lazy has no .Internal submodule and no
-- defaultChunkSize.  libraries/ghc-boot/GHC/Unit/Database.hs imports
-- 'defaultChunkSize' from here (aliased as BS.Lazy), so this provides
-- it plus the core lazy-ByteString API re-exported from
-- Data.ByteString.Lazy.
module Data.ByteString.Lazy.Internal
  ( defaultChunkSize
  , ByteString
  , toChunks
  , fromChunks
  ) where

import Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- | Default chunk size for lazy-ByteString input, matching upstream
-- bytestring's 32KB default.  Only used as an 'hGet' chunk size.
defaultChunkSize :: Int
defaultChunkSize = 32 * 1024

-- | Lazy 'ByteString', via 'BL.ByteString'.  (MicroHs cannot re-export
-- imported names, so these are thin local wrappers.)
type ByteString = BL.ByteString

-- | Split a lazy 'ByteString' into strict chunks.
toChunks :: ByteString -> [BS.ByteString]
toChunks = BL.toChunks

-- | Build a lazy 'ByteString' from strict chunks.
fromChunks :: [BS.ByteString] -> ByteString
fromChunks = BL.fromChunks
