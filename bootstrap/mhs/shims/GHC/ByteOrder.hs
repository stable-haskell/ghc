-- MicroHs has no GHC.ByteOrder.  The bootstrap host is little-endian.
module GHC.ByteOrder (ByteOrder(..), targetByteOrder) where

import Prelude

data ByteOrder = BigEndian | LittleEndian
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

targetByteOrder :: ByteOrder
targetByteOrder = LittleEndian
