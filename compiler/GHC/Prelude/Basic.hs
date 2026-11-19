{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -O2 #-} -- See Note [-O2 Prelude]

-- See Note [Proxies for head and tail]
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags -Wno-x-partial #-}

-- | Custom minimal GHC "Prelude"
--
-- This module serves as a replacement for the "Prelude" module
-- and abstracts over differences between the bootstrapping
-- GHC version, and may also provide a common default vocabulary.

-- Every module in GHC
--   * Is compiled with -XNoImplicitPrelude
--   * Explicitly imports GHC.Prelude.Basic or GHC.Prelude
--   * The later provides some functionality with within ghc itself
--     like pprTrace.

module GHC.Prelude.Basic
  ( module X
  , Applicative (..)
  , module Bits
  , bit
  , shiftL, shiftR
  , setBit, clearBit
  , head, tail, unzip

  , strictGenericLength
#if defined(__MHS__)
  , (<$!>)
  , default Num
  , default IsString
  , default Foldable
  , default Traversable
#endif
  ) where


{- Note [-O2 Prelude]
~~~~~~~~~~~~~~~~~~~~~
There is some code in GHC that is *always* compiled with -O[2] because
of it's impact on compile time performance. Some of this code might depend
on the definitions like shiftL being defined here being performant.

So we always compile this module with -O2. It's (currently) tiny so I
have little reason to suspect this impacts overall GHC compile times
negatively.

-}
-- We export the 'Semigroup' class but w/o the (<>) operator to avoid
-- clashing with the (Outputable.<>) operator which is heavily used
-- through GHC's code-base.

{-
Note [Why do we import Prelude here?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The files ghc-boot-th.cabal, ghc-boot.cabal, ghci.cabal and
ghc-heap.cabal contain the directive default-extensions:
NoImplicitPrelude. There are two motivations for this:
  - Consistency with the compiler directory, which enables
    NoImplicitPrelude;
  - Allows loading the above dependent packages with ghc-in-ghci,
    giving a smoother development experience when adding new
    extensions.
-}

import qualified Prelude
#if defined(__MHS__)
-- MicroHs Prelude exports monomorphic Data.List / Control.Monad functions
-- rather than Foldable/Foldable1/Traversable methods for foldr, length, mapM,
-- etc.  Hide the monomorphic versions here so Data.Foldable, Data.Foldable1,
-- and Data.Traversable below re-export the polymorphic methods unambiguously.
import Prelude as X hiding ((<>), Applicative(..), Foldable(..), Traversable(..), head, tail, unzip,
                            foldr, foldl, foldl1, foldr1, length, null, elem,
                            sum, product, maximum, minimum,
                            any, all, and, or, concat, concatMap, notElem,
                            mapM, mapM_, sequence, sequence_)
#else
import Prelude as X hiding ((<>), Applicative(..), Foldable(..), head, tail, unzip)
#endif
import Control.Applicative (Applicative(..))
#if defined(__MHS__)
-- As in GHC base, foldl1, foldr1, maximum and minimum are Foldable methods;
-- MicroHs Data.Foldable1 also exports those four names, so hide them there
-- to keep the re-export unambiguous (verified: plain hiding is ambiguous).
import Data.Foldable as X (Foldable (elem, foldMap, foldl, foldl', foldl1, foldr, foldr1, length, maximum, minimum, null, product, sum),
                           any, all, and, or, concat, concatMap, notElem, mapM_, sequence_)
import Data.Foldable1 as X hiding (head, last, maximum, minimum, foldl1, foldr1)
import Data.Traversable as X (Traversable (traverse, sequenceA, mapM, sequence))
import qualified Data.Data as DData
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Short as DSBS
#else
import Data.Foldable as X (Foldable (elem, foldMap, foldl, foldl', foldr, length, null, product, sum))
import Data.Foldable1 as X hiding (head, last)
#endif
import qualified Data.List as List
import qualified GHC.Data.List.NonEmpty as NE
import GHC.Stack.Types (HasCallStack)

import GHC.Bits as Bits hiding (bit, shiftL, shiftR, setBit, clearBit)
# if defined(DEBUG)
import qualified GHC.Bits as Bits (shiftL, shiftR)
# endif


{- Note [Default to unsafe shifts inside GHC]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The safe shifts can introduce branches which come
at the cost of performance. We still want the additional
debuggability for debug builds. So we define it as one or the
other depending on the DEBUG setting.

Why do we then continue on to re-export the rest of Data.Bits?
If we would not what is likely to happen is:
* Someone imports Data.Bits, uses xor. Things are fine.
* They add a shift and get an ambiguous definition error.
* The are puzzled for a bit.
* They either:
  + Remove the import of Data.Bits and get an error because xor is not in scope.
  + Add the hiding clause to the Data.Bits import for the shifts.

Either is quite annoying. Simply re-exporting all of Data.Bits avoids this
making for a smoother developer experience. At the cost of having a few more
names in scope at all time. But that seems like a fair tradeoff.

See also #19618
-}

-- We always want the Data.Bits method to show up for rules etc.
{-# INLINE shiftL #-}
{-# INLINE shiftR #-}
shiftL, shiftR :: Bits.Bits a => a -> Int -> a
#if defined(DEBUG)
shiftL = Bits.shiftL
shiftR = Bits.shiftR
#else
shiftL = Bits.unsafeShiftL
shiftR = Bits.unsafeShiftR
#endif

{-# INLINE bit #-}
bit :: (Num a, Bits.Bits a) => Int -> a
bit = \ i -> 1 `shiftL` i
{-# INLINE setBit #-}
setBit :: (Num a, Bits.Bits a) => a -> Int -> a
setBit = \ x i -> x Bits..|. bit i
{-# INLINE clearBit #-}
clearBit :: (Num a, Bits.Bits a) => a -> Int -> a
clearBit = \ x i -> x Bits..&. Bits.complement (bit i)

{- Note [Proxies for head and tail]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Prelude.head and Prelude.tail have recently acquired {-# WARNING in "x-partial" #-},
but the GHC codebase uses them fairly extensively and insists on building warning-free.
Thus, instead of adding {-# OPTIONS_GHC -Wno-x-partial #-} to every module which
employs them, we define warning-less proxies and export them from GHC.Prelude.
-}

-- See Note [Proxies for head and tail]
head :: HasCallStack => [a] -> a
head = Prelude.head
{-# INLINE head #-}

-- See Note [Proxies for head and tail]
tail :: HasCallStack => [a] -> [a]
tail = Prelude.tail
{-# INLINE tail #-}

{- |
The 'genericLength' function defined in base can't be specialised due to the
NOINLINE pragma.

It is also not strict in the accumulator, and strictGenericLength is not exported.

See #25706 for why it is important to use a strict, specialised version.

-}
strictGenericLength :: Num a => [x] -> a
#if defined(__MHS__)
strictGenericLength = fromIntegral . List.length
#else
strictGenericLength = fromIntegral . length
#endif

-- This is `Data.Functor.unzip`. Unfortunately, that function lacks the RULES for specialization.
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip = \ xs -> (fmap fst xs, fmap snd xs)
{-# NOINLINE [1] unzip #-}
{-# RULES "unzip/List" unzip = List.unzip #-}
{-# RULES "unzip/NonEmpty" unzip = NE.unzip #-}

#if defined(__MHS__)
infixl 4 <$!>
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z

default Foldable ([])
default Traversable ([])

instance DData.Data DBS.ByteString where
  gfoldl f z bs = z DBS.pack `f` DBS.unpack bs
  toConstr _    = bsPackConstr
  gunfold k z c = case DData.constrIndex c of
    1 -> k (z DBS.pack)
    _ -> Prelude.error "Data.ByteString.ByteString.gunfold"
  dataTypeOf _  = bsDataType

bsPackConstr :: DData.Constr
bsPackConstr = DData.mkConstr bsDataType "pack" [] DData.Prefix

bsDataType :: DData.DataType
bsDataType = DData.mkDataType "Data.ByteString.ByteString" [bsPackConstr]

instance DData.Data DSBS.ShortByteString where
  gfoldl f z sbs = z DSBS.pack `f` DSBS.unpack sbs
  toConstr _     = sbsPackConstr
  gunfold k z c  = case DData.constrIndex c of
    1 -> k (z DSBS.pack)
    _ -> Prelude.error "Data.ByteString.Short.ShortByteString.gunfold"
  dataTypeOf _   = sbsDataType

sbsPackConstr :: DData.Constr
sbsPackConstr = DData.mkConstr sbsDataType "pack" [] DData.Prefix

sbsDataType :: DData.DataType
sbsDataType = DData.mkDataType "Data.ByteString.Short.ShortByteString" [sbsPackConstr]
#endif
