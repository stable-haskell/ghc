{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Data.List.NonEmpty (module Data.List.NonEmpty, module GHC.Data.List.NonEmpty, toList) where

import Prelude (Bool, (.))
import Control.Applicative
#if !defined(__MHS__)
import qualified Control.Monad as List (zipWithM)
#endif
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty hiding (toList, unzip)
#if defined(__MHS__)
import qualified Data.List as DL
#endif
import qualified Data.List as List
import qualified GHC.Data.List as List

zipWithM :: Applicative f => (a -> b -> f c) -> NonEmpty a -> NonEmpty b -> f (NonEmpty c)
#if defined(__MHS__)
-- sequenceA (zipWith ...) rather than Control.Monad.zipWithM.  GHC's
-- zipWithM is Applicative; MicroHs 0.16 still gives it a Monad constraint,
-- so the Applicative signature below does not type-check there.
zipWithM f (a:|as) (b:|bs) = liftA2 (:|) (f a b) (sequenceList (DL.zipWith f as bs))
  where
    -- MicroHs's Control.Applicative has no sequenceA.
    sequenceList []     = pure []
    sequenceList (x:xs) = liftA2 (:) x (sequenceList xs)
#else
zipWithM f (a:|as) (b:|bs) = liftA2 (:|) (f a b) (List.zipWithM f as bs)
#endif
-- Inline to enable fusion of `List.zipWithM`
-- See Note [Fusion for zipN/zipWithN] in List.hs
{-# INLINE zipWithM #-}

unzip :: NonEmpty (a, b) -> (NonEmpty a, NonEmpty b)
unzip ((a,b):|xs) = (a:|as, b:|bs)
  where
    (as, bs) = List.unzip xs

unzip3 :: NonEmpty (a, b, c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
unzip3 ((a,b,c):|xs) = (a:|as, b:|bs, c:|cs)
  where
    (as, bs, cs) = List.unzip3 xs

mapAndUnzip :: (a -> (b, c)) -> NonEmpty a -> (NonEmpty b, NonEmpty c)
mapAndUnzip f (x:|xs) = (b:|bs, c:|cs)
  where
    (b, c) = f x
    (bs, cs) = List.mapAndUnzip f xs

mapAndUnzip3 :: (a -> (b, c, d)) -> NonEmpty a -> (NonEmpty b, NonEmpty c, NonEmpty d)
mapAndUnzip3 f (x:|xs) = (b:|bs, c:|cs, d:|ds)
  where
    (b, c, d) = f x
    (bs, cs, ds) = List.mapAndUnzip3 f xs

isSingleton :: NonEmpty a -> Bool
isSingleton = List.null . tail
