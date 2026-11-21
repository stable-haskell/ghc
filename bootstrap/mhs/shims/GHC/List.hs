-- MicroHs has no GHC.List.  GHC.Core.Type needs 'build' and
-- GHC.Iface.Recomp needs 'uncons'; everything else comes straight
-- from Data.List (foldl', scanl', unzip4-7, ...).
module GHC.List
  ( module Data.List
  , build
  , augment
  , errorEmptyList
  ) where

import Prelude

import Data.List

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

augment :: (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
augment g xs = g (:) xs

errorEmptyList :: String -> a
errorEmptyList fun = error ("GHC.List." ++ fun ++ ": empty list")
