-- MicroHs exports the sized integral types from Data.Int.
-- GHC.Types.Unique.Supply needs Int(..); GHC.StgToJS.Linker.Opt
-- imports the whole module.
module GHC.Int
  ( Int(..)
  , Int8(..)
  , Int16(..)
  , Int32(..)
  , Int64(..)
  ) where

import Prelude

import Data.Int
