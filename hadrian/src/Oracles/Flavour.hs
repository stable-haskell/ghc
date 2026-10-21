{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Oracles.Flavour
  ( oracles
  , askDynGhcPrograms
  ) where

import Base
import Flavour
import Settings (flavour)

newtype DynGhcPrograms =
  DynGhcPrograms () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult DynGhcPrograms = Bool

oracles :: Rules ()
oracles = do
  void $ addOracle $ \(DynGhcPrograms _) -> dynamicGhcPrograms =<< flavour

askDynGhcPrograms :: Action Bool
askDynGhcPrograms = askOracle $ DynGhcPrograms ()
