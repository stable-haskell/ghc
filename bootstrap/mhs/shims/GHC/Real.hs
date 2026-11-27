-- MicroHs has no GHC.Real.  The Ratio constructor (:%) is hidden
-- behind Data.Ratio's abstract export, but Data.Ratio_Type exposes
-- the real constructor, which is what GHC.HsToCore.Match.Literal,
-- GHC.HsToCore.Pmc.Solver.Types, GHC.Types.SourceText and
-- GHC.Utils.Binary pattern-match on.
module GHC.Real
  ( Real(..)
  , Integral(..)
  , Fractional(..)
  , RealFrac(..)
  , fromIntegral
  , realToFrac
  , even
  , odd
  , gcd
  , lcm
  , (^)
  , (^^)
  , Ratio(..)
  , Rational
  , (%)
  , numerator
  , denominator
  , reduce
  , divZeroError
  , overflowError
  , ratioZeroDenominatorError
  ) where

import Prelude

import Data.Real (Real(..))
import Data.Integral (Integral(..), fromIntegral, even, odd, gcd, lcm, (^))
import Data.Fractional (Fractional(..), realToFrac, (^^))
import Data.RealFrac (RealFrac(..))
import Data.Ratio (Rational, (%), numerator, denominator)
import Data.Ratio_Type (Ratio(..))

reduce :: (Ord a, Integral a) => a -> a -> Ratio a
reduce = (%)

divZeroError :: a
divZeroError = error "GHC.Real.divZeroError: divide by zero"

overflowError :: a
overflowError = error "GHC.Real.overflowError: arithmetic overflow"

ratioZeroDenominatorError :: a
ratioZeroDenominatorError = error "GHC.Real.ratioZeroDenominatorError: Ratio has zero denominator"
