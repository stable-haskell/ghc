-- MicroHs has no GHC.Exception.Type.  The two stage1 importers
-- (GHC.Iface.Errors.Types, GHC.Tc.Errors.Types) only need
-- 'SomeException'; the rest mirrors GHC.Exception.Type so later
-- stages keep compiling.
module GHC.Exception.Type
  ( Exception(..)
  , SomeException(..)
  , ArithException(..)
  , divZeroException
  , overflowException
  , ratioZeroDenomException
  , underflowException
  ) where

import Prelude

import Control.Exception

divZeroException, overflowException,
  ratioZeroDenomException, underflowException :: SomeException
divZeroException        = toException DivideByZero
overflowException       = toException Overflow
ratioZeroDenomException = toException RatioZeroDenominator
underflowException      = toException Underflow
