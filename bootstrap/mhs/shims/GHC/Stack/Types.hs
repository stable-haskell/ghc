-- MicroHs has no implicit-parameter call stacks.  HasCallStack is an empty
-- class with a global instance, so every GHC signature that asks for it
-- still type-checks.  The stack itself is empty.
module GHC.Stack.Types
  ( HasCallStack
  , CallStack(EmptyCallStack)
  , SrcLoc(..)
  ) where

import Prelude

class HasCallStack
instance HasCallStack

data CallStack = EmptyCallStack

data SrcLoc = SrcLoc
  { srcLocPackage   :: String
  , srcLocModule    :: String
  , srcLocFile      :: String
  , srcLocStartLine :: Int
  , srcLocStartCol  :: Int
  , srcLocEndLine   :: Int
  , srcLocEndCol    :: Int
  }
