module GHC.Stack
  ( module GHC.Stack.Types
  , callStack
  , emptyCallStack
  , freezeCallStack
  , fromCallSiteList
  , getCallStack
  , popCallStack
  , prettyCallStack
  , withFrozenCallStack
  ) where

import Prelude

import GHC.Stack.Types

callStack :: CallStack
callStack = EmptyCallStack

emptyCallStack :: CallStack
emptyCallStack = EmptyCallStack

freezeCallStack :: CallStack -> CallStack
freezeCallStack cs = cs

fromCallSiteList :: [(String, SrcLoc)] -> CallStack
fromCallSiteList _ = EmptyCallStack

getCallStack :: CallStack -> [(String, SrcLoc)]
getCallStack _ = []

popCallStack :: CallStack -> CallStack
popCallStack cs = cs

prettyCallStack :: CallStack -> String
prettyCallStack _ = ""

-- The real function has a rank-2 type so the callee sees the frozen
-- stack.  The stub stack is always empty, and HasCallStack is global.
withFrozenCallStack :: a -> a
withFrozenCallStack x = x
