-- MicroHs has no profiler cost-centre stacks.  The stage1 importers
-- (GHC.Linker.Types, GHC.Linker.Loader, GHC.Runtime.Eval.Types,
-- GHC.Runtime.Interpreter) only need the CostCentre/CostCentreStack
-- types (via RemotePtr); every operation is a stub returning an
-- empty stack.
module GHC.Stack.CCS
  ( CostCentreStack
  , CostCentre
  , currentCallStack
  , whoCreated
  , getCurrentCCS
  , getCCSOf
  , clearCCS
  , ccsCC
  , ccsParent
  , ccLabel
  , ccModule
  , ccSrcSpan
  , ccsToStrings
  , renderStack
  ) where

import Prelude

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.String (CString)

data CostCentreStack

data CostCentre

currentCallStack :: IO [String]
currentCallStack = return []

whoCreated :: a -> IO [String]
whoCreated _ = return []

getCurrentCCS :: dummy -> IO (Ptr CostCentreStack)
getCurrentCCS _ = return nullPtr

getCCSOf :: a -> IO (Ptr CostCentreStack)
getCCSOf _ = return nullPtr

clearCCS :: IO a -> IO a
clearCCS m = m

ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC _ = return nullPtr

ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent _ = return nullPtr

ccLabel :: Ptr CostCentre -> IO CString
ccLabel _ = return nullPtr

ccModule :: Ptr CostCentre -> IO CString
ccModule _ = return nullPtr

ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan _ = return nullPtr

ccsToStrings :: Ptr CostCentreStack -> IO [String]
ccsToStrings _ = return []

renderStack :: [String] -> String
renderStack strs =
  "CallStack (from -prof):" ++ concatMap ("\n  " ++) (reverse strs)
