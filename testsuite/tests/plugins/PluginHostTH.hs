{-# LANGUAGE TemplateHaskell #-}
module PluginHostTH where

import Control.Monad (unless)
import Language.Haskell.TH.Syntax (runIO)
import System.Environment (lookupEnv)

answer :: Int
answer = $(do
  marker <- runIO (lookupEnv "GHC_PLUGIN_HOST_TEST")
  unless (marker == Just "target") (fail "TH did not run in the external interpreter")
  [| 42 |])
