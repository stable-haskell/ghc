{-# LANGUAGE TemplateHaskell #-}
module PluginHostCross where

import Language.Haskell.TH.Syntax (runIO)
import qualified System.Info as Info

-- The cross-compiler must evaluate this splice with its target interpreter
-- while executing Simple.Plugin inside its native compiler process.
answer :: Int
answer = $(do
  runIO (putStrLn ("TH platform: " ++ Info.arch ++ "-" ++ Info.os))
  [| 42 |])
