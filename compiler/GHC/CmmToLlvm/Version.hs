module GHC.CmmToLlvm.Version
  ( LlvmVersion(..)
  , supportedLlvmVersionLowerBound
  , supportedLlvmVersionUpperBound
  , parseLlvmVersion
  , llvmVersionSupported
  , llvmVersionStr
  , llvmVersionList
  )
where

import GHC.Prelude

import GHC.CmmToLlvm.Version.Type

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE

-- HACK HACK
supportedLlvmVersionLowerBound :: LlvmVersion
supportedLlvmVersionLowerBound = LlvmVersion (NE.singleton 0)

-- HACK HACK
supportedLlvmVersionUpperBound :: LlvmVersion
supportedLlvmVersionUpperBound = LlvmVersion (NE.singleton 1000)

-- HACK HACK
parseLlvmVersion :: String -> Maybe LlvmVersion
parseLlvmVersion _ = Nothing

-- HACK HACK
llvmVersionSupported :: LlvmVersion -> Bool
llvmVersionSupported _ = True

llvmVersionStr :: LlvmVersion -> String
llvmVersionStr = intercalate "." . map show . llvmVersionList

llvmVersionList :: LlvmVersion -> [Int]
llvmVersionList = NE.toList . llvmVersionNE
