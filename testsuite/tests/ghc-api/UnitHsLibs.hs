{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main where

import Control.Monad (forM_, unless)
import qualified Data.Set as Set
import GHC.Data.ShortText (pack)
import GHC.Platform.Ways
import GHC.Settings (GhcNameVersion(..))
import GHC.Unit.Info

main :: IO ()
main = forM_ cases $ \(label, ways, expected) -> do
    let actual = unitHsLibs (GhcNameVersion "ghc" "test")
                           (Set.fromList ways) unit
    unless (actual == expected) $
        error (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
  where
    -- unitHsLibs only reads unitLibraries. Include an RTS sub-library to
    -- distinguish its encoded name from the obsolete RTS-only way suffixes.
    unit = GenericUnitInfo
        { unitLibraries = map pack ["HSfoo", "HSrts-threaded-debug", "Cffi"] }
    cases =
        [ ("vanilla", [], ["HSfoo", "HSrts-threaded-debug", "Cffi"])
        , ("profiling", [WayProf],
           ["HSfoo_p", "HSrts-threaded-debug_p", "Cffi_p"])
        , ("dynamic", [WayDyn],
           ["HSfoo-ghctest", "HSrts-threaded-debug-ghctest", "ffi"])
        , ("dynamic profiling", [WayProf, WayDyn],
           ["HSfoo_p-ghctest", "HSrts-threaded-debug_p-ghctest", "ffi_p"])
        , ("RTS-only", [WayThreaded, WayDebug],
           ["HSfoo", "HSrts-threaded-debug", "Cffi"])
        , ("profiling with RTS-only", [WayThreaded, WayDebug, WayProf],
           ["HSfoo_p", "HSrts-threaded-debug_p", "Cffi_p"])
        , ("dynamic profiling with RTS-only", [WayThreaded, WayDebug, WayProf, WayDyn],
           ["HSfoo_p-ghctest", "HSrts-threaded-debug_p-ghctest", "ffi_p"])
        ]
