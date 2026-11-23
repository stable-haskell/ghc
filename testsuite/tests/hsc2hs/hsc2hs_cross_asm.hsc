{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Main (main) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Control.Monad (unless)
import System.Exit (exitFailure)

#include "hsc2hs_cross_asm_c.h"

#ifdef TEST_RSP_INCLUDE
#include "hsc2hs_rsp_inc.h"
rspSentinel :: Int
rspSentinel = #const RSP_SENTINEL_VAL
#endif

-- Custom #let directive (previously rejected by --cross-compile!)
#let custom_getter t, f = "get_" #f " :: Ptr () -> IO (%s)\nget_" #f " p = peekByteOff p %ld", \
    (((__typeof__(((t *)0)->f))(-1) < 0) ? "Int32" : "Word8"), \
    (long)offsetof(t, f)

newtype TestStatus = TestStatus CInt deriving (Eq, Show)

-- #enum testing hsc_haskellize: tsOk, _TsLeading, ts2Words, customNegErr
#enum TestStatus, TestStatus, TS_OK, _TS_LEADING, TS_2_WORDS, customNegErr = TS_NEG_ERR

#custom_getter struct test_layout_s, tag

cMinInt64 :: Int64
cMinInt64 = #const INT64_MIN

cMaxWord64 :: Word64
cMaxWord64 = #const UINT64_MAX

cComplexStr :: String
cComplexStr = #const_str TEST_COMPLEX_STR

cArrayStr :: String
cArrayStr = #const_str kStaticArrayStr

cNonUtf8Str :: String
cNonUtf8Str = #const_str TEST_NON_UTF8_BYTES

cMapFailed :: IntPtr
cMapFailed = #const (intptr_t)TEST_MAP_FAILED

type TargetChar   = #type char
type TargetLong   = #type long
type TargetDouble = #type double
type TargetSize   = #type size_t

layoutSize :: Int
layoutSize = #size struct test_layout_s

layoutAlign :: Int
layoutAlign = #alignment struct test_layout_s

ptrOffset :: Int
ptrOffset = #offset struct test_layout_s, ptr

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq lbl got expected =
    unless (got == expected) $ do
        putStrLn ("FAIL [" ++ lbl ++ "]: got " ++ show got ++ ", expected " ++ show expected)
        exitFailure

main :: IO ()
main = do
    assertEq "INT64_MIN"  cMinInt64  minBound
    assertEq "UINT64_MAX" cMaxWord64 maxBound
    assertEq "const_str"  cComplexStr "hsc2hs-\"quote\"-\\slash-\n99-end"
    assertEq "array_str"  cArrayStr   "array-decay-ok"
    assertEq "non_utf8_const_str" cNonUtf8Str "raw-\255\254\128-ok"
    assertEq "MAP_FAILED" cMapFailed  (-1)
    assertEq "tsOk"       tsOk         (TestStatus 0)
    assertEq "_TsLeading" _TsLeading   (TestStatus 7)
    assertEq "ts2Words"   ts2Words     (TestStatus 42)
    assertEq "customNeg"  customNegErr (TestStatus (-13))
    unless (layoutSize >= 16 && layoutAlign >= 4 && ptrOffset >= 8) exitFailure
    putStrLn "hsc2hs_cross_asm: OK"
