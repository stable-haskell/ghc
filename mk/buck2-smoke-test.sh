#!/bin/sh
# Smoke test of a GHC installation built with buck2 (see README.md,
# "Building with Buck2"): the store directory of //buck2-ghc:stage2-libdir
# (bin/ghc, bin/ghc-pkg, ...). Compiles and runs small programs that use
# the libraries, the threaded RTS, Template Haskell, the FFI, hsc2hs and
# runghc.
#
#   mk/buck2-smoke-test.sh $(buck2 build //buck2-ghc:stage2-libdir -m opt --show-simple-output)
set -eu

store=$(realpath "$1")
ghc="$store/bin/ghc"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"

echo "== $ghc --version"
"$ghc" --version
"$store/bin/ghc-pkg" list --simple-output | tr ' ' '\n' | grep -c . | sed 's/^/packages: /'
"$store/bin/ghc-pkg" check

echo "== a program with containers, directory and text"
cat > Hello.hs <<'HS'
module Main where
import qualified Data.Map as M
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
main :: IO ()
main = do
  d <- getCurrentDirectory
  putStrLn (T.unpack (T.toUpper (T.pack "hello")) ++ " " ++ show (M.toList (M.fromList [(1 :: Int, "a")])) ++ " " ++ show (not (null d)))
HS
"$ghc" -O Hello.hs -o hello
./hello | grep -q '^HELLO \[(1,"a")\] True$'

echo "== the threaded RTS"
"$ghc" -O -threaded Hello.hs -o hello-thr -fforce-recomp
./hello-thr +RTS -N2 -RTS | grep -q '^HELLO'

echo "== Template Haskell (the internal interpreter)"
cat > TH.hs <<'HS'
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
main :: IO ()
main = print $(litE (integerL (sum [1 .. 10])))
HS
"$ghc" TH.hs -o th
test "$(./th)" = 55

echo "== the FFI"
cat > cbits.c <<'C'
int twice(int x) { return 2 * x; }
C
cat > FFI.hs <<'HS'
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
foreign import ccall "twice" c_twice :: Int -> Int
main :: IO ()
main = print (c_twice 21)
HS
"$ghc" FFI.hs cbits.c -o ffi
test "$(./ffi)" = 42

echo "== hsc2hs"
cat > Hsc.hsc <<'HS'
module Main where
#include <limits.h>
main :: IO ()
main = print ((#const CHAR_BIT) :: Int)
HS
"$store/bin/hsc2hs" Hsc.hsc -o Hsc.hs
"$ghc" Hsc.hs -o hsc
test "$(./hsc)" = 8

echo "== ghc -e and runghc"
test "$("$ghc" -e 'product [1 .. 5 :: Int]')" = 120
test "$("$store/bin/runghc" Hello.hs)" = 'HELLO [(1,"a")] True'

echo "== haddock and hpc run"
"$store/bin/haddock" --version | head -1
"$store/bin/hpc" version

echo "buck2 smoke test: OK"
