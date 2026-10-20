#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

mkdir -p "$tmp/hadrian/cfg" "$tmp/ghc" "$tmp/compiler/GHC/CmmToLlvm/Version" \
  "$tmp/libraries/ghc-boot" "$tmp/libraries/ghc-boot-th" \
  "$tmp/libraries/ghc-boot-th-next" "$tmp/libraries/ghc-heap" \
  "$tmp/libraries/template-haskell" "$tmp/libraries/ghci" \
  "$tmp/utils/ghc-pkg" "$tmp/utils/ghc-iserv" "$tmp/utils/runghc" \
  "$tmp/libraries/ghc-internal" "$tmp/libraries/ghc-experimental" \
  "$tmp/libraries/base" "$tmp/rts/include"

autoconf -I "$root/m4" -o "$tmp/configure" "$root/configure.ac"
cp "$root/config.guess" "$root/config.sub" "$tmp/"
(
  cd "$tmp"
  ./configure --no-create --srcdir="$root" \
    --build=x86_64-unknown-linux-gnu \
    --host=aarch64-unknown-linux-gnu \
    --target=wasm32-unknown-wasi >configure.log 2>&1
  ./config.status hadrian/cfg/system.config
)

if (cd "$tmp" && ./configure --no-create --srcdir="$root" \
    --build=x86_64-unknown-linux-gnu \
    --host=aarch64-unknown-linux-gnu \
    --target=wasm32-unknown-wasi-prof) >"$tmp/prof-configure.log" 2>&1; then
  echo "configure unexpectedly accepted a profiling ABI as --target" >&2
  exit 1
fi
grep -F -- "--target=wasm32-unknown-wasi-prof selects a Haskell profiling ABI, not a machine triple" "$tmp/prof-configure.log"
if grep -Fq "checking for Python" "$tmp/prof-configure.log"; then
  echo "configure reached tool probes before rejecting the profiling ABI" >&2
  exit 1
fi

config="$tmp/hadrian/cfg/system.config"
grep -Fx 'build-platform-full    = x86_64-unknown-linux-gnu' "$config"
grep -Fx 'host-platform-full     = aarch64-unknown-linux-gnu' "$config"
grep -Fx 'target-platform-full   = wasm32-unknown-wasi' "$config"
