#!/bin/sh
# relocate.sh — bundled with the stable-haskell wasm32-wasi GHC bindist.
#
# Run once after extracting the tarball to a new install prefix.
# The package-db *.conf files already use ${pkgroot}/../lib/wasm32-wasi/…
# (rewritten at build time by Makefile:DIST_COPY_LIB_CONF_CROSS), so the
# only thing that needs adjusting is the binary ghc-pkg cache, which
# encodes absolute paths from the build environment.
#
# ghcup invokes this from viPostInstall with PWD = install prefix.
#
# DRAFT — lives in lode/ until Phase 3 ships. Integrate into the Makefile
# bindist target ($(DIST_DIR)/ghc-wasm32-wasi.tar.gz, Makefile:1029) so the
# .tar.xz contains relocate.sh at the top level.
set -e

PREFIX="$(cd "$(dirname "$0")" && pwd)"
GHC_PKG="$PREFIX/bin/wasm32-wasi-ghc-pkg"
PKG_DB="$PREFIX/lib/targets/wasm32-wasi/lib/package.conf.d"

if [ ! -x "$GHC_PKG" ]; then
  echo "error: $GHC_PKG not found or not executable" >&2
  exit 1
fi
if [ ! -d "$PKG_DB" ]; then
  echo "error: $PKG_DB not found" >&2
  exit 1
fi

echo "Relocating wasm32-wasi-ghc package db to: $PKG_DB"
"$GHC_PKG" recache --package-db "$PKG_DB"

echo
echo "Stable Haskell wasm32-wasi-ghc ready."
echo "  Compiler:  $PREFIX/bin/wasm32-wasi-ghc"
echo "  Package db: $PKG_DB"
echo
echo "Add $PREFIX/bin to your PATH, or invoke via ghcup."
