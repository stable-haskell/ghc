#!/bin/sh
# relocate.sh — bundled with the stable-haskell wasm32-unknown-wasi GHC bindist.
#
# Run once after extracting the tarball to a new install prefix.
# The package-db *.conf files already use ${pkgroot}/../lib/wasm32-unknown-wasi/…
# (rewritten at build time by Makefile:DIST_COPY_LIB_CONF_CROSS), so the
# only thing that needs adjusting is the binary ghc-pkg cache, which
# encodes absolute paths from the build environment.
#
# Standalone usage: cd into the extracted bindist root and run ./relocate.sh
# Ghcup usage:      invoked from viPostInstall with PWD = install prefix
set -e

PREFIX="$(cd "$(dirname "$0")" && pwd)"
GHC_PKG="$PREFIX/bin/wasm32-unknown-wasi-ghc-pkg"
PKG_DB="$PREFIX/lib/targets/wasm32-unknown-wasi/lib/package.conf.d"

if [ ! -x "$GHC_PKG" ]; then
  echo "error: $GHC_PKG not found or not executable" >&2
  echo "       extract the bindist tarball before running this script" >&2
  exit 1
fi
if [ ! -d "$PKG_DB" ]; then
  echo "error: $PKG_DB not found" >&2
  echo "       expected layout: <prefix>/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/" >&2
  exit 1
fi

echo "Relocating wasm32-unknown-wasi-ghc package db to: $PKG_DB"
"$GHC_PKG" recache --package-db "$PKG_DB"

cat <<EOF

Stable Haskell wasm32-unknown-wasi-ghc ready.
  Compiler:   $PREFIX/bin/wasm32-unknown-wasi-ghc
  Package db: $PKG_DB

Add $PREFIX/bin to your PATH, or invoke via ghcup.
EOF
