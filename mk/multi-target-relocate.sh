#!/bin/sh
# relocate.sh — bundled with the stable-haskell multi-target GHC bindist.
#
# Recaches the per-target package-dbs (native + each cross target) after
# the bindist is extracted to a new prefix. The .conf files use
# ${pkgroot}-relative paths (rewritten at build time by the Makefile's
# DIST_COPY_LIB_CONF and DIST_COPY_LIB_CONF_CROSS rules), so only the
# binary package.cache files need refreshing.
#
# Standalone usage: cd into the extracted bindist root and run ./relocate.sh
# Ghcup usage:      invoked by the bundled Makefile's install target

set -e
PREFIX="$(cd "$(dirname "$0")" && pwd)"

# ----------------------------------------------------------------------------
# Native ghc-pkg + native package db
# ----------------------------------------------------------------------------
NATIVE_GHC_PKG="$PREFIX/bin/ghc-pkg"
NATIVE_PKG_DB="$PREFIX/lib/package.conf.d"
if [ -x "$NATIVE_GHC_PKG" ] && [ -d "$NATIVE_PKG_DB" ]; then
  echo "[native] recaching $NATIVE_PKG_DB"
  "$NATIVE_GHC_PKG" recache --package-db "$NATIVE_PKG_DB"
else
  echo "[native] no native ghc-pkg + package db at expected paths — skipping"
fi

# ----------------------------------------------------------------------------
# Per-cross-target ghc-pkg + each target's package db
# ----------------------------------------------------------------------------
# Discover targets by looking at lib/targets/<triple>/ subdirectories — keeps
# this script tolerant if some build flavour omits one of the targets.
if [ -d "$PREFIX/lib/targets" ]; then
  for target_dir in "$PREFIX/lib/targets"/*/; do
    [ -d "$target_dir" ] || continue
    triple="$(basename "$target_dir")"
    cross_pkg="$PREFIX/bin/${triple}-ghc-pkg"
    cross_db="$target_dir/lib/package.conf.d"
    if [ -x "$cross_pkg" ] && [ -d "$cross_db" ]; then
      echo "[$triple] recaching $cross_db"
      "$cross_pkg" recache --package-db "$cross_db"
    elif [ -d "$cross_db" ]; then
      # Some cross targets ship no per-target ghc-pkg (e.g. older JS bindists).
      # Fall back to the native ghc-pkg; the .conf files are arch-agnostic
      # ASCII so the native binary can read+recache them.
      echo "[$triple] using native ghc-pkg to recache $cross_db (no $cross_pkg)"
      "$NATIVE_GHC_PKG" recache --package-db "$cross_db" || \
        echo "[$triple] recache failed — package.cache may be stale" >&2
    fi
  done
fi

# ----------------------------------------------------------------------------
# Tool prerequisites — warn but don't fail
# ----------------------------------------------------------------------------
need_node=
need_emcc=
need_wasi_sdk=
[ -d "$PREFIX/lib/targets/wasm32-unknown-wasi" ] && { need_node=1; need_wasi_sdk=1; }
[ -d "$PREFIX/lib/targets/javascript-unknown-ghcjs" ] && { need_node=1; need_emcc=1; }

warn_missing=
if [ -n "$need_node" ] && ! command -v node >/dev/null 2>&1; then
  warn_missing="${warn_missing} node (Node.js >= 22)"
fi
if [ -n "$need_wasi_sdk" ] && ! command -v wasm32-unknown-wasi-clang >/dev/null 2>&1; then
  warn_missing="${warn_missing} wasm32-unknown-wasi-clang (wasi-sdk)"
fi
if [ -n "$need_emcc" ] && ! command -v emcc >/dev/null 2>&1; then
  warn_missing="${warn_missing} emcc (emscripten)"
fi

if [ -n "$warn_missing" ]; then
  cat >&2 <<EOF

NOTE: Some target prerequisites were not found on PATH:
     $warn_missing

      The compiler will still install, but cross-target compilation
      requires these tools at build time. See
      https://stable-haskell.github.io/ghc/install/ for setup
      instructions.

EOF
fi

echo ""
echo "Stable Haskell multi-target GHC ready."
echo "Invocations:"
echo "  $PREFIX/bin/ghc                          (native)"
[ -d "$PREFIX/lib/targets/wasm32-unknown-wasi" ] && \
  echo "  $PREFIX/bin/wasm32-unknown-wasi-ghc      (wasm cross)"
[ -d "$PREFIX/lib/targets/javascript-unknown-ghcjs" ] && \
  echo "  $PREFIX/bin/javascript-unknown-ghcjs-ghc (JS cross)"
