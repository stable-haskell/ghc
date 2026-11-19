#!/usr/bin/env bash
#
# Generate portable Haskell 2010 Happy parsers and Alex lexers for stage1.
#
# Outputs (OUT_AUTOGEN defaults to _build/mhs-stage1/autogen):
#   $OUT_AUTOGEN/GHC/Parser.hs                <- compiler/GHC/Parser.y
#   $OUT_AUTOGEN/GHC/Parser/Lexer.hs          <- compiler/GHC/Parser/Lexer.x
#   $OUT_AUTOGEN/GHC/Parser/Lexer/String.hs   <- compiler/GHC/Parser/Lexer/String.x
#   $OUT_AUTOGEN/GHC/Parser/HaddockLex.hs     <- compiler/GHC/Parser/HaddockLex.x
#   $OUT_AUTOGEN/GHC/Cmm/Parser.hs            <- compiler/GHC/Cmm/Parser.y
#   $OUT_AUTOGEN/GHC/Cmm/Lexer.hs             <- compiler/GHC/Cmm/Lexer.x
#   $OUT_AUTOGEN/genprimopcode/Parser.hs      <- utils/genprimopcode/Parser.y
#   $OUT_AUTOGEN/genprimopcode/Lexer.hs       <- utils/genprimopcode/Lexer.x
#
# Both happy (stable-haskell/happy feat/portable-haskell2010) and alex
# (stable-haskell/alex feat/portable-bootstrap) are built natively with MicroHs
# into $OUT/bin and invoked without -g so they emit standard Haskell 2010
# Data.Array tables directly with zero post-processing.
#
# --latin1 is passed to alex for the compiler package, matching Hadrian
# (hadrian/src/Settings/Packages.hs: builder Alex ? arg "--latin1");
# utils/genprimopcode uses alex defaults, also matching Hadrian.
#
# Idempotent: an output is regenerated only when it is missing or older than
# its .x/.y source or this script.
#
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
OUT_AUTOGEN="${1:-$ROOT/_build/mhs-stage1/autogen}"
mkdir -p "$OUT_AUTOGEN"
OUT_DIR="$(cd "$(dirname "$OUT_AUTOGEN")" && pwd)"

HAPPY_BIN="${HAPPY_BIN:-$OUT_DIR/bin/happy}"
if [ ! -x "$HAPPY_BIN" ]; then
  HAPPY_BIN="$(command -v happy || true)"
fi
if [ ! -x "$HAPPY_BIN" ]; then
  echo "gen-parsers.sh: error: happy not found (expected $OUT_DIR/bin/happy)" >&2
  exit 1
fi

ALEX_BIN="${ALEX_BIN:-$OUT_DIR/bin/alex}"
if [ ! -x "$ALEX_BIN" ]; then
  ALEX_BIN="$(command -v alex || true)"
fi
if [ ! -x "$ALEX_BIN" ]; then
  echo "gen-parsers.sh: error: alex not found (expected $OUT_DIR/bin/alex)" >&2
  exit 1
fi

echo "gen-parsers.sh: happy=$HAPPY_BIN ($("$HAPPY_BIN" --version | head -n 1))"
echo "gen-parsers.sh: alex=$ALEX_BIN ($("$ALEX_BIN" --version | head -n 1))"

is_fresh() {
  [ -n "${1:-}" ] && [ -n "${2:-}" ] \
    && [ -f "$1" ] && [ "$1" -nt "$2" ] && [ "$1" -nt "$SCRIPT_PATH" ]
}

gen_happy() {
  local src="$1" dst="$2"
  if is_fresh "$dst" "$ROOT/$src"; then
    echo "fresh: $dst"
    return 0
  fi
  mkdir -p "$(dirname "$dst")"
  local tmp="$dst.tmp.$$"
  echo "happy $src -> $dst"
  "$HAPPY_BIN" "$ROOT/$src" -o "$tmp"
  mv "$tmp" "$dst"
}

gen_alex() {
  local flags="$1" src="$2" dst="$3"
  if is_fresh "$dst" "$ROOT/$src"; then
    echo "fresh: $dst"
    return 0
  fi
  mkdir -p "$(dirname "$dst")"
  local tmp="$dst.tmp.$$"
  echo "alex $flags $src -> $dst"
  # shellcheck disable=SC2086
  "$ALEX_BIN" $flags "$ROOT/$src" -o "$tmp"
  mv "$tmp" "$dst"
}

gen_happy "compiler/GHC/Parser.y"        "$OUT_AUTOGEN/GHC/Parser.hs"
gen_happy "compiler/GHC/Cmm/Parser.y"    "$OUT_AUTOGEN/GHC/Cmm/Parser.hs"
gen_happy "utils/genprimopcode/Parser.y" "$OUT_AUTOGEN/genprimopcode/Parser.hs"

gen_alex "--latin1" "compiler/GHC/Parser/Lexer.x"        "$OUT_AUTOGEN/GHC/Parser/Lexer.hs"
gen_alex "--latin1" "compiler/GHC/Parser/Lexer/String.x" "$OUT_AUTOGEN/GHC/Parser/Lexer/String.hs"
gen_alex "--latin1" "compiler/GHC/Parser/HaddockLex.x"   "$OUT_AUTOGEN/GHC/Parser/HaddockLex.hs"
gen_alex "--latin1" "compiler/GHC/Cmm/Lexer.x"           "$OUT_AUTOGEN/GHC/Cmm/Lexer.hs"
gen_alex ""         "utils/genprimopcode/Lexer.x"        "$OUT_AUTOGEN/genprimopcode/Lexer.hs"

echo "gen-parsers.sh: done -> $OUT_AUTOGEN"
