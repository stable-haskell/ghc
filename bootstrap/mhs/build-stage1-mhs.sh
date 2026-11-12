#!/usr/bin/env bash
# Compile the minimal stage1 closure with MicroHs (mhs 0.16.12).
#
# cabal-install itself is built by bootstrap/mhs/build-cabal-mhs.sh, which
# runs stable-haskell/cabal PR 533.  This script does not go through cabal:
# MicroHs is the compiler, and the stage1 packages are passed to mhs directly.
#
#   bootstrap/mhs/build-stage1-mhs.sh [tools|ghc-boot|compiler|all]
#
# Environment:
#   MHS_BIN     MicroHs compiler.  The cabal PR's toolchain (patched mhs)
#               is required for the larger modules.  Default: $HOME/.mcabal/bin/mhs
#   MHS_PREFIX  Package prefix.  Default: directory of MHS_BIN's install.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

MHS_BIN="${MHS_BIN:-$HOME/.mcabal/bin/mhs}"
if [ ! -x "$MHS_BIN" ]; then
  echo "mhs not found at $MHS_BIN" >&2
  echo "Build it with: bootstrap/mhs/build-cabal-mhs.sh toolchain" >&2
  exit 1
fi
export PATH="$(dirname "$MHS_BIN"):$HOME/.mcabal/bin:$PATH"

OUT="${MHS_STAGE1_OUT:-$ROOT/_build/mhs-stage1}"
mkdir -p "$OUT/bin" "$OUT/autogen/GHC/Platform" "$OUT/c" "$OUT/log"

HOST_ARCH="$(uname -m)"
case "$HOST_ARCH" in
  arm64|aarch64) NCG_DEF=(-DHAVE_AARCH64_NCG) ;;
  x86_64|amd64)  NCG_DEF=(-DHAVE_X86_NCG) ;;
  *) echo "unsupported host arch $HOST_ARCH" >&2; exit 1 ;;
esac

# Stage1 flags from cabal.project.stage1: bootstrap, host NCG only,
# no interpreter, no JS/Wasm/LLVM, no HPC.
DEFS=(-DBOOTSTRAPPING -DBOOTSTRAP_TH "${NCG_DEF[@]}")

PKGS=(
  -pcontainers-0.8.1
  -pfilepath-1.5.5.0
  -pos-string-2.0.10
  -pbinary-mhs-0.8.9.3
  -parray-mhs-0.5.8.0
  -ptime-1.15
  -pexceptions-0.10.11
  -ptransformers-0.6.2.0
  -pmtl-2.3.2
  -ppretty-mhs-1.1.3.6
)

INCLUDES=(
  -i"$ROOT/compiler"
  -i"$ROOT/ghc"
  -i"$ROOT/libraries/ghc-boot"
  -i"$ROOT/libraries/ghc-boot-th"
  -i"$ROOT/libraries/ghc-internal/src"
  -i"$ROOT/libraries/ghc-platform/src"
  -i"$ROOT/utils/deriveConstants"
  -i"$ROOT/utils/genapply"
  -i"$ROOT/utils/genprimopcode"
  -i"$OUT/autogen"
  -i"$ROOT/bootstrap/mhs/shims"
  -I"$ROOT/bootstrap/mhs/include"
  -I"$ROOT/compiler"
  -I"$ROOT/rts/include"
)

CACHE="$OUT/stage1.mhscache"

cat > "$OUT/autogen/GHC/Platform/Host.hs" << EOF
module GHC.Platform.Host (hostPlatformArchOS) where
import GHC.Platform.ArchOS
hostPlatformArchOS :: ArchOS
hostPlatformArchOS = ArchOS $(
  case "$HOST_ARCH" in
    arm64|aarch64) echo ArchAArch64 ;;
    *) echo ArchX86_64 ;;
  esac
) $(
  case "$(uname -s)" in
    Darwin) echo OSDarwin ;;
    Linux) echo OSLinux ;;
    FreeBSD) echo OSFreeBSD ;;
    *) echo OSUnknown ;;
  esac
)
EOF

cat > "$OUT/autogen/PackageInfo_ghc_boot.hs" << 'EOF'
module PackageInfo_ghc_boot (version) where
import Data.Version (Version(..))
version :: Version
version = Version [9,14,0] []
EOF

run_mhs() {
  local log="$1"
  shift
  # A later mhs process reloads every package already stored in the cache
  # and then adds each -p package again.  getPathPkgs does not dedup, so a
  # second link compiles the same cbits twice (time's HsTime.c).  -c runs
  # share the cache.  Each link gets its own.
  local cache="$CACHE"
  local arg linking=1
  for arg in "$@"; do
    if [ "$arg" = "-c" ]; then linking=0; fi
  done
  if [ "$linking" = 1 ]; then
    cache="${log%.log}.mhscache"
    rm -f "$cache"
  fi
  echo "=== mhs $* ==="
  if ! "$MHS_BIN" -s -XCPP "${DEFS[@]}" "${PKGS[@]}" "${INCLUDES[@]}" -C"$cache" "$@" > "$log" 2>&1; then
    echo "FAILED. Log: $log" >&2
    tail -40 "$log" >&2
    return 1
  fi
  tail -8 "$log"
}

build_tools() {
  echo "=== [tools] genapply ==="
  run_mhs "$OUT/log/genapply.log" -o "$OUT/bin/genapply" "$ROOT/utils/genapply/Main.hs"
  echo "=== [tools] deriveConstants ==="
  run_mhs "$OUT/log/deriveConstants.log" -o "$OUT/bin/deriveConstants" "$ROOT/utils/deriveConstants/Main.hs"
  ls -lh "$OUT/bin/genapply" "$OUT/bin/deriveConstants"
}

build_ghc_boot() {
  echo "=== [ghc-boot] package ==="
  # mhs accepts one module name and follows its imports.  A persistent
  # cache also stores every -p package again on each process, so the
  # cache file grows by a full copy of the packages per run.  One root
  # module typechecks the package in a single process.
  local root="$OUT/autogen/MhsGhcBoot.hs"
  cat > "$root" << 'EOF'
module MhsGhcBoot where
import GHC.Platform.ArchOS ()
import GHC.Platform.Host ()
import GHC.ForeignSrcLang ()
import GHC.LanguageExtensions ()
import GHC.Lexeme ()
import GHC.Serialized ()
import GHC.Version ()
import GHC.UniqueSubdir ()
import GHC.HandleEncoding ()
import GHC.Data.SizedSeq ()
import GHC.Utils.Encoding ()
import GHC.Data.ShortText ()
import GHC.Settings.Utils ()
import GHC.BaseDir ()
import GHC.Unit.Database ()
EOF
  rm -f "$CACHE"
  run_mhs "$OUT/log/ghc-boot.log" -c -o "$OUT/c/MhsGhcBoot.c" MhsGhcBoot
}

build_compiler() {
  echo "=== [compiler] module GHC (stage1 library root) ==="
  # This is the stage1 compiler library.  MicroHs still rejects unboxed
  # syntax and type families; the log records the first one of those.
  run_mhs "$OUT/log/compiler.log" -c -o "$OUT/compiler.c" GHC
}

cmd="${1:-all}"
case "$cmd" in
  tools) build_tools ;;
  ghc-boot) build_ghc_boot ;;
  compiler) build_compiler ;;
  all)
    build_tools
    build_ghc_boot
    build_compiler
    ;;
  *)
    echo "Usage: $0 [tools|ghc-boot|compiler|all]" >&2
    exit 1
    ;;
esac
