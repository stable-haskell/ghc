#!/usr/bin/env bash
# Build GHC stage1 artefacts with MicroHs (mhs 0.16.12.0).
#
#   bootstrap/mhs/build-stage1-mhs.sh [toolchain|tools|ghc-boot|compiler|ghc|test|all]
#
# Toolchain layout mirrors bootstrap/mhs/build-cabal-mhs.sh (stable-haskell/cabal
# PR 533): a patched MicroHs self-compiles, package discovery is automatic via
# MHSDIR `<Module>.txt` lookup files (so NO `-p` package flags are ever passed
# to mhs), every mhs invocation carries `+RTS -H700M -K32M -RTS`, and staged
# `.mhscache` checkpoints keep progress across the 700+ module GHC closure.
#
# All eight stage1 tools land in $OUT/bin, built natively without any
# bootstrap ghc, happy, or alex:
#   unlit            C, via cc (needs rts-fs/fs.h)
#   happy            Haskell, via mhs (stable-haskell/happy portable H2010)
#   alex             Haskell, via mhs (stable-haskell/alex portable H2010)
#   genapply         Haskell, via mhs
#   deriveConstants  Haskell, via mhs
#   genprimopcode    Haskell, via mhs (parsers via mhs-built happy/alex)
#   ghc-pkg          Haskell, via mhs (Cabal + Cabal-syntax from the
#                    sh/microhs-cabal-install branch + mhs-compat-shims)
#   hsc2hs           Haskell, via mhs (stable-haskell/hsc2hs feat/mhs-bootstrap)
#
# Environment:
#   MHS_BIN          MicroHs compiler. When explicitly set by the caller it is
#                    used as-is; otherwise $OUT/toolchain/bin/mhs if built,
#                    else $HOME/.mcabal/bin/mhs.
#   MHS_STAGE1_OUT   Output directory (default: $ROOT/_build/mhs-stage1).
#   MHS_FORCE_REBUILD=1  Rebuild stages whose checkpoints already exist.
#   MHS_FROM_STAGE   Resume staged compilation from this stage (e.g. 02-utils-data).
set -euo pipefail

# The self-hosted mhs is C. Its Haskell recursion uses the C stack.
# Phase 2 links it with -Wl,-z,stack-size=512MB, which macOS honors and
# Linux does not: the main thread still stops at RLIMIT_STACK (8MB on
# the GitHub runner). Stage 02a then dies with SIGSEGV, exit 139, and
# no RTS "stack overflow". Raise the soft limit before any mhs run.
if [ "$(uname -s)" = Linux ]; then
  ulimit -s unlimited 2>/dev/null || ulimit -s 1048576 || true
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

OUT="${MHS_STAGE1_OUT:-$ROOT/_build/mhs-stage1}"
TOOLCHAIN_DIR="$OUT/toolchain"
CACHE_DIR="$OUT/cache"
AUTOGEN="$OUT/autogen"
WORK_DIR="$OUT/work"
BIN_DIR="$OUT/bin"
LOG_DIR="$OUT/log"
SHARE_DIR="$OUT/share"
mkdir -p "$TOOLCHAIN_DIR/bin" "$CACHE_DIR" "$AUTOGEN" "$WORK_DIR" "$BIN_DIR" "$LOG_DIR" "$SHARE_DIR"

MHS_VERSION="0.16.12.0"
MHS_REPO_URL="${MHS_REPO_URL:-https://github.com/stable-haskell/MicroHs.git}"
MHS_COMMIT="${MHS_COMMIT:-29504a984d2779e8813e019cd29dc03b20f86ed2}"

# --- compiler selection -------------------------------------------------------
# Respect an explicit caller override; otherwise prefer the toolchain we built,
# falling back to the existing mcabal install (read-only use; this script never
# writes outside $OUT).
MHS_BIN_EXPLICIT=0
if [ -n "${MHS_BIN+x}" ]; then
  MHS_BIN_EXPLICIT=1
fi
if [ -z "${MHS_BIN+x}" ]; then
  if [ -x "$TOOLCHAIN_DIR/bin/mhs" ]; then
    MHS_BIN="$TOOLCHAIN_DIR/bin/mhs"
  else
    MHS_BIN="$HOME/.mcabal/bin/mhs"
  fi
fi
if [ ! -x "$MHS_BIN" ]; then
  echo "mhs not found at $MHS_BIN" >&2
  echo "Build it with: bootstrap/mhs/build-stage1-mhs.sh toolchain" >&2
  exit 1
fi
export PATH="$(dirname "$MHS_BIN"):$HOME/.mcabal/bin:$PATH"

# No bootstrap GHC, Happy, or Alex is used anywhere in this script: every tool
# is built natively with cc or $MHS_BIN.
HAPPY_REPO_URL="${HAPPY_REPO_URL:-https://github.com/stable-haskell/happy.git}"
HAPPY_COMMIT="${HAPPY_COMMIT:-6d2b06467694aeebc7b36e8f4960b6b688748dd2}"
ALEX_REPO_URL="${ALEX_REPO_URL:-https://github.com/stable-haskell/alex.git}"
ALEX_COMMIT="${ALEX_COMMIT:-d8b3768f64fe293fea3472c9bc5cccd462f53ac1}"

# --- host platform ------------------------------------------------------------
HOST_ARCH="$(uname -m)"
case "$HOST_ARCH" in
  arm64|aarch64) NCG_DEF=(-DHAVE_AARCH64_NCG); MHS_ARCH=ArchAArch64 ;;
  x86_64|amd64)  NCG_DEF=(-DHAVE_X86_NCG);    MHS_ARCH=ArchX86_64 ;;
  *) echo "unsupported host arch $HOST_ARCH" >&2; exit 1 ;;
esac
case "$(uname -s)" in
  Darwin)  MHS_OS=OSDarwin ;;
  Linux)   MHS_OS=OSLinux ;;
  FreeBSD) MHS_OS=OSFreeBSD ;;
  *)       echo "unsupported host OS $(uname -s)" >&2; exit 1 ;;
esac

# Stage1 flags from cabal.project.stage1: bootstrap, host NCG only, no
# interpreter, no JS/Wasm/LLVM/LLVM, no HPC. -D__MHS__ must be passed
# explicitly: mhs does NOT predefine it, and every compiler fix in this
# tree is guarded on `#if defined(__MHS__)` (verified: without it, mhs
# elaborates the `#else`/original branches, so no fix takes effect).
MHS_DEFS=(-DBOOTSTRAPPING -DBOOTSTRAP_TH -D__MHS__ "${NCG_DEF[@]}")
MHS_RTS=(+RTS -H"${MHS_HEAP:-700M}" -K64M -RTS)

MHS_INCLUDES=(
  -i"$ROOT/compiler"
  -i"$ROOT/ghc"
  -i"$ROOT/libraries/ghc-boot"
  -i"$ROOT/libraries/ghc-boot-th"
  -i"$ROOT/libraries/ghc-internal/src"
  -i"$ROOT/libraries/ghc-platform/src"
  -i"$ROOT/utils/deriveConstants"
  -i"$ROOT/utils/genapply"
  -i"$ROOT/utils/genprimopcode"
  -i"$AUTOGEN"
  -i"$ROOT/bootstrap/mhs/shims"
  -I"$ROOT/bootstrap/mhs/include"
  -I"$AUTOGEN/include"
  -I"$ROOT/compiler"
  -I"$ROOT/rts/include"
)

# mhs has no -XNoImplicitPrelude switch; probe once and use it only if accepted.
# macOS /bin/bash is 3.2: "${NIP_FLAG[@]}" with set -u is an error when the
# array is empty (stage 00 clears it). Test the length, or expand under set +u.
NIP_FLAG=()
probe_nip_flag() {
  NIP_FLAG=()
  if "$MHS_BIN" -XNoImplicitPrelude --numeric-version >/dev/null 2>&1; then
    NIP_FLAG=(-XNoImplicitPrelude)
  else
    echo "NOTE: mhs rejects -XNoImplicitPrelude; staged modules rely on their"
    echo "explicit Prelude/GHC.Prelude imports instead (ghc-boot+compiler already"
    echo "carry default-extensions: NoImplicitPrelude in their .cabal files)."
  fi
}
probe_nip_flag

# After build_toolchain runs inside `all`, re-point MHS_BIN at the freshly
# built toolchain (unless the caller explicitly passed MHS_BIN=...) and
# re-probe NIP_FLAG: the top-of-script probe above ran before the toolchain
# existed and would otherwise pin `all` to ~/.mcabal/bin/mhs with NIP_FLAG=().
refresh_mhs_bin_after_toolchain() {
  if [ "$MHS_BIN_EXPLICIT" != "1" ] && [ -x "$TOOLCHAIN_DIR/bin/mhs" ]; then
    MHS_BIN="$TOOLCHAIN_DIR/bin/mhs"
    export PATH="$(dirname "$MHS_BIN"):$PATH"
    probe_nip_flag
  fi
}

# Overwrite a file only when its content changes, so .mhscache MD5s stay valid.
write_if_changed() {
  local dest="$1"
  local tmp="$dest.tmp.$$"
  mkdir -p "$(dirname "$dest")"
  cat > "$tmp"
  if [ -f "$dest" ] && cmp -s "$dest" "$tmp"; then
    rm -f "$tmp"
  else
    mv "$tmp" "$dest"
  fi
}

# --- toolchain ----------------------------------------------------------------
# Patched MicroHs 0.16.12.0, following build-cabal-mhs.sh: apply the stage1
# patch (falling back to the cabal checkout's microhs-full.patch), self-compile
# MicroHs.Main, relink with the C runtime, and point MHS_PREFIX at it.
# Everything is installed under $OUT/toolchain; $HOME/.mcabal is only read.
build_toolchain() {
  local mhs_prefix="$TOOLCHAIN_DIR"
  local mhs_bin="$mhs_prefix/bin/mhs"
  local stamp="$mhs_prefix/.mhs-stage1-toolchain-v1.stamp"
  if [ -x "$mhs_bin" ] && [ -f "$stamp" ] && [ "${MHS_FORCE_REBUILD:-0}" != "1" ]; then
    echo "=== [toolchain] Reusing MicroHs $MHS_VERSION in $mhs_prefix ==="
    "$mhs_bin" --version
    return 0
  fi
  # NOTE: $HOME/.mcabal/bin/mhs is the UNPATCHED upstream 0.16.12.0 and must
  # never satisfy the toolchain: always build $TOOLCHAIN_DIR/bin/mhs when it
  # (or its stamp) is missing.

  local mhs_src="${MHS_SRC:-}"
  local owns_mhs_src=0
  if [ -z "$mhs_src" ]; then
    mhs_src="$OUT/microhs-src"
    owns_mhs_src=1
  fi
  echo "=== [toolchain] Building MicroHs $MHS_VERSION ($MHS_COMMIT) ==="
  if [ ! -d "$mhs_src/.git" ] && [ ! -f "$mhs_src/MicroHs.cabal" ]; then
    rm -rf "$mhs_src"
    git clone --filter=blob:none "$MHS_REPO_URL" "$mhs_src"
  fi
  if [ -d "$mhs_src/.git" ]; then
    if [ "$owns_mhs_src" = "1" ]; then
      git -C "$mhs_src" remote set-url origin "$MHS_REPO_URL"
      git -C "$mhs_src" checkout -- . 2>/dev/null || true
      git -C "$mhs_src" clean -fd 2>/dev/null || true
      git -C "$mhs_src" fetch origin "$MHS_COMMIT" 2>/dev/null || true
      git -C "$mhs_src" checkout "$MHS_COMMIT"
    fi
  fi

  # 1. Self-compile MicroHs.Main to C with a stage-0 mhs (seed bin/mhs,
  #    ~/.mcabal/bin/mhs, $STAGE0_MHS, or mhs on PATH). Never `make install`
  #    (that would recompile every package / copy everything).
  echo "=== [toolchain] Self-compiling MicroHs.Main ==="
  (
    cd "$mhs_src"
    local stage0=""
    if [ -n "${STAGE0_MHS:-}" ] && [ -x "$STAGE0_MHS" ]; then
      stage0="$STAGE0_MHS"
    elif [ -x "$mhs_src/bin/mhs" ]; then
      stage0="$mhs_src/bin/mhs"
    elif [ -x "$HOME/.mcabal/bin/mhs" ]; then
      stage0="$HOME/.mcabal/bin/mhs"
    elif [ -n "$(command -v mhs || true)" ]; then
      stage0="$(command -v mhs)"
    fi
    if [ -z "$stage0" ]; then
      echo "ERROR: no stage-0 mhs (set STAGE0_MHS or install ~/.mcabal/bin/mhs)" >&2
      exit 1
    fi
    # Set MHSDIR=. so stage-0 mhs compiles patched src/ and lib/ from source
    # instead of picking up unpatched MicroHs-0.16.12.0.pkg from ~/.mcabal.
    rm -f "$WORK_DIR/mhs-toolchain.mhscache"
    MHSDIR=. "$stage0" "${MHS_RTS[@]}" -C"$WORK_DIR/mhs-toolchain.mhscache" -z -i -imhs -isrc -ilib MicroHs.Main -ogenerated/mhs-patched.c
    mkdir -p "$mhs_prefix/bin"
    local stack_ldflags=()
    case "$(uname -s)" in
      Darwin) stack_ldflags=(-Wl,-stack_size,0x20000000) ;;
      Linux)  stack_ldflags=(-Wl,-z,stack-size=536870912) ;;
    esac
    cc -O3 "${stack_ldflags[@]}" -Isrc/runtime -Isrc/runtime/unix \
      src/runtime/main.c src/runtime/eval.c generated/mhs-patched.c \
      -lm -o "$mhs_bin"
  )
  [ -x "$mhs_bin" ] || { echo "ERROR: toolchain link produced no $mhs_bin" >&2; exit 1; }
  "$mhs_bin" --version

  # 2. Zero-bloat package access: MicroHs resolves MHSDIR relative to
  #    <bindir>/../mhs-0.16.12.0, so symlink the toolchain's package dir at
  #    the ~/.mcabal packages (read-only use) plus cpphs next to mhs
  #    (mhs shells out to `cpphs` for -XCPP).
  mkdir -p "$mhs_prefix/bin"
  rm -rf "$mhs_prefix/mhs-$MHS_VERSION"
  mkdir -p "$mhs_prefix/mhs-$MHS_VERSION/src"
  if [ -d "$HOME/.mcabal/mhs-$MHS_VERSION" ]; then
    for entry in "$HOME/.mcabal/mhs-$MHS_VERSION"/*; do
      [ "$(basename "$entry")" = "src" ] && continue
      ln -sfn "$entry" "$mhs_prefix/mhs-$MHS_VERSION/$(basename "$entry")"
    done
  fi
  cp -R "$mhs_src/src/runtime" "$mhs_prefix/mhs-$MHS_VERSION/src/runtime"
  # Installed-mode mhs links <bindir>/../mhs-VERSION/packages/MicroHs-VERSION/data/src/runtime,
  # so overlay the patched runtime there too (packages/ itself stays shared read-only).
  if [ -d "$HOME/.mcabal/mhs-$MHS_VERSION/packages" ]; then
    rm -f "$mhs_prefix/mhs-$MHS_VERSION/packages"
    mkdir -p "$mhs_prefix/mhs-$MHS_VERSION/packages"
    for entry in "$HOME/.mcabal/mhs-$MHS_VERSION"/packages/*; do
      [ "$(basename "$entry")" = "MicroHs-$MHS_VERSION" ] && continue
      ln -sfn "$entry" "$mhs_prefix/mhs-$MHS_VERSION/packages/$(basename "$entry")"
    done
    mkdir -p "$mhs_prefix/mhs-$MHS_VERSION/packages/MicroHs-$MHS_VERSION/data/src"
    ln -sfn "$HOME/.mcabal/mhs-$MHS_VERSION/packages/MicroHs-$MHS_VERSION/data/mhs.conf" \
      "$mhs_prefix/mhs-$MHS_VERSION/packages/MicroHs-$MHS_VERSION/data/mhs.conf"
    rm -rf "$mhs_prefix/mhs-$MHS_VERSION/packages/MicroHs-$MHS_VERSION/data/src/runtime"
    cp -R "$mhs_src/src/runtime" \
      "$mhs_prefix/mhs-$MHS_VERSION/packages/MicroHs-$MHS_VERSION/data/src/runtime"
  fi
  if [ ! -x "$mhs_prefix/bin/cpphs" ] && [ -x "$HOME/.mcabal/bin/cpphs" ]; then
    ln -sfn "$HOME/.mcabal/bin/cpphs" "$mhs_prefix/bin/cpphs"
  fi
  touch "$stamp"
  ls -lh "$mhs_bin"
}

# run_logged <log> <cmd...>: run with output to log; report log on failure.
run_logged() {
  local log="$1"; shift
  local rc=0
  "$@" > "$log" 2>&1 || rc=$?
  if [ $rc -ne 0 ]; then
    # mhs reports the cc invocation and then "command failed". The
    # compiler or linker diagnostic is above that trailer, so tail -3
    # used to hide it (ghc-pkg's missing -lcrypto showed up only as
    # "collect2: error: ld returned 1").
    grep -nE 'error:|undefined reference|fatal error|multiple definition|No such file' "$log" | head -40 || true
    tail -5 "$log" || true
    echo "FAILED ($rc): $* -- log: $log" >&2
    return $rc
  fi
  tail -3 "$log" || true
}

install_mhs_tool_wrapper() {
  local name="$1" heap="$2"
  cat > "$BIN_DIR/$name" <<EOF
#!/usr/bin/env sh
case " \$* " in
  *" +RTS "*) exec "\$(dirname "\$0")/$name.bin" "\$@" ;;
  *) exec "\$(dirname "\$0")/$name.bin" +RTS -H"\${MHS_TOOL_HEAP:-$heap}" -K64M -RTS "\$@" ;;
esac
EOF
  chmod +x "$BIN_DIR/$name"
}

build_unlit() {
  if [ -x "$BIN_DIR/unlit" ] && [ "${MHS_FORCE_REBUILD:-0}" != "1" ]; then
    return 0
  fi
  echo "=== [tools] unlit (cc) ==="
  # rts-fs/fs.c provides __rts_fopen (plain fopen on POSIX, see rts-fs.cabal).
  cc -O2 -DFS_NAMESPACE=rts -I"$ROOT/rts-fs" \
    -o "$BIN_DIR/unlit" "$ROOT/utils/unlit/unlit.c" "$ROOT/rts-fs/fs.c"
  printf 'hello\n\n> main = putStrLn "hi"\n' > "$WORK_DIR/unlit-probe.lhs"
  "$BIN_DIR/unlit" -q "$WORK_DIR/unlit-probe.lhs" "$WORK_DIR/unlit-probe.hs"
  grep -q 'main = putStrLn' "$WORK_DIR/unlit-probe.hs"
  echo "unlit OK"
}

build_happy_alex() {
  build_unlit

  if [ ! -x "$BIN_DIR/happy" ] || [ "${MHS_FORCE_REBUILD:-0}" = "1" ]; then
    echo "=== [tools] happy (mhs, $HAPPY_COMMIT) ==="
    local happy_src="${HAPPY_SRC:-$OUT/happy-src}"
    if [ ! -d "$happy_src/.git" ]; then
      rm -rf "$happy_src"
      git clone --filter=blob:none "$HAPPY_REPO_URL" "$happy_src"
    fi
    git -C "$happy_src" remote set-url origin "$HAPPY_REPO_URL"
    git -C "$happy_src" fetch origin "$HAPPY_COMMIT" 2>/dev/null || true
    git -C "$happy_src" checkout -- . 2>/dev/null || true
    git -C "$happy_src" checkout "$HAPPY_COMMIT"

    mkdir -p "$SHARE_DIR/happy"
    cp "$happy_src"/lib/data/* "$SHARE_DIR/happy/"

    local happy_shadow="$WORK_DIR/happy-build-src"
    rm -rf "$happy_shadow"
    mkdir -p "$happy_shadow"
    local sroot src_file rel dst_file
    for sroot in lib/grammar/src lib/tabular/src lib/frontend/src lib/backend-lalr/src lib/backend-glr/src app; do
      while IFS= read -r -d '' src_file; do
        rel="${src_file#"$happy_src"/}"
        case "$rel" in
          *.lhs)
            dst_file="$happy_shadow/${rel%.lhs}.hs"
            mkdir -p "$(dirname "$dst_file")"
            "$BIN_DIR/unlit" -q "$src_file" "$dst_file"
            ;;
          *.hs)
            dst_file="$happy_shadow/$rel"
            mkdir -p "$(dirname "$dst_file")"
            cp "$src_file" "$dst_file"
            ;;
        esac
      done < <(find "$happy_src/$sroot" -type f \( -name '*.lhs' -o -name '*.hs' \) -print0)
    done

    local happy_stubs="$WORK_DIR/happy-stubs"
    mkdir -p "$happy_stubs/Happy"
    write_if_changed "$happy_stubs/Happy/Paths.hs" <<EOF
module Happy.Paths where
import Data.Version (Version(..))
import System.Environment (lookupEnv)
version :: Version
version = Version [2,2,1] []
getDataDir :: IO String
getDataDir = do
  m <- lookupEnv "HAPPY_DATADIR"
  case m of
    Just d  -> return d
    Nothing -> return "$SHARE_DIR/happy"
EOF
    write_if_changed "$happy_stubs/Paths_happy.hs" <<EOF
module Paths_happy where
import Data.Version (Version(..))
import System.Environment (lookupEnv)
version :: Version
version = Version [2,2,1] []
getDataDir :: IO String
getDataDir = do
  m <- lookupEnv "HAPPY_DATADIR"
  case m of
    Just d  -> return d
    Nothing -> return "$SHARE_DIR/happy"
EOF

    run_logged "$LOG_DIR/happy.log" \
      "$MHS_BIN" "${MHS_RTS[@]}" -z -s \
      -C"$WORK_DIR/happy.mhscache" \
      -i"$happy_stubs" \
      -i"$happy_shadow/lib/grammar/src" \
      -i"$happy_shadow/lib/tabular/src" \
      -i"$happy_shadow/lib/frontend/src" \
      -i"$happy_shadow/lib/backend-lalr/src" \
      -i"$happy_shadow/lib/backend-glr/src" \
      -i"$happy_shadow/app" \
      Main -o "$BIN_DIR/happy.bin"
    install_mhs_tool_wrapper happy 500M
    "$BIN_DIR/happy" --version
    echo "happy OK"
  fi

  if [ ! -x "$BIN_DIR/alex" ] || [ "${MHS_FORCE_REBUILD:-0}" = "1" ]; then
    echo "=== [tools] alex (mhs, $ALEX_COMMIT) ==="
    local alex_src="${ALEX_SRC:-$OUT/alex-src}"
    if [ ! -d "$alex_src/.git" ]; then
      rm -rf "$alex_src"
      git clone --filter=blob:none "$ALEX_REPO_URL" "$alex_src"
    fi
    git -C "$alex_src" remote set-url origin "$ALEX_REPO_URL"
    git -C "$alex_src" fetch origin "$ALEX_COMMIT" 2>/dev/null || true
    git -C "$alex_src" checkout -- . 2>/dev/null || true
    git -C "$alex_src" checkout "$ALEX_COMMIT"

    mkdir -p "$SHARE_DIR/alex"
    cp "$alex_src"/data/* "$SHARE_DIR/alex/"

    local alex_stubs="$WORK_DIR/alex-stubs"
    mkdir -p "$alex_stubs"
    write_if_changed "$alex_stubs/Paths_alex.hs" <<EOF
module Paths_alex where
import Data.Version (Version(..))
import System.Environment (lookupEnv)
version :: Version
version = Version [3,5,4,2] []
getDataDir :: IO String
getDataDir = do
  m <- lookupEnv "ALEX_DATADIR"
  case m of
    Just d  -> return d
    Nothing -> return "$SHARE_DIR/alex"
EOF

    local alex_boot="$WORK_DIR/alex-boot"
    mkdir -p "$alex_boot"
    "$BIN_DIR/happy" "$alex_src/src/Parser.y" -o "$alex_boot/Parser.hs"
    cp "$alex_src/boot/Scan.hs" "$alex_boot/Scan.hs"

    run_logged "$LOG_DIR/alex.log" \
      "$MHS_BIN" "${MHS_RTS[@]}" -z -s \
      -C"$WORK_DIR/alex.mhscache" \
      -i"$alex_stubs" \
      -i"$alex_src/src" \
      -i"$alex_boot" \
      Main -o "$BIN_DIR/alex.bin"
    install_mhs_tool_wrapper alex 500M
    "$BIN_DIR/alex" --version
    echo "alex OK"
  fi
}

# --- autogen ------------------------------------------------------------------
# Idempotent autogen for ghc-boot/compiler/ghc (mirrors hadrian
# generateVersionHs/generatePlatformHostHs and compiler/cabal-buck2
# gen_settings_config.py; every identifier below is imported somewhere under
# compiler/ or ghc/). Fingerprint needs no autogen: GHC.Utils.Fingerprint is
# plain Haskell. The primop .hs-incl files already live in
# bootstrap/mhs/include/ (committed by an earlier stage1 change).
prepare_autogen() {
  echo "=== [autogen] $AUTOGEN ==="
  write_if_changed "$AUTOGEN/GHC/Platform/Host.hs" <<EOF
module GHC.Platform.Host (hostPlatformArchOS) where
import GHC.Platform.ArchOS
hostPlatformArchOS :: ArchOS
hostPlatformArchOS = ArchOS $MHS_ARCH $MHS_OS
EOF

  write_if_changed "$AUTOGEN/PackageInfo_ghc_boot.hs" <<'EOF'
module PackageInfo_ghc_boot (version) where
import Data.Version (Version(..))
version :: Version
version = Version [9,14,0] []
EOF

  # GHC.CmmToLlvm.Version.Bounds (mirrors hadrian's interpolation of
  # compiler/GHC/CmmToLlvm/Version/Bounds.hs.in; bounds per configure.ac:
  # LlvmMinVersion=10 inclusive, LlvmMaxVersion=100 exclusive).
  mkdir -p "$AUTOGEN/GHC/CmmToLlvm/Version"
  write_if_changed "$AUTOGEN/GHC/CmmToLlvm/Version/Bounds.hs" <<'EOF'
module GHC.CmmToLlvm.Version.Bounds
  ( supportedLlvmVersionLowerBound
  , supportedLlvmVersionUpperBound
  )
where

import GHC.Prelude ()

import GHC.CmmToLlvm.Version.Type

import qualified Data.List.NonEmpty as NE

-- | The (inclusive) lower bound on the LLVM Version that is currently supported.
supportedLlvmVersionLowerBound :: LlvmVersion
supportedLlvmVersionLowerBound = LlvmVersion (10 NE.:| [])

-- | The (not-inclusive) upper bound  bound on the LLVM Version that is currently supported.
supportedLlvmVersionUpperBound :: LlvmVersion
supportedLlvmVersionUpperBound = LlvmVersion (100 NE.:| [])
EOF

  # GHC.Platform.Constants: generated by `deriveConstants
  # --gen-haskell-type` (pure Haskell, no C compilation needed), the same
  # command hadrian's genPlatformConstantsType runs. Build the tool when it
  # is missing so this also works for a standalone `compiler` run (which
  # never runs build_tools); share build_tools' cache so no work repeats.
  gen_platform_constants() {
    mkdir -p "$AUTOGEN/GHC/Platform"
    local dc="$BIN_DIR/deriveConstants"
    if [ ! -x "$dc" ]; then
      echo "--- building deriveConstants for autogen ---"
      "$MHS_BIN" "${MHS_RTS[@]}" -s -XCPP "${MHS_DEFS[@]}" "${MHS_INCLUDES[@]}" \
        -C"$WORK_DIR/deriveConstants.mhscache" \
        -o "$dc" "$ROOT/utils/deriveConstants/Main.hs"
    fi
    "$dc" --gen-haskell-type -o "$WORK_DIR/Constants.hs.tmp"
    write_if_changed "$AUTOGEN/GHC/Platform/Constants.hs" < "$WORK_DIR/Constants.hs.tmp"
    rm -f "$WORK_DIR/Constants.hs.tmp"
  }
  gen_platform_constants

  # Values mirror gen_settings_config.py, derived without any bootstrap
  # compiler: the target triple comes from the same $MHS_ARCH-$MHS_OS
  # table as build_ghc, and cStage is hard-coded to 2 (as in
  # compiler/Setup.hs).
  local build_platform host_platform project_name booter_version
  case "$MHS_ARCH-$MHS_OS" in
    ArchAArch64-OSDarwin)
      build_platform="aarch64-apple-darwin"; host_platform="aarch64-apple-darwin" ;;
    ArchX86_64-OSDarwin)
      build_platform="x86_64-apple-darwin"; host_platform="x86_64-apple-darwin" ;;
    ArchAArch64-OSLinux)
      build_platform="aarch64-unknown-linux"; host_platform="aarch64-unknown-linux" ;;
    ArchAArch64-OSFreeBSD)
      build_platform="aarch64-unknown-freebsd"; host_platform="aarch64-unknown-freebsd" ;;
    ArchX86_64-OSFreeBSD)
      build_platform="x86_64-unknown-freebsd"; host_platform="x86_64-unknown-freebsd" ;;
    *)
      build_platform="x86_64-unknown-linux"; host_platform="x86_64-unknown-linux" ;;
  esac
  project_name="The Glorious Glasgow Haskell Compilation System"
  booter_version="$("$MHS_BIN" --numeric-version 2>/dev/null || echo 0.16.12.0)"
  local ghc_internal_unit_id="ghc-internal"
  # Escape for Haskell string literals.
  build_platform="${build_platform//\\/\\\\}"; build_platform="${build_platform//\"/\\\"}"
  host_platform="${host_platform//\\/\\\\}"; host_platform="${host_platform//\"/\\\"}"
  write_if_changed "$AUTOGEN/GHC/Settings/Config.hs" <<EOF
module GHC.Settings.Config
  ( module GHC.Version
  , cBuildPlatformString
  , cHostPlatformString
  , cProjectName
  , cBooterVersion
  , cStage
  , cProjectUnitId
  , cGhcInternalUnitId
  ) where

import GHC.Prelude.Basic

import GHC.Version

cBuildPlatformString :: String
cBuildPlatformString = "$build_platform"

cHostPlatformString :: String
cHostPlatformString = "$host_platform"

cProjectName          :: String
cProjectName          = "$project_name"

cBooterVersion        :: String
cBooterVersion        = "$booter_version"

cStage                :: String
cStage                = show (2 :: Int)

cProjectUnitId :: String
cProjectUnitId = "ghc"

cGhcInternalUnitId :: String
cGhcInternalUnitId = "$ghc_internal_unit_id"
EOF

  # Paths_hsc2hs for the out-of-tree hsc2hs (cabal generates this).
  mkdir -p "$SHARE_DIR/hsc2hs"
  write_if_changed "$AUTOGEN/Paths_hsc2hs.hs" <<EOF
module Paths_hsc2hs (version, getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir, getDataFileName) where
import Data.Version (Version(..))
version :: Version
version = Version [0,68,10] []
getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = return "$BIN_DIR"
getLibDir = return "$SHARE_DIR/hsc2hs"
getDynLibDir = return "$SHARE_DIR/hsc2hs"
getDataDir = return "$SHARE_DIR/hsc2hs"
getLibexecDir = return "$SHARE_DIR/hsc2hs"
getSysconfDir = return "$SHARE_DIR/hsc2hs"
getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return ("$SHARE_DIR/hsc2hs/" ++ name)
EOF

  # ghcplatform.h is generated by configure in a full build; the bootstrap
  # provides a stub (feature macros arrive via -D flags instead).
  mkdir -p "$AUTOGEN/include"
  write_if_changed "$AUTOGEN/include/ghcplatform.h" <<'EOF'
/* Stub for the MicroHs stage1 bootstrap. Feature macros are passed with -D. */
#ifndef GHCPLATFORM_H
#define GHCPLATFORM_H
#endif
EOF

  build_happy_alex
  if [ -x "$ROOT/bootstrap/mhs/gen-parsers.sh" ]; then
    echo "--- gen-parsers.sh $AUTOGEN ---"
    HAPPY_BIN="$BIN_DIR/happy" ALEX_BIN="$BIN_DIR/alex" \
      "$ROOT/bootstrap/mhs/gen-parsers.sh" "$AUTOGEN"
  else
    echo "--- no bootstrap/mhs/gen-parsers.sh; tools generate parsers inline ---"
  fi
}

# --- ghc-pkg -------------------------------------------------------------------
# ghc-pkg needs Cabal + Cabal-syntax, which have no mhs packages: build it
# natively with mhs from the sh/microhs-cabal-install branch plus
# mhs-compat-shims. Called by build_tools (and by build_ghc if the tool
# does not exist yet).
build_ghc_pkg() {
  if [ -x "$BIN_DIR/ghc-pkg" ] && [ "${MHS_FORCE_REBUILD:-0}" != "1" ]; then
    echo "=== [ghc-pkg] Reusing $BIN_DIR/ghc-pkg ==="
    return 0
  fi
  echo "=== [ghc-pkg] (mhs) ==="
  local cabal_mhs_src="${CABAL_MHS_SRC:-$OUT/cabal-mhs-src}"
  if [ -d "$ROOT/_build/mhs-cabal-src/.git" ]; then
    cabal_mhs_src="$ROOT/_build/mhs-cabal-src"
  fi
  if [ ! -d "$cabal_mhs_src/Cabal" ] || [ ! -d "$cabal_mhs_src/Cabal-syntax" ]; then
    CABAL_MHS_REPO="${CABAL_MHS_REPO:-https://github.com/stable-haskell/cabal.git}"
    CABAL_MHS_REF="${CABAL_MHS_REF:-sh/microhs-cabal-install}"
    echo "--- cloning $CABAL_MHS_REPO ($CABAL_MHS_REF) ---"
    rm -rf "$cabal_mhs_src"
    git clone --branch "$CABAL_MHS_REF" --single-branch "$CABAL_MHS_REPO" "$cabal_mhs_src"
  fi
  if [ ! -d "$cabal_mhs_src/mhs-compat-shims/src" ]; then
    echo "ERROR: $cabal_mhs_src has no mhs-compat-shims/src (need ${CABAL_MHS_REF:-sh/microhs-cabal-install})" >&2
    exit 1
  fi
  install_binary_shim_files "$cabal_mhs_src"
  # Paths_Cabal (version only; Distribution.Simple.Utils imports it).
  local ghc_pkg_autogen="$WORK_DIR/ghc-pkg-autogen"
  local cabal_version
  cabal_version="$(grep -m1 '^version:' "$cabal_mhs_src/Cabal/Cabal.cabal" | awk '{print $2}')"
  [ -n "$cabal_version" ] || { echo "ERROR: no version in Cabal.cabal" >&2; exit 1; }
  mkdir -p "$ghc_pkg_autogen"
  write_if_changed "$ghc_pkg_autogen/Paths_Cabal.hs" <<EOF
module Paths_Cabal (version) where
import Data.Version (Version(..))
version :: Version
version = Version [$(echo "$cabal_version" | tr '.' ',')] []
EOF
  # Stage 1 (implicit Prelude): compile Cabal-syntax, Cabal modules used by
  # ghc-pkg, mhs-compat-shims, System.Posix, and installed GHC.Fingerprint
  # into ghc-pkg.mhscache WITHOUT -XNoImplicitPrelude (Cabal and
  # mhs-compat-shims rely on implicit Prelude).
  # Stage 2 (-XNoImplicitPrelude): compile libraries/ghc-boot and
  # utils/ghc-pkg/Main.hs on top of that cache.
  rm -f "$WORK_DIR/ghc-pkg.mhscache"
  cat > "$WORK_DIR/GhcPkgSeed.hs" <<'EOF'
module GhcPkgSeed where
import GHC.Fingerprint (Fingerprint(..))
import qualified Distribution.Simple.PackageIndex
import qualified Distribution.Simple.Utils
import qualified Distribution.InstalledPackageInfo
import qualified Distribution.Parsec
import qualified Distribution.Package
import qualified Distribution.Text
import qualified Distribution.Version
import qualified Distribution.Backpack
import qualified Distribution.Pretty
import qualified Distribution.Types.UnqualComponentName
import qualified Distribution.Types.LibraryName
import qualified Distribution.Types.MungedPackageName
import qualified Distribution.Types.MungedPackageId
import qualified System.Posix
import qualified MhsTool.Directory
import qualified System.Directory.MTime
import qualified Data.Binary
import qualified Data.Binary.Get
import qualified Data.Binary.Put
ghcPkgSeed :: Fingerprint
ghcPkgSeed = Fingerprint 0 0
EOF
  run_logged "$LOG_DIR/ghc-pkg-seed.log" \
    "$MHS_BIN" "${MHS_RTS[@]}" -v -s "${MHS_DEFS[@]}" \
    -i"$WORK_DIR" \
    -i"$ROOT/bootstrap/mhs/shims" \
    -i"$ROOT/bootstrap/mhs/tool-shims" \
    -i"$ghc_pkg_autogen" \
    -i"$cabal_mhs_src/Cabal-syntax/src" \
    -i"$cabal_mhs_src/Cabal/src" \
    -i"$cabal_mhs_src/mhs-compat-shims/src" \
    -I"$cabal_mhs_src/Cabal-syntax/src" \
    -I"$cabal_mhs_src/Cabal/src" \
    -I"$cabal_mhs_src/mhs-compat-shims/cbits" \
    -I"$ROOT/bootstrap/mhs/cbits" \
    -DBOOTSTRAPPED_CABAL=1 \
    -C"$WORK_DIR/ghc-pkg.mhscache" \
    -fno-code GhcPkgSeed
  # mhs-compat-shims/cbits/mhs_sha256.c calls OpenSSL SHA256 on every
  # OS other than macOS (CommonCrypto, in libSystem). The default mhs
  # target only appends -lm, and GNU ld does not resolve a -l that
  # appears before the objects. The "environment" target puts cclibs
  # at the end of the cc command and expands $MHSCCLIBS in the shell,
  # which is where -lcrypto has to sit. macOS must not take -lcrypto:
  # a nix-provided libcrypto would be baked into the binary.
  local cclibs=" -lm"
  if [ "$MHS_OS" != OSDarwin ]; then
    cclibs=" -lm -lcrypto"
  fi
  set +u
  run_logged "$LOG_DIR/ghc-pkg.log" \
    env CC=cc MHSCCFLAGS='-w -Wall -O3 ' MHSCCLIBS="$cclibs" MHSCONF=unix \
    "$MHS_BIN" "${MHS_RTS[@]}" -z -s -XCPP -tenvironment "${MHS_DEFS[@]}" "${NIP_FLAG[@]}" \
    -i"$ROOT/libraries/ghc-boot" -i"$ROOT/libraries/ghc-boot-th" \
    -i"$ROOT/libraries/ghc-platform/src" \
    -i"$AUTOGEN" -i"$ROOT/bootstrap/mhs/shims" \
    -i"$ROOT/bootstrap/mhs/tool-shims" \
    -i"$ghc_pkg_autogen" \
    -i"$cabal_mhs_src/Cabal-syntax/src" \
    -i"$cabal_mhs_src/Cabal/src" \
    -i"$cabal_mhs_src/mhs-compat-shims/src" \
    -I"$AUTOGEN/include" -I"$ROOT/compiler" \
    -I"$ROOT/bootstrap/mhs/include" \
    -I"$cabal_mhs_src/Cabal-syntax/src" \
    -I"$cabal_mhs_src/Cabal/src" \
    -I"$cabal_mhs_src/mhs-compat-shims/cbits" \
    -I"$ROOT/bootstrap/mhs/cbits" \
    -DBOOTSTRAPPED_CABAL=1 -DBOOTSTRAPPING \
    -optc "-include$ROOT/bootstrap/mhs/cbits/mhs_tool_posix.h" \
    -C"$WORK_DIR/ghc-pkg.mhscache" \
    -o "$BIN_DIR/ghc-pkg.bin" "$ROOT/utils/ghc-pkg/Main.hs" \
    "$ROOT/bootstrap/mhs/cbits/mhs_tool_posix.c" \
    "$ROOT/bootstrap/mhs/cbits/mhs_posix.c" \
    "$cabal_mhs_src/mhs-compat-shims/cbits/mhs_spawn.c" \
    "$cabal_mhs_src/mhs-compat-shims/cbits/mhs_posix.c" \
    "$cabal_mhs_src/mhs-compat-shims/cbits/mhs_handle.c" \
    "$cabal_mhs_src/mhs-compat-shims/cbits/mhs_sha256.c"
  set -u
  install_mhs_tool_wrapper ghc-pkg 300M
  "$BIN_DIR/ghc-pkg" --version
  echo "ghc-pkg OK"
}

# --- stage1 tools ---------------------------------------------------------------
# Builds all eight utils into $BIN_DIR and verifies each one runs.
build_tools() {
  build_unlit
  build_happy_alex

  if [ ! -x "$BIN_DIR/genapply" ] || [ "${MHS_FORCE_REBUILD:-0}" = "1" ]; then
    echo "=== [tools] genapply (mhs) ==="
    run_logged "$LOG_DIR/genapply.log" \
      "$MHS_BIN" "${MHS_RTS[@]}" -z -s -XCPP "${MHS_DEFS[@]}" "${MHS_INCLUDES[@]}" \
      -C"$WORK_DIR/genapply.mhscache" \
      -o "$BIN_DIR/genapply" "$ROOT/utils/genapply/Main.hs"
    # Functional check: generate AutoApply from a synthetic target-info header.
    cat > "$WORK_DIR/target-info.h" <<'EOF'
// WORD_SIZE 8
// TAG_BITS 1
// BITMAP_BITS_SHIFT 3
// MAX_Real_Vanilla_REG 10
// MAX_Real_Float_REG 6
// MAX_Real_Double_REG 6
// MAX_Real_Long_REG 5
// MAX_Real_XMM_REG 8
EOF
    "$BIN_DIR/genapply" "$WORK_DIR/target-info.h" > "$WORK_DIR/AutoApply.cmm"
    grep -q "Automatically generated by utils/genapply" "$WORK_DIR/AutoApply.cmm"
    echo "genapply OK"
  fi

  if [ ! -x "$BIN_DIR/deriveConstants" ] || [ "${MHS_FORCE_REBUILD:-0}" = "1" ]; then
    echo "=== [tools] deriveConstants (mhs) ==="
    run_logged "$LOG_DIR/deriveConstants.log" \
      "$MHS_BIN" "${MHS_RTS[@]}" -z -s -XCPP "${MHS_DEFS[@]}" "${MHS_INCLUDES[@]}" \
      -C"$WORK_DIR/deriveConstants.mhscache" \
      -o "$BIN_DIR/deriveConstants" "$ROOT/utils/deriveConstants/Main.hs"
    "$BIN_DIR/deriveConstants" --help > "$WORK_DIR/deriveConstants-usage.txt" 2>&1 || true
    grep -qi "usage\|deriveconstants" "$WORK_DIR/deriveConstants-usage.txt" \
      || test -s "$WORK_DIR/deriveConstants-usage.txt"
    echo "deriveConstants OK"
  fi

  if [ ! -x "$BIN_DIR/genprimopcode" ] || [ "${MHS_FORCE_REBUILD:-0}" = "1" ]; then
    echo "=== [tools] genprimopcode (mhs) ==="
    local gp_dir="$AUTOGEN/genprimopcode"
    mkdir -p "$gp_dir"
    if [ ! -f "$gp_dir/Parser.hs" ] || [ ! -f "$gp_dir/Lexer.hs" ] \
        || [ "$ROOT/utils/genprimopcode/Parser.y" -nt "$gp_dir/Parser.hs" ] \
        || [ "$ROOT/utils/genprimopcode/Lexer.x" -nt "$gp_dir/Lexer.hs" ]; then
      echo "--- happy/alex: regenerating Parser/Lexer ---"
      "$BIN_DIR/happy" "$ROOT/utils/genprimopcode/Parser.y" -o "$gp_dir/Parser.hs"
      "$BIN_DIR/alex"  "$ROOT/utils/genprimopcode/Lexer.x"  -o "$gp_dir/Lexer.hs"
    else
      echo "--- reusing $gp_dir/{Parser,Lexer}.hs ---"
    fi
    run_logged "$LOG_DIR/genprimopcode.log" \
        "$MHS_BIN" "${MHS_RTS[@]}" -z -s -XCPP "${MHS_DEFS[@]}" "${MHS_INCLUDES[@]}" \
        -i"$gp_dir" \
        -C"$WORK_DIR/genprimopcode.mhscache" \
        -o "$BIN_DIR/genprimopcode" "$ROOT/utils/genprimopcode/Main.hs"
    # Functional check: preprocess primops.txt.pp (-x c for clang on .pp) and emit --data-decl.
    cc -E -x c -P -w -I"$ROOT/compiler" -I"$AUTOGEN/include" \
      "$ROOT/compiler/GHC/Builtin/primops.txt.pp" -o "$WORK_DIR/primops.txt"
    "$BIN_DIR/genprimopcode" --data-decl < "$WORK_DIR/primops.txt" \
      > "$WORK_DIR/primop-data-decl.hs"
    grep -q "data PrimOp" "$WORK_DIR/primop-data-decl.hs"
    echo "genprimopcode OK"
  fi

  build_ghc_pkg

  if [ ! -x "$BIN_DIR/hsc2hs" ] || [ "${MHS_FORCE_REBUILD:-0}" = "1" ]; then
    echo "=== [tools] hsc2hs (mhs, out-of-tree pin) ==="
    # utils/hsc2hs is not in this tree (it moved out; stage1-tools pins it via
    # source-repository-package). Clone the pin into $OUT scratch instead of
    # dirtying any checkout. Never write into utils/hsc2hs.
    local hsc_repo="${HSC2HS_REPO:-https://github.com/stable-haskell/hsc2hs.git}"
    local hsc_commit="${HSC2HS_COMMIT:-0c7f2940b790bba73274e7bfddffdd6cd916956e}"
    local hsc_src="${HSC2HS_SRC:-$OUT/hsc2hs-src}"
    if [ ! -d "$hsc_src/.git" ]; then
      git clone "$hsc_repo" "$hsc_src"
    fi
    git -C "$hsc_src" remote set-url origin "$hsc_repo"
    git -C "$hsc_src" fetch origin "$hsc_commit" 2>/dev/null || true
    git -C "$hsc_src" checkout -- . 2>/dev/null || true
    git -C "$hsc_src" checkout "$hsc_commit"
    cp "$hsc_src/data/template-hsc.h" "$SHARE_DIR/hsc2hs/template-hsc.h"
    ( cd "$hsc_src" && run_logged "$LOG_DIR/hsc2hs.log" \
      "$MHS_BIN" "${MHS_RTS[@]}" -z -s -XCPP "${MHS_DEFS[@]}" \
      -isrc -i"$AUTOGEN" \
      -i"$ROOT/bootstrap/mhs/shims" -i"$ROOT/bootstrap/mhs/tool-shims" \
      -I"$AUTOGEN/include" -I"$ROOT/bootstrap/mhs/cbits" \
      -C"$WORK_DIR/hsc2hs.mhscache" \
      src/Main.hs cbits/utils.c "$ROOT/bootstrap/mhs/cbits/mhs_tool_posix.c" \
      -o "$BIN_DIR/hsc2hs" )
    "$BIN_DIR/hsc2hs" --version
    echo "hsc2hs OK"
  fi

  ls -lh "$BIN_DIR/unlit" "$BIN_DIR/happy" "$BIN_DIR/alex" \
    "$BIN_DIR/genapply" "$BIN_DIR/deriveConstants" \
    "$BIN_DIR/genprimopcode" "$BIN_DIR/ghc-pkg" "$BIN_DIR/hsc2hs"
}

# --- staged checkpoints ---------------------------------------------------------
# step_mhs <stage> <RootModule...>: compile one umbrella module with `-c` into
# the stage cache; on success promote it to last-good.mhscache. mhs only writes
# -C when compilation succeeds, so a monolithic 700-module step would lose all
# progress on error; these checkpoints bound the loss to one stage.
# Stages are skipped when MHS_FROM_STAGE sorts after them, or when their
# checkpoint already exists (unless MHS_FORCE_REBUILD=1).
STAGE_ORDER=(
  00-shims-platform
  01-ghc-boot
  01b-syntax-th-leaves
  02a-prelude-fs
  02b-unit-syntax
  02c-outputable
  02-utils-data
  03-types-builtin
  04-core-stg-cmm
  05-hs-ast
  06-parser-rn
  07-tc-ds
  08-compiler
)

stage_index() {
  local needle="$1" idx=0 s
  for s in "${STAGE_ORDER[@]}"; do
    if [ "$s" = "$needle" ]; then echo "$idx"; return 0; fi
    idx=$((idx + 1))
  done
  echo "-1"
}

step_mhs() {
  # CI runs each stage as its own process (`ghc-boot`, then `stage-02a`,
  # ...). The -i added in one process does not survive into the next, and
  # ghc-boot's ShortText plus later compiler modules import Data.Binary.
  use_binary_shim
  local name="$1"; shift
  local active_cache="$CACHE_DIR/${name}.mhscache"
  local last_good="$CACHE_DIR/last-good.mhscache"
  local done_stamp="$CACHE_DIR/${name}.done"

  if [ -n "${MHS_FROM_STAGE:-}" ]; then
    local cur_idx from_idx
    cur_idx="$(stage_index "$name")"
    from_idx="$(stage_index "$MHS_FROM_STAGE")"
    if [ "$from_idx" -lt 0 ]; then
      echo "ERROR: unknown MHS_FROM_STAGE=$MHS_FROM_STAGE (expected one of: ${STAGE_ORDER[*]})" >&2
      return 1
    fi
    if [ "$cur_idx" -ge 0 ] && [ "$cur_idx" -lt "$from_idx" ]; then
      echo "=== [$name] Skipping (before MHS_FROM_STAGE=$MHS_FROM_STAGE) ==="
      return 0
    fi
  fi
  if [ -f "$done_stamp" ] && [ -s "$active_cache" ] \
      && [ "${MHS_FORCE_REBUILD:-0}" != "1" ]; then
    echo "=== [$name] Reusing checkpoint ($* ($(date '+%H:%M:%S'))) ==="
    cp "$active_cache" "$last_good"
    return 0
  fi

  echo "=== [$name] Compiling $* ($(date '+%H:%M:%S')) ==="
  rm -f "$active_cache" "$done_stamp"
  if [ -s "$last_good" ]; then
    cp "$last_good" "$active_cache"
  fi
  local rc=0
  if [ -n "${KEEP_STAGE_C:-}" ]; then
    local umbrella="$AUTOGEN/MhsStage_${name}.hs"
    {
      echo "module MhsStage_${name//-/_} where"
      for mod in "$@"; do echo "import $mod ()"; done
    } | write_if_changed "$umbrella"
    # set +u: bash 3.2 errors on an empty "${NIP_FLAG[@]}".
    set +u
    "$MHS_BIN" "${MHS_RTS[@]}" -v -s -XCPP "${MHS_DEFS[@]}" "${NIP_FLAG[@]}" \
      "${MHS_INCLUDES[@]}" -C"$active_cache" \
      -c -o "$WORK_DIR/MhsStage_${name}.c" "$umbrella" \
      > "$LOG_DIR/stage-${name}.log" 2>&1 || rc=$?
    set -u
  else
    set +u
    "$MHS_BIN" "${MHS_RTS[@]}" -v -s -XCPP "${MHS_DEFS[@]}" "${NIP_FLAG[@]}" \
      "${MHS_INCLUDES[@]}" -C"$active_cache" \
      -fno-code "$@" \
      > "$LOG_DIR/stage-${name}.log" 2>&1 || rc=$?
    set -u
  fi
  tail -5 "$LOG_DIR/stage-${name}.log" || true
  if [ $rc -ne 0 ]; then
    echo "ERROR: stage $name failed ($*). Log: $LOG_DIR/stage-${name}.log" >&2
    grep -n -C 5 -E 'error:|panic|mhs:' "$LOG_DIR/stage-${name}.log" >&2 || true
    tail -25 "$LOG_DIR/stage-${name}.log" >&2 || true
    return $rc
  fi
  if [ -s "$active_cache" ]; then
    cp "$active_cache" "$last_good"
    touch "$done_stamp"
    if [ -n "${KEEP_STAGE_C:-}" ]; then
      cp "$WORK_DIR/MhsStage_${name}.c" "$KEEP_STAGE_C"
      rm -f "$WORK_DIR/MhsStage_${name}.c"
    fi
    ls -lh "$last_good"
  else
    echo "ERROR: cache empty after $name ($*)" >&2
    return 1
  fi
}

# 00 without NoImplicitPrelude semantics: shims + platform run WITHOUT
# NIP_FLAG (a shim or platform file may rely on Prelude); stages 01..08 pass it.
stage_00_shims_platform() {
  local saved_nip=()
  if [ "${#NIP_FLAG[@]}" -gt 0 ]; then
    saved_nip=("${NIP_FLAG[@]}")
  fi
  NIP_FLAG=()
  step_mhs 00-shims-platform \
    GHC.Bits GHC.ByteOrder GHC.Show GHC.Stack GHC.Stack.Types \
    System.Directory.MTime System.Directory.OsPath \
    GHC.Platform.ArchOS GHC.Platform.Host
  local rc=$?
  NIP_FLAG=()
  if [ "${#saved_nip[@]}" -gt 0 ]; then
    NIP_FLAG=("${saved_nip[@]}")
  fi
  return $rc
}

# Data.Binary is not in this MicroHs snapshot. The byte-compatible
# implementation is mhs-compat-shims in stable-haskell/cabal. Copy only
# those three modules so Text.PrettyPrint and Distribution.* in that
# tree do not shadow packages the stage-1 tools already installed.
# ghc-boot compiles with -XNoImplicitPrelude. The shim modules name
# Either and the other Prelude types without importing them.
add_binary_shim_prelude() {
  python3 - "$@" <<'PY'
import pathlib, sys
for name in sys.argv[1:]:
    path = pathlib.Path(name)
    lines = path.read_text().splitlines(keepends=True)
    if any(line.startswith("import Prelude") for line in lines):
        continue
    inserted = False
    for i, line in enumerate(lines):
        if line.strip() == ") where" or (line.startswith("module ") and line.rstrip().endswith(" where")):
            lines.insert(i + 1, "\nimport Prelude\n")
            inserted = True
            break
    if not inserted:
        raise SystemExit(f"no module header in {name}")
    path.write_text("".join(lines))
PY
}

install_binary_shim_files() {
  local dest="$OUT/binary-shim"
  if [ ! -f "$dest/Data/Binary.hs" ] \
      || [ ! -f "$dest/Data/Binary/Get.hs" ] \
      || [ ! -f "$dest/Data/Binary/Put.hs" ]; then
  local cabal_mhs_src="${1:-${CABAL_MHS_SRC:-$OUT/cabal-mhs-src}}"
  if [ ! -f "$cabal_mhs_src/mhs-compat-shims/src/Data/Binary.hs" ] \
      && [ -d "$ROOT/_build/mhs-cabal-src/mhs-compat-shims/src" ]; then
    cabal_mhs_src="$ROOT/_build/mhs-cabal-src"
  fi
  if [ ! -f "$cabal_mhs_src/mhs-compat-shims/src/Data/Binary.hs" ]; then
    local repo="${CABAL_MHS_REPO:-https://github.com/stable-haskell/cabal.git}"
    local ref="${CABAL_MHS_REF:-sh/microhs-cabal-install}"
    echo "--- cloning $repo ($ref) for the Data.Binary shim ---"
    rm -rf "$cabal_mhs_src"
    git clone --branch "$ref" --single-branch --depth 1 "$repo" "$cabal_mhs_src"
  fi
  local src="$cabal_mhs_src/mhs-compat-shims/src"
  if [ ! -f "$src/Data/Binary.hs" ]; then
    echo "ERROR: $src has no Data/Binary.hs" >&2
    exit 1
  fi
  mkdir -p "$dest/Data/Binary"
  cp -f "$src/Data/Binary.hs" "$dest/Data/Binary.hs"
  cp -f "$src/Data/Binary/Get.hs" "$dest/Data/Binary/Get.hs"
  cp -f "$src/Data/Binary/Put.hs" "$dest/Data/Binary/Put.hs"
  fi
  add_binary_shim_prelude \
    "$dest/Data/Binary.hs" \
    "$dest/Data/Binary/Get.hs" \
    "$dest/Data/Binary/Put.hs"
}

use_binary_shim() {
  install_binary_shim_files
  if [ "${BINARY_SHIM_ADDED:-0}" = 1 ]; then
    return 0
  fi
  MHS_INCLUDES+=(-i"$OUT/binary-shim")
  BINARY_SHIM_ADDED=1
}

# 01: ghc-boot + ghc-boot-th (MhsGhcBoot + TH syntax).
stage_01_ghc_boot() {
  use_binary_shim
  write_if_changed "$AUTOGEN/MhsGhcBoot.hs" <<'EOF'
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
  step_mhs 01-ghc-boot \
    MhsGhcBoot GHC.LanguageExtensions.Type Language.Haskell.TH.Syntax
}

stage_02a_utils_base() {
  step_mhs 01b-syntax-th-leaves \
    GHC.Platform.Constants GHC.Data.Word64Map.Strict \
    GHC.Data.Word64Set GHC.Utils.Exception GHC.Boot.TH.Syntax
  step_mhs 02a-prelude-fs \
    GHC.Prelude GHC.Data.FastString GHC.Data.StringBuffer
  step_mhs 02b-unit-syntax \
    Language.Haskell.Syntax GHC.Types.Basic GHC.Unit.Types GHC.Unit.Module.Env
  step_mhs 02c-outputable \
    GHC.Utils.Outputable GHC.Utils.Panic GHC.Data.Bag GHC.Data.OrdList
}

stage_02_utils_data() {
  step_mhs 02-utils-data GHC.Utils.Binary
}

stage_03_types_builtin() {
  step_mhs 03-types-builtin \
    GHC.Types.Unique.Supply GHC.Types.SrcLoc GHC.Types.Name GHC.Types.Var \
    GHC.Builtin.Names GHC.Builtin.Types GHC.Builtin.PrimOps
}

stage_04_core_stg_cmm() {
  step_mhs 04-core-stg-cmm \
    GHC.Core GHC.Core.Opt.Pipeline GHC.Stg.Syntax GHC.Cmm GHC.CmmToAsm
}

stage_05_hs_ast() {
  step_mhs 05-hs-ast Language.Haskell.Syntax GHC.Hs
}

stage_06_parser_rn() {
  step_mhs 06-parser-rn GHC.Parser GHC.Rename.Module
}

stage_07_tc_ds() {
  step_mhs 07-tc-ds GHC.Tc.Module GHC.HsToCore
}

stage_08_compiler() {
  step_mhs 08-compiler GHC
}

# --- ghc driver -----------------------------------------------------------------
build_ghc() {
  # Separate CI invocation from the stages. The link reloads compiler
  # modules that import Data.Binary, so the shim has to be on the path.
  use_binary_shim
  local last_good="$CACHE_DIR/last-good.mhscache"
  local stage08="$CACHE_DIR/08-compiler.mhscache"
  if [ ! -s "$stage08" ]; then
    echo "ERROR: 08-compiler.mhscache missing; run the compiler stages first" >&2
    return 1
  fi
  cp "$stage08" "$last_good"
  echo "=== [ghc] Linking $BIN_DIR/ghc ($(date '+%H:%M:%S')) ==="
  local active="$WORK_DIR/ghc-link.mhscache"
  rm -f "$active"
  cp "$last_good" "$active"
  local ghc_stack_optc=()
  case "$(uname -s)" in
    Darwin) ghc_stack_optc=(-optc "-Wl,-stack_size,0x20000000") ;;
    Linux)  ghc_stack_optc=(-optc "-Wl,-z,stack-size=536870912") ;;
  esac
  set +u
  run_logged "$LOG_DIR/ghc-link.log" \
    "$MHS_BIN" "${MHS_RTS[@]}" -z -s -XCPP "${MHS_DEFS[@]}" "${NIP_FLAG[@]}" \
    "${MHS_INCLUDES[@]}" -C"$active" \
    "${ghc_stack_optc[@]}" \
    -optc "-I$ROOT/bootstrap/mhs/cbits" \
    -optc "-include$ROOT/bootstrap/mhs/cbits/mhs_rts_stubs.h" \
    "$ROOT/bootstrap/mhs/cbits/mhs_posix.c" \
    "$ROOT/bootstrap/mhs/cbits/mhs_rts_stubs.c" \
    -o "$BIN_DIR/ghc" ghc/Main.hs
  set -u
  rm -f "$active"

  # Populate $OUT/lib so `ghc -B$OUT/lib --info` and driver invocations work out of the box.
  local cc_cmd cxx_cmd ar_cmd ranlib_cmd ld_cmd otool_cmd install_name_cmd
  local tgt_triple llvm_tgt leading_us ld_compact ld_filelist ld_single ld_gnu sub_via_sym
  cc_cmd="$(command -v cc || echo cc)"
  cxx_cmd="$(command -v c++ || echo c++)"
  ar_cmd="$(command -v ar || echo ar)"
  ranlib_cmd="$(command -v ranlib || echo ranlib)"
  ld_cmd="$(command -v ld || echo ld)"
  otool_cmd="$(command -v otool || echo /bin/false)"
  install_name_cmd="$(command -v install_name_tool || echo /bin/false)"
  case "$MHS_ARCH-$MHS_OS" in
    ArchAArch64-OSDarwin)
      tgt_triple="aarch64-apple-darwin"; llvm_tgt="arm64-apple-darwin"
      leading_us="YES"; ld_compact="YES"; ld_filelist="YES"; ld_single="YES"; ld_gnu="NO"; sub_via_sym="YES" ;;
    ArchX86_64-OSDarwin)
      tgt_triple="x86_64-apple-darwin"; llvm_tgt="x86_64-apple-darwin"
      leading_us="YES"; ld_compact="YES"; ld_filelist="YES"; ld_single="YES"; ld_gnu="NO"; sub_via_sym="YES" ;;
    ArchAArch64-OSLinux)
      tgt_triple="aarch64-unknown-linux"; llvm_tgt="aarch64-unknown-linux-gnu"
      leading_us="NO"; ld_compact="NO"; ld_filelist="NO"; ld_single="NO"; ld_gnu="YES"; sub_via_sym="NO" ;;
    ArchAArch64-OSFreeBSD)
      tgt_triple="aarch64-unknown-freebsd"; llvm_tgt="aarch64-unknown-freebsd"
      leading_us="NO"; ld_compact="NO"; ld_filelist="NO"; ld_single="NO"; ld_gnu="YES"; sub_via_sym="NO" ;;
    ArchX86_64-OSFreeBSD)
      tgt_triple="x86_64-unknown-freebsd"; llvm_tgt="x86_64-unknown-freebsd"
      leading_us="NO"; ld_compact="NO"; ld_filelist="NO"; ld_single="NO"; ld_gnu="YES"; sub_via_sym="NO" ;;
    *)
      tgt_triple="x86_64-unknown-linux"; llvm_tgt="x86_64-unknown-linux-gnu"
      leading_us="NO"; ld_compact="NO"; ld_filelist="NO"; ld_single="NO"; ld_gnu="YES"; sub_via_sym="NO" ;;
  esac

  # Self-contained stage1 lib: ensure deriveConstants + ghc-pkg exist (a
  # standalone `ghc` run never runs build_tools), then generate
  # $OUT/lib/rts/include/DerivedConstants.h via deriveConstants --gen-header
  # so GHC.Unit.State.initUnits loads PlatformConstants from the rts unit.
  if [ ! -x "$BIN_DIR/deriveConstants" ]; then
    echo "--- building deriveConstants for build_ghc ---"
    "$MHS_BIN" "${MHS_RTS[@]}" -s -XCPP "${MHS_DEFS[@]}" "${MHS_INCLUDES[@]}" \
      -C"$WORK_DIR/deriveConstants.mhscache" \
      -o "$BIN_DIR/deriveConstants" "$ROOT/utils/deriveConstants/Main.hs"
  fi
  if [ ! -x "$BIN_DIR/ghc-pkg" ]; then
    echo "--- building ghc-pkg for build_ghc ---"
    build_ghc_pkg
  fi
  local host_arch_def host_os_def dc_target_os
  case "$MHS_ARCH" in
    ArchAArch64) host_arch_def="-Daarch64_HOST_ARCH=1" ;;
    ArchX86_64) host_arch_def="-Dx86_64_HOST_ARCH=1" ;;
    *) host_arch_def="-Dx86_64_HOST_ARCH=1" ;;
  esac
  case "$MHS_OS" in
    OSDarwin) host_os_def="-Ddarwin_HOST_OS=1"; dc_target_os="darwin" ;;
    OSLinux) host_os_def="-Dlinux_HOST_OS=1"; dc_target_os="linux" ;;
    OSFreeBSD) host_os_def="-Dfreebsd_HOST_OS=1"; dc_target_os="freebsd" ;;
    *) host_os_def="-Dlinux_HOST_OS=1"; dc_target_os="linux" ;;
  esac
  mkdir -p "$OUT/lib/rts/include" "$WORK_DIR/dc-tmp"
  "$BIN_DIR/deriveConstants" --gen-header \
    -o "$OUT/lib/rts/include/DerivedConstants.h" \
    --tmpdir "$WORK_DIR/dc-tmp" \
    --gcc-program "$cc_cmd" \
    --gcc-flag "-I$ROOT/rts" \
    --gcc-flag "-I$ROOT/rts/include" \
    --gcc-flag "-I$ROOT/rts-headers/include" \
    --gcc-flag "-I$ROOT/bootstrap/mhs/include" \
    --gcc-flag "-I$AUTOGEN/include" \
    --gcc-flag -DSIZEOF_VOID_P=8 \
    --gcc-flag -DSIZEOF_LONG=8 \
    --gcc-flag -DSIZEOF_LONG_LONG=8 \
    --gcc-flag -DSIZEOF_INT=4 \
    --gcc-flag -DSIZEOF_SHORT=2 \
    --gcc-flag -DSIZEOF_CHAR=1 \
    --gcc-flag "$host_arch_def" \
    --gcc-flag "$host_os_def" \
    --gcc-flag -DHAVE_PTHREAD_H=1 \
    --nm-program "$(command -v nm || echo nm)" \
    --target-os "$dc_target_os"
  rm -rf "$WORK_DIR/dc-tmp"

  mkdir -p "$OUT/lib/package.conf.d"
  cat > "$OUT/lib/package.conf.d/rts-1.0.conf" <<EOF
name: rts
version: 1.0
id: rts
key: rts
exposed: True
include-dirs: $OUT/lib/rts/include
EOF
  if [ -x "$BIN_DIR/ghc-pkg" ]; then
    "$BIN_DIR/ghc-pkg" --global-package-db="$OUT/lib/package.conf.d" recache
  fi
  [ -f "$ROOT/driver/ghc-usage.txt" ] && cp "$ROOT/driver/ghc-usage.txt" "$OUT/lib/ghc-usage.txt"
  [ -f "$ROOT/driver/ghci-usage.txt" ] && cp "$ROOT/driver/ghci-usage.txt" "$OUT/lib/ghci-usage.txt"
  [ -f "$ROOT/llvm-targets" ] && cp "$ROOT/llvm-targets" "$OUT/lib/llvm-targets"
  [ -f "$ROOT/llvm-passes" ] && cp "$ROOT/llvm-passes" "$OUT/lib/llvm-passes"

  cat > "$OUT/lib/settings" <<EOF
[("C compiler command", "$cc_cmd")
,("C compiler flags", "")
,("C++ compiler command", "$cxx_cmd")
,("C++ compiler flags", "")
,("C compiler link flags", "")
,("C compiler supports -no-pie", "NO")
,("CPP command", "$cc_cmd")
,("CPP flags", "-E")
,("Haskell CPP command", "$cc_cmd")
,("Haskell CPP flags", "-E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs")
,("JavaScript CPP command", "$cc_cmd")
,("JavaScript CPP flags", "-E")
,("C-- CPP command", "$cc_cmd")
,("C-- CPP flags", "-E -undef -traditional")
,("C-- CPP supports -g0", "NO")
,("ld supports compact unwind", "$ld_compact")
,("ld supports filelist", "$ld_filelist")
,("ld supports single module", "$ld_single")
,("ld is GNU ld", "$ld_gnu")
,("ld supports verbatim namespace", "NO")
,("Merge objects command", "$ld_cmd")
,("Merge objects flags", "-r")
,("Merge objects supports response files", "NO")
,("ar command", "$ar_cmd")
,("ar flags", "qcls")
,("ar supports at file", "YES")
,("ar supports -L", "NO")
,("ranlib command", "$ranlib_cmd")
,("otool command", "$otool_cmd")
,("install_name_tool command", "$install_name_cmd")
,("windres command", "/bin/false")
,("unlit command", "\$topdir/../bin/unlit")
,("Relative Global Package DB", "package.conf.d")
,("cross compiling", "NO")
,("target platform string", "$tgt_triple")
,("target os", "$MHS_OS")
,("target arch", "$MHS_ARCH")
,("target word size", "8")
,("target word big endian", "NO")
,("target has GNU nonexec stack", "NO")
,("target has .ident directive", "YES")
,("target has subsections via symbols", "$sub_via_sym")
,("target has libm", "YES")
,("Unregisterised", "NO")
,("LLVM target", "$llvm_tgt")
,("LLVM llc command", "llc")
,("LLVM opt command", "opt")
,("LLVM llvm-as command", "clang")
,("LLVM llvm-as flags", "")
,("Use inplace MinGW toolchain", "NO")
,("target RTS linker only supports shared libraries", "NO")
,("Use interpreter", "NO")
,("Use LibFFI", "NO")
,("Tables next to code", "YES")
,("Leading underscore", "$leading_us")
,("target is dynamic", "NO")
,("target ships dynamic libraries", "NO")
,("target is profiled", "NO")
,("target ships profiling libraries", "NO")
,("base unit-id", "base")
]
EOF
  ls -lh "$BIN_DIR/ghc"
}

# --- smoke tests ------------------------------------------------------------------
run_tests() {
  echo "=== [test] stage1 tools ==="
  # Phase 6 is a fresh job. The tools artifact does not include $WORK_DIR,
  # so the probes written while building the tools are not here.
  mkdir -p "$WORK_DIR"
  printf 'hello\n\n> main = putStrLn "hi"\n' > "$WORK_DIR/unlit-probe.lhs"
  "$BIN_DIR/unlit" -q "$WORK_DIR/unlit-probe.lhs" "$WORK_DIR/unlit-retest.hs"
  grep 'main = putStrLn' "$WORK_DIR/unlit-retest.hs" >/dev/null && echo "unlit OK"
  "$BIN_DIR/happy" --version | head -n 1
  "$BIN_DIR/alex" --version | head -n 1
  cat > "$WORK_DIR/target-info.h" <<'EOF'
// WORD_SIZE 8
// TAG_BITS 1
// BITMAP_BITS_SHIFT 3
// MAX_Real_Vanilla_REG 10
// MAX_Real_Float_REG 6
// MAX_Real_Double_REG 6
// MAX_Real_Long_REG 5
// MAX_Real_XMM_REG 8
EOF
  "$BIN_DIR/genapply" "$WORK_DIR/target-info.h" | grep "Automatically generated" >/dev/null \
    && echo "genapply OK"
  "$BIN_DIR/deriveConstants" --help >/dev/null 2>&1 || true
  echo "deriveConstants OK (runs)"
  cc -E -x c -P -w -I"$ROOT/compiler" -I"$AUTOGEN/include" \
    "$ROOT/compiler/GHC/Builtin/primops.txt.pp" -o "$WORK_DIR/primops.txt"
  "$BIN_DIR/genprimopcode" --data-decl < "$WORK_DIR/primops.txt" \
    | grep "data PrimOp" >/dev/null && echo "genprimopcode OK"
  "$BIN_DIR/ghc-pkg" --version
  "$BIN_DIR/hsc2hs" --version
  if [ -x "$BIN_DIR/ghc" ]; then
    echo "=== [test] stage1 ghc ==="
    "$BIN_DIR/ghc" -B"$OUT/lib" --version
    "$BIN_DIR/ghc" -B"$OUT/lib" --numeric-version
    "$BIN_DIR/ghc" -B"$OUT/lib" --supported-languages | sed -n '1,5p'
    "$BIN_DIR/ghc" -B"$OUT/lib" --info | sed -n '1,10p'

    echo "=== [test] stage1 ghc native codegen (HelloStage1.hs -> .s + .o + .hi) ==="
    cat > "$WORK_DIR/HelloStage1.hs" <<'HSEOF'
{-# LANGUAGE NoImplicitPrelude #-}
module HelloStage1 (MyBool(..), notB, choose) where

data MyBool = MyTrue | MyFalse

notB :: MyBool -> MyBool
notB MyTrue  = MyFalse
notB MyFalse = MyTrue

choose :: MyBool -> a -> a -> a
choose MyTrue  x _ = x
choose MyFalse _ y = y
HSEOF
    rm -f "$WORK_DIR/HelloStage1.s" "$WORK_DIR/HelloStage1.o" "$WORK_DIR/HelloStage1.hi" "$WORK_DIR/HelloStage1.hie"
    "$BIN_DIR/ghc" -B"$OUT/lib" -no-rts -this-unit-id ghc-internal -dno-typeable-binds \
      -O -S -ddump-simpl "$WORK_DIR/HelloStage1.hs" \
      -o "$WORK_DIR/HelloStage1.s" > "$WORK_DIR/HelloStage1-simpl.txt"
    test -s "$WORK_DIR/HelloStage1.s"
    grep -q "notB" "$WORK_DIR/HelloStage1-simpl.txt"
    "$BIN_DIR/ghc" -B"$OUT/lib" -no-rts -this-unit-id ghc-internal -dno-typeable-binds \
      -O -c "$WORK_DIR/HelloStage1.hs" \
      -o "$WORK_DIR/HelloStage1.o"
    test -s "$WORK_DIR/HelloStage1.o"
    test -s "$WORK_DIR/HelloStage1.hi"
    file "$WORK_DIR/HelloStage1.o"
    nm "$WORK_DIR/HelloStage1.o" | grep "HelloStage1_notB_info" >/dev/null
    ls -lh "$WORK_DIR/HelloStage1.s" "$WORK_DIR/HelloStage1.o" "$WORK_DIR/HelloStage1.hi"
    echo "stage1 ghc native codegen OK"
  else
    echo "=== [test] stage1 ghc: SKIP ($BIN_DIR/ghc not built) ==="
  fi
  echo "ALL TOOL TESTS DONE"
}

cmd="${1:-all}"
case "$cmd" in
  toolchain) build_toolchain ;;
  tools) prepare_autogen; build_tools ;;
  ghc-boot) prepare_autogen; stage_00_shims_platform; stage_01_ghc_boot ;;
  compiler)
    prepare_autogen
    stage_00_shims_platform; stage_01_ghc_boot
    stage_02a_utils_base; stage_02_utils_data; stage_03_types_builtin; stage_04_core_stg_cmm
    stage_05_hs_ast; stage_06_parser_rn; stage_07_tc_ds; stage_08_compiler
    ;;
  stage-02a) prepare_autogen; stage_02a_utils_base ;;
  stage-02)  prepare_autogen; stage_02_utils_data ;;
  stage-03)  prepare_autogen; stage_03_types_builtin ;;
  stage-04)  prepare_autogen; stage_04_core_stg_cmm ;;
  stage-05)  prepare_autogen; stage_05_hs_ast ;;
  stage-06)  prepare_autogen; stage_06_parser_rn ;;
  stage-07)  prepare_autogen; stage_07_tc_ds ;;
  stage-08)  prepare_autogen; stage_08_compiler ;;
  ghc) prepare_autogen; build_ghc ;;
  test) run_tests ;;
  all)
    build_toolchain
    refresh_mhs_bin_after_toolchain
    prepare_autogen
    build_tools
    stage_00_shims_platform; stage_01_ghc_boot
    stage_02a_utils_base; stage_02_utils_data; stage_03_types_builtin; stage_04_core_stg_cmm
    stage_05_hs_ast; stage_06_parser_rn; stage_07_tc_ds; stage_08_compiler
    build_ghc
    run_tests
    ;;
  *)
    echo "Usage: $0 [toolchain|tools|ghc-boot|compiler|ghc|test|all]" >&2
    exit 1
    ;;
esac
