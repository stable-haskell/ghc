#!/bin/sh
# Install the tools of the Buck2 build (see README.md, "Building with
# Buck2") into a prefix: the pinned buck2 release, cabal-install with the
# `buck2` command (built from the Stable Haskell fork of cabal with the
# boot compiler), and the haskell-buck2 rules at ./buck2.
#
#   mk/buck2-tools.sh <prefix> [<boot ghc>]
#
# Afterwards <prefix>/bin holds buck2 and cabal (put it first on $PATH).
# Set BUCK2_RELEASE, CABAL_REPO, CABAL_BRANCH, HASKELL_BUCK2_REPO,
# HASKELL_BUCK2_BRANCH to override the pinned sources.
set -eu

prefix=$(realpath -m "$1")
ghc=${2:-ghc-9.8.4}
BUCK2_RELEASE=${BUCK2_RELEASE:-2026-09-01}
CABAL_REPO=${CABAL_REPO:-https://github.com/stable-haskell/cabal.git}
CABAL_BRANCH=${CABAL_BRANCH:-cabal-buck2-ghc}
HASKELL_BUCK2_REPO=${HASKELL_BUCK2_REPO:-https://github.com/stable-haskell/haskell-buck2.git}
HASKELL_BUCK2_BRANCH=${HASKELL_BUCK2_BRANCH:-cabal-buck2-ghc}

mkdir -p "$prefix/bin" "$prefix/src"

if [ ! -x "$prefix/bin/buck2" ]; then
  echo "== buck2 $BUCK2_RELEASE"
  case "$(uname -m)" in
    x86_64) arch=x86_64 ;;
    aarch64|arm64) arch=aarch64 ;;
    *) echo "unsupported architecture: $(uname -m)" >&2; exit 1 ;;
  esac
  curl -fsSL "https://github.com/facebook/buck2/releases/download/$BUCK2_RELEASE/buck2-$arch-unknown-linux-gnu.zst" -o "$prefix/bin/buck2.zst"
  zstd -d -f -q "$prefix/bin/buck2.zst" -o "$prefix/bin/buck2"
  rm -f "$prefix/bin/buck2.zst"
  chmod +x "$prefix/bin/buck2"
fi
"$prefix/bin/buck2" --version

if [ ! -x "$prefix/bin/cabal" ]; then
  echo "== cabal with the buck2 command ($CABAL_REPO, $CABAL_BRANCH), built with $ghc"
  if [ ! -d "$prefix/src/cabal" ]; then
    git clone -q --depth 1 -b "$CABAL_BRANCH" "$CABAL_REPO" "$prefix/src/cabal"
  fi
  (cd "$prefix/src/cabal" && cabal install exe:cabal -w "$ghc" --disable-tests --disable-benchmarks \
      --installdir="$prefix/bin" --install-method=copy --overwrite-policy=always)
fi
"$prefix/bin/cabal" --version

if [ ! -d buck2 ]; then
  echo "== haskell-buck2 ($HASKELL_BUCK2_REPO, $HASKELL_BUCK2_BRANCH) at ./buck2"
  git clone -q --depth 1 -b "$HASKELL_BUCK2_BRANCH" "$HASKELL_BUCK2_REPO" buck2
fi
