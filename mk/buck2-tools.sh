#!/bin/sh
# Install the experimental Linux Buck2 tools (see README.md) into a prefix.
#
#   mk/buck2-tools.sh <prefix> [<boot ghc>]
#
# Afterwards <prefix>/bin holds buck2 and cabal; ./buck2 holds the rules.
# Override CABAL_REPO/CABAL_REV or HASKELL_BUCK2_REPO/HASKELL_BUCK2_REV
# to test other exact commits. A BUCK2_RELEASE override also needs the
# corresponding archive's BUCK2_SHA256. Use a fresh prefix for other pins.
set -eu

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  echo "usage: $0 <prefix> [<boot ghc>]" >&2
  exit 1
fi
case "$(uname -s)" in
  Linux) ;;
  *) echo "buck2-tools: the experimental build currently supports Linux only" >&2; exit 1 ;;
esac
case "$(uname -m)" in
  x86_64) arch=x86_64; default_sha256=dc9447bd2e13860fc157001a1803c518c085444dcc6c65460d9d1f55154b32a3 ;;
  aarch64|arm64) arch=aarch64; default_sha256=dbcee75db00c557df2dbb382c6b5be1643c086b87d924aab7060998a7a6d5801 ;;
  *) echo "unsupported architecture: $(uname -m)" >&2; exit 1 ;;
esac

prefix=$(realpath -m "$1")
ghc=${2:-ghc-9.8.4}
BUCK2_RELEASE=${BUCK2_RELEASE:-2026-09-01}
BUCK2_SHA256=${BUCK2_SHA256:-$default_sha256}
CABAL_REPO=${CABAL_REPO:-https://github.com/stable-haskell/cabal.git}
CABAL_REV=${CABAL_REV:-aae7c3bd1925f9cc6b40944f11295e863a90b3b1}
HASKELL_BUCK2_REPO=${HASKELL_BUCK2_REPO:-https://github.com/stable-haskell/haskell-buck2.git}
HASKELL_BUCK2_REV=${HASKELL_BUCK2_REV:-9a5075e72f27dd0f74638f1181d19c1a4df04d1a}

# Do not silently reuse a moving branch or overwrite a developer's checkout.
checkout() {
  repo=$1 rev=$2 dest=$3
  case "$rev" in
    *[!0-9a-f]*|'') echo "expected a full commit id: $rev" >&2; exit 1 ;;
  esac
  if [ "${#rev}" -ne 40 ]; then
    echo "expected a full commit id: $rev" >&2; exit 1
  fi
  if [ ! -d "$dest" ]; then
    git init -q "$dest"
    git -C "$dest" fetch -q --depth 1 "$repo" "$rev"
    git -C "$dest" checkout -q --detach FETCH_HEAD
  fi
  if [ "$(git -C "$dest" rev-parse HEAD)" != "$rev" ] ||
      [ -n "$(git -C "$dest" status --porcelain)" ]; then
    echo "$dest must be a clean checkout of $rev; use a fresh path" >&2
    exit 1
  fi
}

mkdir -p "$prefix/bin" "$prefix/src"
buck2_source="$BUCK2_RELEASE $arch $BUCK2_SHA256"
if [ ! -x "$prefix/bin/buck2" ] ||
    [ "$(cat "$prefix/bin/.buck2-source" 2>/dev/null || true)" != "$buck2_source" ]; then
  echo "== buck2 $BUCK2_RELEASE ($arch)"
  download=$(mktemp -d "$prefix/.buck2-download.XXXXXX")
  trap 'rm -rf "$download"' 0
  trap 'exit 1' 1 2 15
  curl -fsSL "https://github.com/facebook/buck2/releases/download/$BUCK2_RELEASE/buck2-$arch-unknown-linux-gnu.zst" -o "$download/buck2.zst"
  printf '%s  %s\n' "$BUCK2_SHA256" "$download/buck2.zst" | sha256sum -c -
  zstd -d -q "$download/buck2.zst" -o "$download/buck2"
  chmod +x "$download/buck2"
  "$download/buck2" --version
  mv "$download/buck2" "$prefix/bin/buck2"
  printf '%s\n' "$buck2_source" > "$prefix/bin/.buck2-source"
fi
"$prefix/bin/buck2" --version

cabal_source="$CABAL_REPO $CABAL_REV $ghc"
if [ ! -x "$prefix/bin/cabal" ] ||
    [ "$(cat "$prefix/bin/.cabal-source" 2>/dev/null || true)" != "$cabal_source" ]; then
  echo "== cabal with the buck2 command ($CABAL_REV), built with $ghc"
  checkout "$CABAL_REPO" "$CABAL_REV" "$prefix/src/cabal"
  (cd "$prefix/src/cabal" && cabal install exe:cabal -w "$ghc" --disable-tests --disable-benchmarks \
      --installdir="$prefix/bin" --install-method=copy --overwrite-policy=always)
  printf '%s\n' "$cabal_source" > "$prefix/bin/.cabal-source"
fi
"$prefix/bin/cabal" --version

checkout "$HASKELL_BUCK2_REPO" "$HASKELL_BUCK2_REV" buck2
