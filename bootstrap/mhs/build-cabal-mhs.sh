#!/usr/bin/env bash
# Build cabal-install with MicroHs, using stable-haskell/cabal PR 533
# (branch sh/microhs-cabal-install).  That branch already contains the
# MicroHs port and bootstrap/mhs/build-cabal-mhs.sh.  This wrapper checks
# the branch out next to the GHC tree and runs it.
#
#   bootstrap/mhs/build-cabal-mhs.sh [toolchain|build|test|all]
#
# Environment:
#   CABAL_MHS_REPO   git URL           (default: https://github.com/stable-haskell/cabal.git)
#   CABAL_MHS_REF    branch or commit  (default: sh/microhs-cabal-install)
#   CABAL_MHS_SRC    checkout          (default: $ROOT/_build/mhs-cabal-src)
#   MHS_PREFIX       passed through to the cabal script (default: $HOME/.mcabal)
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CABAL_MHS_REPO="${CABAL_MHS_REPO:-https://github.com/stable-haskell/cabal.git}"
CABAL_MHS_REF="${CABAL_MHS_REF:-sh/microhs-cabal-install}"
CABAL_MHS_SRC="${CABAL_MHS_SRC:-$ROOT/_build/mhs-cabal-src}"
CMD="${1:-all}"

if [ ! -d "$CABAL_MHS_SRC/.git" ]; then
  echo "=== [cabal-mhs] Cloning $CABAL_MHS_REPO ($CABAL_MHS_REF) ==="
  git clone --branch "$CABAL_MHS_REF" --single-branch "$CABAL_MHS_REPO" "$CABAL_MHS_SRC"
else
  echo "=== [cabal-mhs] Updating $CABAL_MHS_SRC to $CABAL_MHS_REF ==="
  git -C "$CABAL_MHS_SRC" fetch origin "$CABAL_MHS_REF"
  git -C "$CABAL_MHS_SRC" checkout "$CABAL_MHS_REF"
  git -C "$CABAL_MHS_SRC" pull --ff-only origin "$CABAL_MHS_REF" || true
fi

echo "=== [cabal-mhs] $(git -C "$CABAL_MHS_SRC" log -1 --oneline) ==="
exec "$CABAL_MHS_SRC/bootstrap/mhs/build-cabal-mhs.sh" "$CMD"
