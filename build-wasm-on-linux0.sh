#!/usr/bin/env bash
# Helper script to build WASM cross-compiler on linux-0
# Syncs the repo (source only, no _build/), then runs the Makefile-based build.
#
# Usage: ./build-wasm-on-linux0.sh
#
# Prerequisites on linux-0:
# 1. Nix must be installed with flake support
#
# Note: This script handles the case where the local repo is a git worktree
# by re-initializing a fresh git repo on the remote after syncing (nix develop
# requires the directory to be a proper git repository to evaluate flake.nix).

set -euo pipefail

REMOTE_HOST="${REMOTE_HOST:-x86_64-linux-0.lan}"
REMOTE_USER="${REMOTE_USER:-${USER}}"
REMOTE_DIR="${REMOTE_DIR:-/tmp/ghc-wasm-build}"
LOCAL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

info() { echo -e "${BLUE}ℹ${NC} $*"; }
success() { echo -e "${GREEN}✓${NC} $*"; }
warn() { echo -e "${YELLOW}⚠${NC} $*"; }

info "Syncing GHC source to ${REMOTE_HOST}:${REMOTE_DIR}..."
ssh "${REMOTE_USER}@${REMOTE_HOST}" "mkdir -p ${REMOTE_DIR}"

# Sync the source (excluding build artifacts and git metadata).
# We exclude .git because when building from a git worktree, .git is a FILE
# pointing to the main repo on the local machine — unusable on the remote.
rsync -avz --delete \
    --exclude='_build/' \
    --exclude='.git' \
    --exclude='.git/' \
    --exclude='cabal-cache/' \
    --exclude='.nix-wasm-bin/' \
    --exclude='*.o' \
    --exclude='*.hi' \
    "${LOCAL_DIR}/" \
    "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR}/"

# Re-initialize git on the remote.
# nix develop requires the directory to be a valid git repo to evaluate flake.nix
# (it uses builtins.fetchGit / git+file:// URL for the local flake).
# After rsync (which excludes .git), we create a fresh repo with all files staged.
info "Re-initializing git repository on remote (needed for nix develop)..."
ssh "${REMOTE_USER}@${REMOTE_HOST}" "cd ${REMOTE_DIR} && git init -q && git add -A && git commit -q -m 'build: local snapshot' 2>/dev/null || true"

success "Sync complete"

info "Starting WASM cross-compiler build on linux-0..."
info "Using Makefile-based build (stage2 → stage3-wasm32-wasi)"
warn "This will take 1-2 hours depending on the machine"
echo ""

# Run the build interactively so progress is visible
ssh -t "${REMOTE_USER}@${REMOTE_HOST}" "cd ${REMOTE_DIR} && ./build-wasm-make.sh"

success "Build completed!"
info "The WASM cross-compiler is available at:"
echo "  ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR}/_build/stage3/bin/wasm32-wasi-ghc"
