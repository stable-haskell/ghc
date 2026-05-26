#!/usr/bin/env bash
# Build WASM cross-compiler using Makefile (NOT Hadrian!)
# Based on https://github.com/stable-haskell/ghc/issues/134
#
# This script uses NIX flake for ALL dependencies (NO ghcup!)
# 1. Nix provides GHC bootstrap compiler and tools
# 2. Nix provides LLVM/Clang with WASM support (pinned via ghc-wasm-meta)
# 3. Builds stage2 (bootstrap native compiler)
# 4. Builds stage3-wasm32-wasi (cross-compiler + WASM libraries)
#
# The Makefile default CABAL=_build/stage0/bin/cabal is used (no override needed):
# stage0 cabal is built automatically from the repo's pinned Cabal source.

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info() { echo -e "${BLUE}ℹ${NC} $*"; }
success() { echo -e "${GREEN}✓${NC} $*"; }
error() { echo -e "${RED}✗${NC} $*"; }
warn() { echo -e "${YELLOW}⚠${NC} $*"; }

# Check we're in the right place
if [ ! -f "Makefile" ]; then
    error "Not in GHC source directory (Makefile not found)"
    exit 1
fi

info "Building WASM cross-compiler via Makefile"
info "Using Nix flake for all dependencies (NO ghcup)"
info ""

# Check for nix
if ! command -v nix >/dev/null 2>&1; then
    error "nix not found. Please install Nix first."
    exit 1
fi

# Build stage2: native bootstrap compiler
# Uses Makefile default CABAL=_build/stage0/bin/cabal (stage0 built automatically)
info "Step 1/2: Building stage2 (bootstrap compiler)"
info "This builds GHC itself using the bootstrap compiler..."
info "Expected time: 45-60 minutes"
info ""

nix develop . -c make stage2

echo ""
success "Stage2 build complete!"
echo ""

# Build stage3-wasm32-wasi: cross-compiler + WASM libraries
info "Step 2/2: Building stage3-wasm32-wasi (WASM cross-compiler)"
info "This builds the WASM cross-compiler using stage2..."
info "Expected time: 30-45 minutes"
info ""

nix develop . -c make stage3-wasm32-wasi

echo ""
success "WASM cross-compiler build complete!"
echo ""
echo "Cross-compiler location:"
echo "  _build/stage3/bin/wasm32-wasi-ghc"
echo ""
echo "To test:"
echo "  printf 'main = putStrLn \"Hello WASM\"\n' > hello.hs"
echo "  nix develop . -c _build/stage3/bin/wasm32-wasi-ghc hello.hs -o hello.wasm"
