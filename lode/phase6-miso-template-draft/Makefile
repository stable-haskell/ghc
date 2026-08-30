# Self-documenting Makefile for stable-haskell/miso-wasm-template.
# Run `make` (or `make help`) to see targets.
#
# All toolchain selection lives in cabal.project (with-compiler /
# with-build-compiler). No redundant CLI flags here — keeps a single
# source of truth.

.DEFAULT_GOAL := help
SHELL := /bin/bash

# Override these only if invoking a non-default toolchain.
WASM_GHC      ?= wasm32-wasi-ghc
CABAL         ?= cabal
PKG           := myapp

# Discovered lazily so `make help` works without the toolchain installed.
LIBDIR         = $(shell $(WASM_GHC) --print-libdir)
POST_LINK      = $(LIBDIR)/post-link.mjs
WASM_ARTIFACT  = $(shell find dist-newstyle -name '$(PKG).wasm' -print -quit)

.PHONY: help build link serve clean cabal-build

help:                       ## Show this help.
	@awk 'BEGIN{FS=":.*##"; printf "\nTargets:\n"} \
	      /^[a-zA-Z_-]+:.*##/ {printf "  \033[36m%-12s\033[0m %s\n", $$1, $$2}' \
	      $(MAKEFILE_LIST)

build: link                 ## Build wasm + emit JSFFI glue + copy to public/.

cabal-build:                ## Run cabal (toolchains taken from cabal.project).
	$(CABAL) build $(PKG)

link: cabal-build           ## Run post-link.mjs and stage artifacts.
	@mkdir -p public
	@test -n "$(WASM_ARTIFACT)" || { echo "no .wasm found under dist-newstyle"; exit 1; }
	node $(POST_LINK) --input $(WASM_ARTIFACT) --output public/ghc_wasm_jsffi.js
	cp $(WASM_ARTIFACT) public/myapp.wasm

serve:                      ## Serve public/ at http://localhost:8080 (Ctrl-C to stop).
	npx --yes http-server public -c-1 -p 8080

clean:                      ## Remove build artifacts.
	rm -rf dist-newstyle
	rm -f public/myapp.wasm public/ghc_wasm_jsffi.js
