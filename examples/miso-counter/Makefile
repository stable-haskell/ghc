# Self-documenting Makefile for the stable-haskell wasm miso-counter template.
#
# Default target prints help. Typical workflow:
#   make build      # cabal builds myapp.wasm  (FIRST run: ~5-10 min, mostly TH deps)
#   make run-web    # post-link + serve public/ at http://localhost:8000
#
# (No run-node target — miso is a DOM-driven framework; it needs a browser.)

WASM_VERSION ?= wasm32-wasi-9.14.0.stable.12
GHCUP_PREFIX ?= $(or $(GHCUP_INSTALL_BASE_PREFIX),$(HOME))
WASM_LIB     ?= $(GHCUP_PREFIX)/.ghcup/ghc/$(WASM_VERSION)/lib/targets/wasm32-unknown-wasi/lib
POST_LINK    := $(shell realpath -m "$(WASM_LIB)/post-link.mjs" 2>/dev/null || echo "$(WASM_LIB)/post-link.mjs")

WASM_OUT     := dist-newstyle/store/host/wasm32-unknown-wasi/bin/myapp.wasm

.DEFAULT_GOAL := help

B := \033[1m
N := \033[0m

.PHONY: help
help:
	@printf "$(B)stable-haskell wasm miso-counter template$(N)\n\n"
	@printf "  $(B)make build$(N)      build myapp.wasm via cabal (dual-compiler)\n"
	@printf "                  first run: ~5-10 min (50+ TH-heavy deps:\n"
	@printf "                  aeson, lens, jsaddle, miso, jsaddle-wasm, ...)\n"
	@printf "  $(B)make run-web$(N)    post-link + serve public/ at :8000\n"
	@printf "                  open http://localhost:8000 in your browser\n"
	@printf "  $(B)make clean$(N)      remove build artifacts (keeps dep cache)\n"
	@printf "\nPrerequisites:\n"
	@printf "  - ghcup install ghc   $(WASM_VERSION)\n"
	@printf "  - ghcup install cabal 3.17.0.0.stable.0\n"
	@printf "  - node on \$$PATH (Node.js 22+) — required for the wasm-iserv\n"
	@printf "    Template Haskell host and for post-link\n"
	@printf "  - python3 on \$$PATH (only for $(B)run-web$(N) — the http.server)\n"
	@printf "\nWASM_LIB = $(WASM_LIB)\n"

.PHONY: build
build:
	cabal build myapp

$(WASM_OUT): build

.PHONY: post-link-web
post-link-web: $(WASM_OUT)
	@mkdir -p public
	@cp $(WASM_OUT) public/myapp.wasm
	@node $(POST_LINK) -i public/myapp.wasm -o public/jsffi.mjs
	@printf "Generated public/myapp.wasm + public/jsffi.mjs (for run-web)\n"

.PHONY: run-web
run-web: post-link-web
	@printf "Serving $(B)public/$(N) at $(B)http://localhost:8000$(N) (Ctrl-C to stop)\n"
	@cd public && python3 -m http.server 8000

.PHONY: clean
clean:
	rm -rf dist-newstyle
	rm -f public/myapp.wasm public/jsffi.mjs
