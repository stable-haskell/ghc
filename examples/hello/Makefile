# Self-documenting Makefile for the stable-haskell wasm hello template.
#
# Default target prints help. Common workflow:
#   make build      # cabal builds myapp.wasm via the wasm cross-compiler
#   make run-node   # post-link + run under node (prints "Hello from …")
#   make run-web    # post-link + serve public/ at http://localhost:8000

# Locate post-link.mjs by asking the wasm compiler for its libdir — version-
# agnostic, so this works with the multi-target bindist, a standalone wasm
# cross-compiler, or any install layout. Override WASM_LIB to force a path.
WASM_LIB     ?= $(shell wasm32-unknown-wasi-ghc --print-libdir 2>/dev/null)
# post-link.mjs uses `import.meta.filename === process.argv[1]` to detect
# direct invocation; on macOS where /tmp is a symlink, argv[1] keeps the
# user-typed path while import.meta.filename is realpath-resolved, so we
# canonicalize via `realpath` to keep them equal.
POST_LINK    := $(shell realpath -m "$(WASM_LIB)/post-link.mjs" 2>/dev/null || echo "$(WASM_LIB)/post-link.mjs")

WASM_OUT     := dist-newstyle/store/host/wasm32-unknown-wasi/bin/myapp.wasm

.DEFAULT_GOAL := help

# Pretty terminal colors (subdued)
B := \033[1m
N := \033[0m

.PHONY: help
help:
	@printf "$(B)stable-haskell wasm hello template$(N)\n\n"
	@printf "  $(B)make build$(N)      build myapp.wasm via cabal (dual-compiler)\n"
	@printf "  $(B)make run-node$(N)   post-link + execute under node (prints stdout)\n"
	@printf "  $(B)make run-web$(N)    post-link + serve public/ at :8000\n"
	@printf "  $(B)make clean$(N)      remove build artifacts\n"
	@printf "\nPrerequisites:\n"
	@printf "  - ghcup install ghc   multi-9.14.0.stable.1   (native + wasm + JS)\n"
	@printf "  - ghcup install cabal 3.17.0.0.stable.0\n"
	@printf "  - node on \$$PATH (Node.js 22+) — required for both post-link and run\n"
	@printf "\nWASM_LIB = $(WASM_LIB)\n"

.PHONY: build
build:
	cabal build myapp

# post-link.mjs reads the ghc_wasm_jsffi custom section out of the wasm and
# emits a paired ESM module the JS host imports to provide the wasm's
# JavaScript-side FFI imports. The wasm + jsffi.mjs MUST live next to each
# other when the host loads them.
$(WASM_OUT): build

.PHONY: post-link-node
post-link-node: $(WASM_OUT)
	@cp $(WASM_OUT) ./myapp.wasm
	@node $(POST_LINK) -i ./myapp.wasm -o ./jsffi.mjs
	@printf "Generated ./myapp.wasm + ./jsffi.mjs (for run-node)\n"

.PHONY: post-link-web
post-link-web: $(WASM_OUT)
	@mkdir -p public
	@cp $(WASM_OUT) public/myapp.wasm
	@node $(POST_LINK) -i public/myapp.wasm -o public/jsffi.mjs
	@printf "Generated public/myapp.wasm + public/jsffi.mjs (for run-web)\n"

.PHONY: run-node
run-node: post-link-node
	@node run.mjs

.PHONY: run-web
run-web: post-link-web
	@printf "Serving public/ at $(B)http://localhost:8000$(N) (Ctrl-C to stop)\n"
	@cd public && python3 -m http.server 8000

.PHONY: clean
clean:
	rm -rf dist-newstyle
	rm -f myapp.wasm jsffi.mjs
	rm -f public/myapp.wasm public/jsffi.mjs
