# Self-documenting Makefile for the stable-haskell JS + native hello.
#
# One source, two targets from the multi-target GHC:
#   make run-node     # build for the JS backend, run the .js under node
#   make run-native   # build the SAME source natively, run it on this host
# (wasm has its own reactor template — see the `hello` example.)

.DEFAULT_GOAL := help
B := \033[1m
N := \033[0m

.PHONY: help
help:
	@printf "$(B)stable-haskell JS + native hello$(N)\n\n"
	@printf "  $(B)make build$(N)        cabal build for the JS backend\n"
	@printf "  $(B)make run-node$(N)     build + run the JS output under node\n"
	@printf "  $(B)make run-native$(N)   build + run the SAME source natively (ghc)\n"
	@printf "  $(B)make clean$(N)        remove build artifacts\n"
	@printf "\nPrerequisites:\n"
	@printf "  - ghcup install ghc   multi-9.14.0.stable.1   (native + wasm + JS)\n"
	@printf "  - ghcup install cabal 3.17.0.0.stable.0\n"
	@printf "  - emscripten + node on \$$PATH (the JS target's C toolchain + runtime)\n"

.PHONY: build
build:
	cabal build myapp

# The JS backend emits a node-runnable program; `cabal list-bin` prints its path.
.PHONY: run-node
run-node: build
	@node "$$(cabal list-bin exe:myapp | tail -1)"

# Same Main.hs, native compiler — bypasses cabal.project's JS with-compiler.
.PHONY: run-native
run-native:
	@ghc -O0 -outputdir .native-build -o myapp-native app/Main.hs >/dev/null
	@./myapp-native

.PHONY: clean
clean:
	rm -rf dist-newstyle .native-build
	rm -f myapp-native
