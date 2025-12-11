#!/bin/sh

set -e

cabal build \
	${CABAL0_ARGS} \
	-j \
	--disable-tests \
	--project-dir libraries/Cabal \
	--builddir=$(pwd)/build/stage0 \
	--ghc-options="-fhide-source-paths" \
	cabal-install:exe:cabal 1>&2

exec cabal list-bin \
	${CABAL0_ARGS} \
	-v0 \
	-j \
	--disable-tests \
	--project-dir libraries/Cabal \
	--builddir=$(pwd)/build/stage0 \
	--ghc-options="-fhide-source-paths" \
	cabal-install:exe:cabal
