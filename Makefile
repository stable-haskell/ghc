SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

GHC ?= ghc
GHC_VERSION=$(shell $(GHC) --numeric-version)

STAGE ?= stage1

BUILD_DIR = _build/$(STAGE)/build
STORE_DIR = _build/$(STAGE)/store

GHC = _build/$(STAGE)/bin/ghc
CABAL := _build/cabal/bin/cabal

.PHONY: all
all: _build/stage1/bin/ghc

CONFIG_SCRIPTS := $(patsubst %.ac,%,$(wildcard libraries/*/configure.ac))

$(CONFIG_SCRIPTS) : % : %.ac
	autoreconf -i $(@D)

all-config-scripts: $(CONFIG_SCRIPTS)

cabal: $(CABAL)

CABAL_BUILD    = $(CABAL) --store-dir $(STORE_DIR) build --builddir $(abspath $(BUILD_DIR)) $(CABAL_ARGS)
CABAL_LIST_BIN = $(CABAL) --store-dir $(STORE_DIR) list-bin --builddir $(abspath $(BUILD_DIR)) $(CABAL_ARGS)

$(CABAL): STAGE=cabal
$(CABAL): CABAL_ARGS=--project-dir libraries/Cabal
$(CABAL):
	echo "Building cabal for $(STAGE)"
	mkdir -p $(@D)
	$(CABAL_BUILD) cabal-install:exe:cabal
	ln -svf $$($(CABAL_LIST_BIN) cabal-install:exe:cabal) $@

_build/stage1/bin/ghc: STAGE=stage1
_build/stage1/bin/ghc: CABAL_ARGS=--project-file cabal.project.stage1
_build/stage1/bin/ghc: $(CABAL) $(CONFIG_SCRIPTS)
	mkdir -p $(@D)
	$(CABAL_BUILD) ghc-bin:exe:ghc
	ln -svf $$($(CABAL_LIST_BIN) ghc-bin:exe:ghc) $@

clean:
	rm -rf _build
	git submodule foreach git clean -dxf
