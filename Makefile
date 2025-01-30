SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

export CABAL := $(shell cabal list-bin -v0 --project-dir libraries/Cabal cabal-install:exe:cabal)
export CABAL_ARGS_STAGE1 =--project-file cabal.project.stage1
export GHC_STAGE1 := $(shell cabal list-bin -v0 $(CABAL_ARGS_STAGE1) ghc-bin:exe:ghc)

all: $(GHC_STAGE1)

cabal: $(CABAL)
	
$(CABAL):
	cabal build --project-dir libraries/Cabal cabal-install:exe:cabal

CONFIG_SCRIPTS := $(wildcard libraries/*/configure.ac)

$(CONFIG_SCRIPTS:%.ac=%) : % : %.ac
	autoreconf -i -f $(@D)

$(GHC_STAGE1): $(CABAL) $(CONFIG_SCRIPTS)
	$(CABAL) build $(CABAL_ARGS_STAGE1) ghc-bin:exe:ghc

clean:
	rm -rf _build

clean-submodules:
	git submodule foreach git clean -dxf

