SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

CABAL0 ?= cabal
CABAL  ?= _stage0/bin/cabal
GHC    ?= ghc

define LIB =
BOLD_RED='\033[1;31m'
RESET='\033[0m'

log() {
  echo -e "$${BOLD_RED}+$${RESET} $$@" >&2
  "$$@"
}
endef

%/configure : %/configure.ac
	@$(LIB)
	log autoreconf $(@D)

CABAL_FLAGS += --store-dir $(OUT)/store --logs-dir $(OUT)/logs

CABAL_INSTALL_FLAGS += --builddir $(OUT)/build
CABAL_INSTALL_FLAGS += --with-compiler $(GHC)
CABAL_INSTALL_FLAGS += --installdir $(OUT)/bin
CABAL_INSTALL_FLAGS += --overwrite-policy=always
# If we copy the executables then ghc will recognise _stage1 as topdir (rather than a path in the store)
CABAL_INSTALL_FLAGS += --install-method=copy

CABAL_INSTALL = $(CABAL) $(CABAL_FLAGS) install $(CABAL_INSTALL_FLAGS)

cabal: _stage0/bin/cabal

_stage0/bin/cabal: OUT ?= $(abspath _stage0)
_stage0/bin/cabal: CABAL=$(CABAL0)
_stage0/bin/cabal:
	@$(LIB)
	log mkdir -p $(@D)
	log $(CABAL_INSTALL) --project-dir libraries/Cabal --project-file cabal.release.project cabal-install:exe:cabal


STAGE1_EXE = ghc ghc-toolchain-bin deriveConstants genprimopcode genapply
STAGE1_BIN := $(addprefix _stage1/bin/,$(STAGE1_EXE))

$(STAGE1_BIN) &: OUT ?= $(abspath _stage1)
$(STAGE1_BIN) &: $(CABAL)
	@$(LIB)
	log mkdir -p $(@D)
	log $(CABAL_INSTALL) --project-file cabal.project.stage1 $(addprefix exe:,$(STAGE1_EXE))

TARGET := $(shell cc -dumpmachine)

# FIXME: why do they all claim ("target has subsections via symbols","NO") for
# macOS? 9.8 seems to claim this as well. This seems wrong and will severely
# impact dead-stripability if ghc does not emit subsection via symbols. wtf.
# I remain that `ghc-toolchain` is a bad tool and should just be a configure
# script for ghc-bin producing a `settings` file according to a passed
# `--target`. I also think most of the settins file values should have sensible
# defaults: install_name_tool = install_name_tool, ... that do not need to be
# explicitly listed in the settings file unless you want to override them.

_stage1/lib/settings: _stage1/bin/ghc-toolchain-bin
	@$(LIB)
	mkdir -p $(@D)
	log _stage1/bin/ghc-toolchain-bin --cc=cc --cxx=c++ --output-settings -t $(TARGET) -o $@

stage1: _stage1/bin/ghc _stage1/lib/settings

stage1-rts: CABAL = _stage0/bin/cabal
stage1-rts: OUT ?= $(abspath _stage1-rts)
stage1-rts: _stage0/bin/cabal stage1 rts/configure libraries/ghc-internal/configure
	@$(LIB)

	flags=(                                     \
	  --with-compiler $(abspath _stage1/bin/ghc) \
	  --prefix "$(OUT)"                         \
	  --package-db "$(OUT)/lib/package.conf.d"  \
	)
	mkdir -p $(OUT)/lib/package.conf.d

	pushd rts-fs || exit
	log ../$(CABAL) act-as-setup --build-type=Simple -- configure --builddir "$(OUT)/dist/rts-fs" "$${flags[@]}"
	log ../$(CABAL) act-as-setup --build-type=Simple -- build --builddir "$(OUT)/dist/rts-fs"
	log ../$(CABAL) act-as-setup --build-type=Simple -- install --builddir "$(OUT)/dist/rts-fs"
	popd

	pushd rts-headers || exit
	log ../$(CABAL) act-as-setup --build-type=Simple -- configure --builddir "$(OUT)/dist/rts-headers" "$${flags[@]}"
	log ../$(CABAL) act-as-setup --build-type=Simple -- build --builddir "$(OUT)/dist/rts-headers"
	log ../$(CABAL) act-as-setup --build-type=Simple -- install --builddir "$(OUT)/dist/rts-headers"
	popd

	export DERIVE_CONSTANTS=$(abspath _stage1/bin/deriveConstants)
	export GENAPPLY=$(abspath _stage1/bin/genapply)
	pushd rts || exit
	log ../$(CABAL) act-as-setup --build-type=Configure -- configure --builddir "$(OUT)/dist/rts" -v "$${flags[@]}"
	log ../$(CABAL) act-as-setup --build-type=Configure -- build --builddir "$(OUT)/dist/rts" -v
	log ../$(CABAL) act-as-setup --build-type=Configure -- install --builddir "$(OUT)/dist/rts" -v
	popd

