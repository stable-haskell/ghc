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

CABAL_BUILD_FLAGS += --builddir $(OUT)/build
CABAL_BUILD_FLAGS += --with-compiler $(GHC)

CABAL_BUILD = $(CABAL) $(CABAL_FLAGS) build $(CABAL_BUILD_FLAGS)

CABAL_INSTALL_FLAGS += --installdir $(OUT)/bin
CABAL_INSTALL_FLAGS += --overwrite-policy=always
# If we copy the executables then ghc will recognise _stage1 as topdir (rather than a path in the store)
CABAL_INSTALL_FLAGS += --install-method=copy

CABAL_INSTALL = $(CABAL) $(CABAL_FLAGS) install $(CABAL_BUILD_FLAGS) $(CABAL_INSTALL_FLAGS)

cabal: _stage0/bin/cabal

CABAL_EXE = cabal cabal-main-simple cabal-main-configure
CABAL_BIN = $(addprefix _stage0/bin/,$(CABAL_EXE))

$(CABAL_BIN) &: OUT ?= $(abspath _stage0)
$(CABAL_BIN) &: CABAL=$(CABAL0)
$(CABAL_BIN) &:
	@$(LIB)
	log mkdir -p $(@D)
	log $(CABAL_INSTALL) --project-dir libraries/Cabal --project-file cabal.release.project $(addprefix exe:,$(CABAL_EXE))

STAGE1_EXE = ghc ghc-toolchain-bin deriveConstants genprimopcode genapply
STAGE1_BIN = $(addprefix _stage1/bin/,$(STAGE1_EXE))

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

#
# TODO: We cannot build the rts with cabal-install because we are passing
# to deriveConstants the headers in rts-headers via a relative path.
# cabal-install's install command makes a source distribution before building
# and this breaks the relative path. The solution is to move the call to
# derivedConstants into Setup.hs where we have access to the installed
# packagedb. I had a version of this then it looked too complex and I moved back
# to configure.ac.
# 
# stage1-rts: CABAL = _stage0/bin/cabal
# stage1-rts: GHC = $(abspath _stage1/bin/ghc)
# stage1-rts: OUT ?= $(abspath _stage1-rts)
# stage1-rts: _stage0/bin/cabal _stage1/bin/ghc _stage1/bin/deriveConstants _stage1/bin/genapply rts/configure
# 	@$(LIB)
# 	# for rts/configure
# 	export DERIVE_CONSTANTS=$(abspath _stage1/bin/deriveConstants)
# 	export GENAPPLY=$(abspath _stage1/bin/genapply)
# 	log $(CABAL_INSTALL) --lib --package-env $(OUT) --project-file cabal.project.stage1-rts all

stage1-rts: CABAL = _stage0/bin/cabal
stage1-rts: CABAL_CONFIG_FLAGS += --with-compiler $(abspath _stage1/bin/ghc)
stage1-rts: CABAL_CONFIG_FLAGS += --prefix $(OUT)
stage1-rts: CABAL_CONFIG_FLAGS += --package-db $(OUT)/lib/package.conf.d
stage1-rts: OUT ?= $(abspath _stage1-rts)
stage1-rts: _stage0/bin/cabal-main-simple _stage0/bin/cabal-main-configure _stage1/bin/ghc _stage1/bin/deriveConstants _stage1/bin/genapply rts/configure
	@$(LIB)
	mkdir -p $(OUT)/lib/package.conf.d

	# for rts/configure
	export DERIVE_CONSTANTS=$(abspath _stage1/bin/deriveConstants)
	export GENAPPLY=$(abspath _stage1/bin/genapply)

	pushd rts-fs || exit
	log ../_stage0/bin/cabal-main-simple configure --builddir "$(OUT)/dist/rts-fs" $(CABAL_CONFIG_FLAGS)
	log ../_stage0/bin/cabal-main-simple build --builddir "$(OUT)/dist/rts-fs"
	log ../_stage0/bin/cabal-main-simple install --builddir "$(OUT)/dist/rts-fs"
	popd

	pushd rts-headers || exit
	log ../_stage0/bin/cabal-main-simple configure --builddir "$(OUT)/dist/rts-headers" $(CABAL_CONFIG_FLAGS)
	log ../_stage0/bin/cabal-main-simple build --builddir "$(OUT)/dist/rts-headers"
	log ../_stage0/bin/cabal-main-simple install --builddir "$(OUT)/dist/rts-headers"
	popd

	pushd rts || exit
	log ../_stage0/bin/cabal-main-configure configure --builddir "$(OUT)/dist/rts" $(CABAL_CONFIG_FLAGS)
	log ../_stage0/bin/cabal-main-configure build --builddir "$(OUT)/dist/rts"
	log ../_stage0/bin/cabal-main-configure install --builddir "$(OUT)/dist/rts"
	popd

_stage1-boot/src/compiler/GHC/Builtin/primops.txt: compiler/GHC/Builtin/primops.txt.pp
	@$(LIB)
	mkdir -p $(@D)
	log cc -E -undef -traditional -P -x c $< >$@

_stage1-boot/src/ghc-internal/ghc-internal.cabal:
	@$(LIB)
	rm -rf $(@D)
	mkdir -p $(@D)
	cp -r libraries/ghc-internal/* $(@D)

_stage1-boot/src/ghc-internal/src/GHC/Internal/Prim.hs: _stage1-boot/src/compiler/GHC/Builtin/primops.txt | _stage1-boot/src/ghc-internal/ghc-internal.cabal
	@$(LIB)
	mkdir -p $(@D)
	log _stage1/bin/genprimopcode --make-haskell-source < $< > $@

_stage1-boot/src/ghc-internal/src/GHC/Internal/PrimopWrappers.hs: _stage1-boot/src/compiler/GHC/Builtin/primops.txt | _stage1-boot/src/ghc-internal/ghc-internal.cabal
	@$(LIB)
	mkdir -p $(@D)
	log _stage1/bin/genprimopcode --make-haskell-wrappers < $< > $@
	
_stage1-boot/src/ghc-internal/.stamp: _stage1-boot/src/ghc-internal/ghc-internal.cabal
_stage1-boot/src/ghc-internal/.stamp: _stage1-boot/src/ghc-internal/src/GHC/Internal/Prim.hs
_stage1-boot/src/ghc-internal/.stamp: _stage1-boot/src/ghc-internal/src/GHC/Internal/PrimopWrappers.hs

# targets
STAGE1_BOOT_TARGETS = rts ghc-internal ghc-experimental ghc-compact base stm system-cxx-std-lib
# shallow compat packages over ghc-internal
STAGE1_BOOT_TARGETS += ghc-prim ghc-bignum integer-gmp template-haskell
# target dependencies
STAGE1_BOOT_TARGETS += ghc-boot-th pretty
# other boot libraries used by tests
STAGE1_BOOT_TARGETS += array binary bytestring Cabal Cabal-syntax containers deepseq directory exceptions file-io filepath hpc mtl os-string parsec process semaphore-compat text time transformers
STAGE1_BOOT_TARGETS += unix
# FIXME: we'd have to install Win32 for Windows target. Maybe --libs could install dependencies too..
# ghc related
STAGE1_BOOT_TARGETS += ghc-boot ghc-heap ghc-platform ghc-toolchain ghci ghc

stage1-boot: CABAL = _stage0/bin/cabal
stage1-boot: GHC = $(abspath _stage1/bin/ghc)
stage1-boot: OUT ?= $(abspath _stage1-boot)
stage1-boot: _stage0/bin/cabal _stage1/bin/ghc _stage1-boot/src/ghc-internal/.stamp _stage1-boot/src/ghc-internal/configure
	@$(LIB)
	# for rts/configure
	export DERIVE_CONSTANTS=$(abspath _stage1/bin/deriveConstants)
	export GENAPPLY=$(abspath _stage1/bin/genapply)
	log $(CABAL_INSTALL) --lib --package-env $(OUT) --project-file cabal.project.stage1-boot $(STAGE1_BOOT_TARGETS)

