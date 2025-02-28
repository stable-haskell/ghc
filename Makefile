SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

all: stage1-boot

CABAL0 ?= cabal
CABAL  ?= _stage0/bin/cabal
GHC0   ?= ghc

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

_stage1-rts/lib/package.conf.d/rts-fs-1.0.0.0.conf: _stage0/bin/cabal-main-simple 
	@$(LIB)
	mkdir -p "$(@D)/lib/package.conf.d"

	pushd rts-fs || exit
	log ../_stage0/bin/cabal-main-simple configure --builddir "$(OUT)/dist/rts-fs" $(CABAL_CONFIG_FLAGS) --ipid=$(notdir $(@:%.conf=%))
	log ../_stage0/bin/cabal-main-simple build --builddir "$(OUT)/dist/rts-fs"
	log ../_stage0/bin/cabal-main-simple install --builddir "$(OUT)/dist/rts-fs"
	popd

_stage1-rts/lib/package.conf.d/rts-headers-1.0.0.0.conf: _stage0/bin/cabal-main-simple
	@$(LIB)
	mkdir -p "$(@D)"

	pushd rts-headers || exit
	log ../_stage0/bin/cabal-main-simple configure --builddir "$(OUT)/dist/rts-headers" $(CABAL_CONFIG_FLAGS) --ipid=$(notdir $(@:%.conf=%))
	log ../_stage0/bin/cabal-main-simple build --builddir "$(OUT)/dist/rts-headers"
	log ../_stage0/bin/cabal-main-simple install --builddir "$(OUT)/dist/rts-headers"
	popd

# NOTE: The bootstrap is particularly annoying in this step.
#
# rts has a custom config so we need a Cabal to compile Setup.hs, but we cannot
# use the new compiler (not even stage1) for this because we do not have the rts
# yet (as we are just preparing to build it).
#
# We cut the know by using the version of Cabal we had built with the stage0 compiler.
# NOTE: the requirement _stage0/bin/cabal is representative of the fact that we had built the library
#

# _stage0/bin/cabal as evidence that we have build cabal
_stage1-rts/dist/rts/Setup: .EXTRA_PREREQS=_stage0/bin/cabal
_stage1-rts/dist/rts/Setup: rts/Setup.hs
	@$(LIB)
	# FIXME I am using the store as a package db, its path is not quite fixed as it
	# relies on the bootstrap compiler id
	mkdir -p $(@D)
	log $(GHC0) -clear-package-db -global-package-db -package-db _stage0/store/*/package.db -o $@ $<

_stage1-rts/lib/package.conf.d/rts-1.0.0.0.conf: _stage1/bin/ghc rts/configure _stage1-rts/lib/package.conf.d/rts-fs-1.0.0.0.conf _stage1-rts/lib/package.conf.d/rts-headers-1.0.0.0.conf _stage1-rts/dist/rts/Setup _stage1/bin/deriveConstants _stage1/bin/genapply 
	@$(LIB)
	mkdir -p "$(@D)/lib/package.conf.d"

	# for rts setup to find derivedConstants and genApply
	export PATH=$(abspath _stage1/bin):$$PATH

	pushd rts || exit
	log ../_stage1-rts/dist/rts/Setup configure --builddir "$(OUT)/dist/rts" $(CABAL_CONFIG_FLAGS) --ipid=$(notdir $(@:%.conf=%))
	log ../_stage1-rts/dist/rts/Setup build --builddir "$(OUT)/dist/rts"
	log ../_stage1-rts/dist/rts/Setup install --builddir "$(OUT)/dist/rts"
	popd

stage1-rts: GHC = _stage1/bin/ghc
stage1-rts: CABAL_CONFIG_FLAGS += -v --with-compiler $(abspath _stage1/bin/ghc)
stage1-rts: CABAL_CONFIG_FLAGS += --prefix $(OUT)
stage1-rts: CABAL_CONFIG_FLAGS += --package-db $(OUT)/lib/package.conf.d
stage1-rts: OUT ?= $(abspath _stage1-rts)
stage1-rts: $(addprefix _stage1-rts/lib/package.conf.d/,rts-1.0.0.0.conf rts-fs-1.0.0.0.conf rts-headers-1.0.0.0.conf)

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
	
_stage1-boot/src/ghc-internal/.ready: _stage1-boot/src/ghc-internal/ghc-internal.cabal
_stage1-boot/src/ghc-internal/.ready: _stage1-boot/src/ghc-internal/src/GHC/Internal/Prim.hs
_stage1-boot/src/ghc-internal/.ready: _stage1-boot/src/ghc-internal/src/GHC/Internal/PrimopWrappers.hs
_stage1-boot/src/ghc-internal/.ready: _stage1-boot/src/ghc-internal/configure

# targets
STAGE1_BOOT_TARGETS = ghc-internal ghc-experimental ghc-compact base stm system-cxx-std-lib
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
stage1-boot: _stage0/bin/cabal _stage1/bin/ghc _stage1-boot/src/ghc-internal/.ready stage1-rts
	@$(LIB)
	# for rts/configure
	export DERIVE_CONSTANTS=$(abspath _stage1/bin/deriveConstants)
	export GENAPPLY=$(abspath _stage1/bin/genapply)
	log $(CABAL_INSTALL) --lib --package-db=$(abspath _stage1-rts/lib/package.conf.d) --package-env $(OUT) --project-file cabal.project.stage1-boot Cabal
