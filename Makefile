# The Makefile to build GHC with cabal
#
# Pre-requisites:
# There are a few tools we expect to exist prior to building this.  These are
# documented here.  If you end up creating more dependencies, be explicit about
# them here at the start.
#
#  - make
#  - ghc (a recent enough GHC to function at the boostrap compiler)
#  - cabal (a recent enough cabal-install executable to build the cabal pkgs
#    and the necessary changes for dual stage, ...)
#
# Build Plan:
# In general GHC is built in two stages to ensure the compiler is linked against
# libraries built by the same compiler.  This allows the compiler to have the
# same abi as the code it produces.  This does not work for cross compilers, as
# we can't build a compiler for the host platform using a host->target compiler.
#
# Stage 0 (boostrap stage)
# In this stage we build just enough of the compiler dependencies to build the
# Stage 1 compiler (ghc executable).  We want to build the ghc executable, but
# may need to build extra libraries that are newer than the ones that come with
# the bootstrap compiler.  This crucially also means, these libraries are stable
# (as long as their source doesn't change) to be cached.
#
# Stage 1
# We now have the boostrap libraries and a compiler (ghc1) built with (ghc0) the
# ghc0-libraries + extra libraries built with ghc0. Thus we have a ghc1 but no
# libraries yet.  We now build all the libraries required to build the compiler
# (from ø) using ghc1. First we need to build the rts with ghc1. Then build all
# the libraries necessary to build ghc2. And then build ghc2 using ghc1 with the
# newly build libraries.
#
# Stage 2
# This gives us our stage2 compiler (ghc2, all it's deps built with ghc1).  This
# is the pair we package up.

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

all: stage1-boot

CABAL  ?= cabal
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
CABAL_BUILD_FLAGS += --with-compiler $(GHC) --with-hc-pkg $(GHC)-pkg --with-build-compiler $(GHC0) --with-build-hc-pkg $(GHC0)-pkg

CABAL_BUILD = $(CABAL) $(CABAL_FLAGS) build $(CABAL_BUILD_FLAGS)

CABAL_INSTALL_FLAGS += --installdir $(OUT)/bin
CABAL_INSTALL_FLAGS += --overwrite-policy=always
# If we copy the executables then ghc will recognise _stage1 as topdir (rather than a path in the store)
CABAL_INSTALL_FLAGS += --install-method=copy
# stop cabal from being a fucking turd.
CABAL_INSTALL_FLAGS += --write-ghc-environment-files=never

CABAL_INSTALL = $(CABAL) $(CABAL_FLAGS) install $(CABAL_BUILD_FLAGS) $(CABAL_INSTALL_FLAGS)

STAGE1_EXE = ghc ghc-pkg ghc-toolchain-bin deriveConstants genprimopcode genapply unlit
STAGE1_BIN = $(addprefix _stage1/bin/,$(STAGE1_EXE))

$(STAGE1_BIN) &: OUT ?= $(abspath _stage1)
$(STAGE1_BIN) &: override GHC=$(GHC0)
$(STAGE1_BIN) &:
	@$(LIB)
	log mkdir -p $(@D)
	log export HADRIAN_SETTINGS="$$(cat ./HADRIAN_SETTINGS)"
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
	log _stage1/bin/ghc-toolchain-bin --cc=cc --cxx=c++ --install-name-tool=install_name_tool --otool=otool --output-settings -t $(TARGET) -o $@

_stage2/lib/settings: _stage1/bin/ghc-toolchain-bin
	@$(LIB)
	mkdir -p $(@D)
	log _stage1/bin/ghc-toolchain-bin --cc=cc --cxx=c++ --install-name-tool=install_name_tool --otool=otool --output-settings -t $(TARGET) -o $@


stage1: _stage1/bin/ghc _stage1/lib/settings


# this should be in the setup of compiler.cabal?
_stage1/src/compiler/GHC/Builtin/primops.txt: compiler/GHC/Builtin/primops.txt.pp
	@$(LIB)
	mkdir -p $(@D)
	log cc -E -undef -traditional -P -x c $< >$@

# This should be in the setup of ghc-internal.cabal?
_stage1/src/ghc-internal/ghc-internal.cabal:
	@$(LIB)
	rm -rf $(@D)
	mkdir -p $(@D)
	cp -r libraries/ghc-internal/* $(@D)

_stage1/src/ghc-internal/src/GHC/Internal/Prim.hs: _stage1/src/compiler/GHC/Builtin/primops.txt | _stage1/src/ghc-internal/ghc-internal.cabal
	@$(LIB)
	mkdir -p $(@D)
	log _stage1/bin/genprimopcode --make-haskell-source < $< > $@

_stage1/src/ghc-internal/src/GHC/Internal/PrimopWrappers.hs: _stage1/src/compiler/GHC/Builtin/primops.txt | _stage1/src/ghc-internal/ghc-internal.cabal
	@$(LIB)
	mkdir -p $(@D)
	log _stage1/bin/genprimopcode --make-haskell-wrappers < $< > $@

_stage1/src/ghc-internal/.ready: _stage1/src/ghc-internal/ghc-internal.cabal
_stage1/src/ghc-internal/.ready: _stage1/src/ghc-internal/src/GHC/Internal/Prim.hs
_stage1/src/ghc-internal/.ready: _stage1/src/ghc-internal/src/GHC/Internal/PrimopWrappers.hs
_stage1/src/ghc-internal/.ready: _stage1/src/ghc-internal/configure

# We need this folder to exist prior to running anything with _stage1/bin/ghc and cabal,
# because that will result in ghc-pkg being invoked. That one looking into the ../lib/settings file.
# Finding that package.conf.d is the global package db, and then falling over itself because that
# folder doesn't exist after building the ghc executable. So here we link the cabal _store_ package-db
# into the _stage1/lib/package.conf.d folder, thus the new ghc compiler will always have access to all
# the packages it built on the way to ghc stage2.
_stage1/lib/package.conf.d:
	@$(LIB)
	mkdir -p _stage1/lib _stage2/lib
	log _stage1/bin/ghc-pkg init $(PWD)/_stage2/store/$$(_stage1/bin/ghc --info | grep "Project Unit Id" | cut -d'"' -f4)/package.db
	log ln -sf $(PWD)/_stage2/store/$$(_stage1/bin/ghc --info | grep "Project Unit Id" | cut -d'"' -f4)/package.db _stage1/lib/package.conf.d
	log ln -sf $(PWD)/_stage2/store/$$(_stage1/bin/ghc --info | grep "Project Unit Id" | cut -d'"' -f4)/package.db _stage2/lib/package.conf.d

# Now that we've jammed the rts's into the stage1 compiler package.db, we can go on and just build the rest of the stage2 compiler.
# This will automatically also mean, we end up with the relevant packages to ship alognside the stage2 compiler in the _stage1/lib/package.conf.d.
STAGE2_EXE = ghc ghc-pkg
STAGE2_BIN = $(addprefix _stage2/bin/,$(STAGE2_EXE))

$(STAGE2_BIN) &: OUT ?= $(abspath _stage2)
$(STAGE2_BIN) &: GHC = $(abspath _stage1/bin/ghc)
$(STAGE2_BIN) &: GHC0 = $(shell which ghc)
$(STAGE2_BIN) &: _stage1/bin/ghc _stage1/lib/settings rts/configure _stage1/lib/package.conf.d _stage1/src/ghc-internal/.ready
	@$(LIB)
	log mkdir -p $(@D)
	log export HADRIAN_SETTINGS="$$(cat ./HADRIAN_SETTINGS)"
	log $(CABAL_INSTALL) --package-db=$(abspath _stage1/lib/package.conf.d) --project-file cabal.project.stage2 $(addprefix exe:,$(STAGE2_EXE))

# 1. git clean -xfd
# 2. make _stage1/bin/ghc
# 3. make _stage2/bin/ghc
# 4. make _stage2/lib/settings