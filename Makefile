# Top-level Makefile
#
# This file is still _TOO_ large (should be < 100L). There are too many moving
# _global_ parts, most of this should be relegated to the respective packages.
# The whole version replacement therapy is utterly ridiculous. It should be done
# in the respective packages.

# ┌─────────────────────────────────────────────────────────────────────────┐
# │                        GHC Bootstrapping Stages                         │
# ├─────────────────────────────────────────────────────────────────────────┤
# │                                                                         │
# │  Stage 0 (Bootstrap)                                                    │
# │  ┌─────────┐     ┌─────────┐                                            │
# │  │  ghc0   │     │  pkg0   │  (initial boot packages)                   │
# │  │ (binary)│     │         │                                            │
# │  └────┬────┘     └────┬────┘                                            │
# │       │               │                                                 │
# │       └───────┬───────┘                                                 │
# │               ▼                                                         │
# │         ┌─────────┐                                                     │
# │         │  pkg0+  │  (augmented boot packages)                          │
# │         └────┬────┘                                                     │
# │              │                                                          │
# │  ············│························································· │
# │              ▼                                                          │
# │  Stage 1     │                                                          │
# │  ┌─────────┐ │                                                          │
# │  │  ghc1   │◄┘  (built with ghc0, linked with rts0)                     │
# │  │         │                                                            │
# │  └────┬────┘                                                            │
# │       │                                                                 │
# │       │     ┌─────────┐                                                 │
# │       └────►│  pkg1   │  (initially empty, then populated)              │
# │       ┌─────│         │  (built with ghc1)                              │
# │       │     └─────────┘                                                 │
# │       │           ▲                                                     │
# │       │           │ (mutual dependency; ghc1 needs to sees pkg1)        │
# │       ▼           │                                                     │
# │  ┌─────────┐      │                                                     │
# │  │  ghc1   │──────┘                                                     │
# │  │ (uses)  │                                                            │
# │  └────┬────┘                                                            │
# │       │                                                                 │
# │  ·····│································································ │
# │       ▼                                                                 │
# │  Stage 2                                                                │
# │  ┌─────────┐  ┌──────────┐  ┌─────────┐                                 │
# │  │  ghc2   │  │ ghc-pkg2 │  │  ...    │                                 │
# │  │         │  │          │  │         │                                 │
# │  └─────────┘  └──────────┘  └─────────┘                                 │
# │  (built with ghc1, linked with rts1)                                    │
# │                                                                         │
# │  ┌─────────────────────────────────┐                                    │
# │  │        SHIPPED RESULT           │                                    │
# │  │  ┌─────────┐   ┌─────────┐      │                                    │
# │  │  │  pkg1   │ + │  ghc2   │      │                                    │
# │  │  └─────────┘   └─────────┘      │                                    │
# │  └─────────────────────────────────┘                                    │
# │                                                                         │
# │  Notes:                                                                 │
# │  • Binaries: one stage ahead (ghc1 builds pkg1, ghc2 ships with pkg1)   │
# │  • Libraries: one stage below (pkg1 ships with ghc2)                    │
# │  • ghc1 and ghc2 are ABI compatible                                     |
# |  • ghc0 and ghc1 are not guaranteed to be ABI compatible                |
# │  • ghc1 is linked against rts0, ghc2 against rts1                       │
# |  • augmented packages are needed because ghc1 may require newer         |
# |    versions or even new packages, not shipped with the boot compiler    |
# │                                                                         │
# └─────────────────────────────────────────────────────────────────────────┘


# ISSUES:
# - [ ] Where do we get the version number from? The configure script _does_ contain
#       one and sets it, but should it come from the last release tag this branch is
#       contains?
# - [ ] The hadrian folder needs to be removed.
# - [ ] All sublibs should be SRPs in the relevant cabal.project files. No more
#       submodules.

SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c

VERBOSE ?= 0

# If using autoconf feature toggles you can instead run:
#   ./configure --enable-dynamic --enable-profiling --enable-debug
# which generates cabal.project.stage2.settings (imported by cabal.project.stage2).
# The legacy DYNAMIC=1 path still appends flags directly; if both are used the
# configure-generated settings file (import) and these args should agree.
#
# Enable dynamic runtime/linking support when DYNAMIC=1 is passed on the make
# command line. This will build shared libraries, a dynamic RTS (defining
# -DDYNAMIC) and allow tests requiring dynamic linking (e.g. plugins-external)
# to run. The default remains static to keep rebuild cost low.
DYNAMIC ?= 0

ROOT_DIR := $(patsubst %/,%,$(dir $(realpath $(lastword $(MAKEFILE_LIST)))))

#
# System tools
#

CABAL0 = cabal
GHC0   = ghc-9.8.4
PYTHON = python3
SED    = sed
TAR    = tar

#
# Some compiler toolchain settings
#

CABAL_ARGS          =
CC_LINK_OPT         =
GHC_CONFIGURE_ARGS  =

GHC_TOOLCHAIN_ARGS  = \
	--disable-ld-override \
	--cc $(CC) --cxx $(CXX) --ar $(AR) --ld $(LD)

ifeq ($(DYNAMIC),1)
GHC_CONFIGURE_ARGS += --enable-dynamic
endif


#
# Build directories and paths
#

# NOTE: it's tricky to know when and where we need an absolute path or we can
# get away with a relative path. We make BUILD_DIR absolute and all derived
# paths will be absolute too.
BUILD_DIR  := $(abspath _build)
STAGE_DIR   = $(BUILD_DIR)/$(STAGE)
STORE_DIR   = $(STAGE_DIR)/store
LOGS_DIR    = $(STAGE_DIR)/logs

DIST_DIR   := $(BUILD_DIR)/dist

CABAL_DIR  := $(let STAGE,cabal,$(BUILD_DIR))

STAGE1_DIR := $(let STAGE,stage1,$(STAGE_DIR))
STAGE2_DIR := $(let STAGE,stage2,$(STAGE_DIR))
STAGE3_DIR := $(let STAGE,stage3,$(STAGE_DIR))

# HOST_PLATFROM is always from the bootstrap compiler
HOST_PLATFORM := $(shell $(GHC0) --print-host-platform)

CABAL      := $(BUILD_DIR)/cabal/bin/cabal$(EXE_EXT)

STAGE1_PATH := $(let STAGE,stage1,$(STORE_DIR)/host/$(HOST_PLATFORM))
STAGE2_PATH := $(let STAGE,stage2,$(STORE_DIR)/host/$(HOST_PLATFORM))

GHC1        := $(STAGE1_PATH)/bin/ghc$(EXE_EXT)
GHC2        := $(STAGEE2_PATH)/bin/ghc$(EXE_EXT)
# GHC3       := $(DIST3_DIR)/bin/ghc$(EXE_EXT)

# Default value
GHC        := $(GHC0)

#
# GHC --info helper
#

# Get --info from the stage appropriate compiler, parse with bootstrap compiler
define GHC_INFO
$(info [GHC_INFO] $1 $2)$(shell $(1) --info | $(GHC0) -e 'getContents >>= foldMap putStrLn . lookup "$2" . read')
endef

# GHC0_LIBDIR     := $(shell $(GHC0) --print-libdir)

# TARGET_PLATFORM   = $(shell $(GHC) --print-target-platform)

#
# Hadrian settings
#

TARGET_ARCH      = $(call GHC_INFO,$(GHC),target arch)
TARGET_OS        = $(call GHC_INFO,$(GHC),target os)
GIT_COMMIT_ID   := $(shell git rev-parse HEAD)

# # TODO: KILL HADRIAN SETTINGS WITH FIRE
# #
# # It should not exist. Project version should be read from the
# define HADRIAN_SETTINGS
# [ ("hostPlatformArch",    "$(TARGET_ARCH)") \
# , ("hostPlatformOS",      "$(TARGET_OS)") \
# , ("cProjectGitCommitId", "$(GIT_COMMIT_ID)") \
# , ("cProjectVersion",     "9.14") \
# , ("cProjectVersionInt",  "914") \
# , ("cProjectPatchLevel",  "0") \
# , ("cProjectPatchLevel1", "0") \
# , ("cProjectPatchLevel2", "0") \
# ]
# endef

#
# Misc settings
#

# Handle CPUS and THREADS
CPUS_DETECT_SCRIPT := ./mk/detect-cpu-count.sh
CPUS := $(shell if [ -x $(CPUS_DETECT_SCRIPT) ]; then $(CPUS_DETECT_SCRIPT); else echo 2; fi)
THREADS ?= $(shell echo $$(( $(CPUS) + 1 )))

#
# Build macros
#

ifeq ($(MAKE_HOST),x86_64-pc-msys)
# Windows executables require .exe extension for native programs to find them
EXE_EXT := .exe

# FIXME Are we sure about this? Do we need to check if it exists?
CC      = x86_64-w64-mingw32-clang.exe
CXX     = x86_64-w64-mingw32-clang++.exe
LD      = ld.lld.exe

# https://gitlab.haskell.org/ghc/ghc/-/issues/7289#note_646155
CC_LINK_OPT   = -Wl,CRT_fp8.o
CYGPATH       = cygpath --unix -f -
CYGPATH_MIXED = cygpath --mixed -f -
else
CYGPATH       = cat
CYGPATH_MIXED = cat
endif

#
# Logging utilities
#

# LOG_GROUP_START = @echo "::group::$1"
# LOG_GROUP_END = @echo "::endgroup::"

BOLD = $(shell tput bold)
NORMAL = $(shell tput sgr0)

LOG_GROUP_START = @echo "$(BOLD)>>>>> $1$(NORMAL)"
LOG_GROUP_END = @echo ""

LOG = @echo "$(BOLD)[$(STAGE)]$(NORMAL): $(1)"

# CABAL_BUILD
#
# Generic "cabal build"
#
define CABAL_BUILD
	$(CABAL) \
		--remote-repo-cache $(BUILD_DIR)/packages \
		--store-dir $(STORE_DIR) \
		--logs-dir $(LOGS_DIR) \
	build \
		--with-gcc $(CC) \
		--with-ld $(LD) \
		--with-ar $(AR) \
		--with-tar $(TAR) \
		--project-file cabal.project.$(STAGE) \
		--builddir $(STAGE_DIR) \
		--with-compiler $(GHC) \
		--with-build-compiler $(GHC0) \
		--ghc-options "-ghcversion-file=$(ROOT_DIR)/rts/include/ghcversion.h" \
		$(CABAL_ARGS)
endef

# FIX_LIB_NAME
#
# $(1): library target, possibly with sublibrary after colon
#
# pkg     -> pkg-*
# pkg:lib -> pkg-*-lib
FIX_LIB_NAME = $(let pkg lib,$(subst :, ,$(1)),$(pkg)-*$(if $(lib),-$(lib)))

# DIST_COPY_EXE
#
# Copies a executable named $(1) from the local store into the distribution
# directory.
#
# $(1) name of the executable to copy
#
# NOTE: the ending empty line is important
define DIST_COPY_EXE
	$(call LOG,Copying executable $(1) into $(DIST_DIR)/bin)
	@cp -a \
		$(STORE_DIR)/host/$(HOST_PLATFORM)/bin/$(1)$(EXE_EXT) \
		$(DIST_DIR)/bin/$(1)$(EXE_EXT)

endef

# $(1) name of the executable to link
# $(2) platform
define DIST_TARGET_EXE_LINK
	@ln -s \
		$(DIST_DIR)/bin/$(1)$(EXE_EXT) \
		$(DIST_DIR)/bin/$(2)-$(1)$(EXE_EXT)

endef

# DIST_COPY_LIB
#
# Copies a library from the local store into the distribution directory.
#
# $(1) name of the library to copy
#
# NOTE: the ending empty line is important
define DIST_COPY_LIB
	$(call LOG,Copying library $(1) into $(DIST_DIR)/lib)
	@cp -ar \
		$(STORE_DIR)/host/$(TARGET_PLATFORM)/lib/$(call FIX_LIB_NAME,$(1)) \
		$(DIST_DIR)/lib

endef

# DIST_COPY_LIB_CONF
#
# Copies a library packagedb entry from the local store into the distribution
# directory.
#
# $(1) library to copy
#
# NOTE: the ending empty line is important
define DIST_COPY_LIB_CONF
	$(call LOG,Copying $(1) packagedb entry into $(DIST_DIR)/lib)
	@cp -a \
		$(STORE_DIR)/host/$(TARGET_PLATFORM)/package.conf.d/$(call FIX_LIB_NAME,$(1)).conf \
		$(DIST_DIR)/lib/package.conf.d/
	@$(SED) -i \
		-e 's|$(STORE_DIR)/host/$(TARGET_PLATFORM)|\$${pkgroot}/..|g' \
		$(DIST_DIR)/lib/package.conf.d/$(call FIX_LIB_NAME,$(1)).conf

endef
# 	@$(SED) -i \
# 		-e "s|haddock-interfaces:.*|haddock-interfaces: \"\$${pkgroot}/$3/html/libraries/$5/$1.haddock\"|" \
# 		-e "s|haddock-html:.*|haddock-html: \"\$${pkgroot}/$3/html/libraries/$5\"|" \
# 		-e "s|import-dirs:.*|import-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
# 		-e "s|library-dirs:.*|library-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
# 		-e "s|library-dirs-static:.*|library-dirs-static: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
# 		-e "s|dynamic-library-dirs:.*|dynamic-library-dirs: \"\$${pkgroot}/../lib/$4\"|" \
# 		-e "s|data-dir:.*|data-dir: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
# 		-e "s|include-dirs:.*|include-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}/include\"|" \
# 		-e "s|^    /.*||" \
# 		-e "s|^    [A-Z]:.*||" \
# 		$(DIST_DIR)/lib/package.conf.d/$(call FIX_LIB_NAME,$(1)).conf

DIST_COPY_EXES      = $(if $(1),$(foreach exe,$(1),$(call DIST_COPY_EXE,$(exe),$(2))))
DIST_COPY_LIBS      = $(if $(1),$(foreach lib,$(1),$(call DIST_COPY_LIB,$(lib))))
DIST_COPY_LIBS_CONF = $(if $(1),$(foreach lib,$(1),$(call DIST_COPY_LIB_CONF,$(lib))))

#
# Files and targets
#

CONFIGURE_SCRIPTS = \
	configure \
	rts/configure \
	libraries/ghc-internal/configure \
	libraries/libffi-clib/configure \
	libraries/directory/configure \
	libraries/process/configure \
	libraries/terminfo/configure \
	libraries/time/configure \
	libraries/unix/configure

# Files that will be generated by config.status from their .in counterparts
CONFIGURED_FILES := \
	ghc/ghc-bin.cabal \
	compiler/GHC/CmmToLlvm/Version/Bounds.hs \
	compiler/ghc.cabal \
	libraries/ghc-boot/ghc-boot.cabal \
	libraries/ghc-boot-th/ghc-boot-th.cabal \
	libraries/ghc-heap/ghc-heap.cabal \
	libraries/template-haskell/template-haskell.cabal \
	libraries/ghci/ghci.cabal \
	utils/ghc-pkg/ghc-pkg.cabal \
	utils/ghc-iserv/ghc-iserv.cabal \
	utils/runghc/runghc.cabal \
	libraries/ghc-internal/ghc-internal.cabal \
	libraries/ghc-experimental/ghc-experimental.cabal \
	libraries/base/base.cabal \
	rts/include/ghcversion.h

# __  __       _         _                       _
# |  \/  | __ _(_)_ __   | |_ __ _ _ __ __ _  ___| |_
# | |\/| |/ _` | | '_ \  | __/ _` | '__/ _` |/ _ \ __|
# | |  | | (_| | | | | | | || (_| | | | (_| |  __/ |_
# |_|  |_|\__,_|_|_| |_|  \__\__,_|_|  \__, |\___|\__|
#                                      |___/

.PHONY: all
all: stage2

#            _           _       _           _        _ _
#   ___ __ _| |__   __ _| |     (_)_ __  ___| |_ __ _| | |
#  / __/ _` | '_ \ / _` | |_____| | '_ \/ __| __/ _` | | |
# | (_| (_| | |_) | (_| | |_____| | | | \__ \ || (_| | | |
#  \___\__,_|_.__/ \__,_|_|     |_|_| |_|___/\__\__,_|_|_|

$(CABAL): STAGE=cabal
$(CABAL):
	$(call LOG,Building $@)
	$(CABAL0) build -j --with-compiler $(GHC0) --project-dir=libraries/Cabal --builddir=$(STAGE_DIR) cabal-install:exe:cabal
	@mkdir -p $(@D)
	@cp $$($(CABAL0) list-bin -v0 -j --with-compiler $(GHC0) --project-dir=libraries/Cabal --builddir=$(STAGE_DIR) cabal-install:exe:cabal | $(CYGPATH)) $@

#  ____  _                     _
# / ___|| |_ __ _  __ _  ___  / |
# \___ \| __/ _` |/ _` |/ _ \ | |
#  ___) | || (_| | (_| |  __/ | |
# |____/ \__\__,_|\__, |\___| |_|
#                 |___/

# These are configuration variables for stage one

# TODO we should not need genprimops code here, it is needed by compiler/Setup.hs
# but it is also listed as a build-tool-depends in compiler/ghc.cabal so cabal-install
# will build it automatically. The effect of listing genprimops here is that it
# will be included as a host target rather as a build target. So we end up compiling it
# twice for no reason.
STAGE1_EXECUTABLES = \
	deriveConstants \
	genapply \
	genprimopcode \
	ghc \
	ghc-pkg \
	ghc-toolchain-bin \
	hsc2hs \
	unlit

STAGE1_LIBRARIES =

STAGE1_EXTRA_INCLUDE_DIRS ?=
STAGE1_EXTRA_LIB_DIRS	 ?=

stage1: STAGE=stage1
stage1: GHC=$(GHC0)
stage1: $(CABAL) $(CONFIGURED_FILES) | hackage
	$(call LOG,Starting build of $(STAGE) using $(GHC))

	$(call LOG,Building executables $(STAGE1_EXECUTABLES))
	$(CABAL_BUILD) $(addprefix exe:,$(STAGE1_EXECUTABLES))

	$(call LOG,Creating $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/settings)
	@$(STORE_DIR)/host/$(HOST_PLATFORM)/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple $(HOST_PLATFORM) --output-settings -o $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/settings

	$(call LOG,Creating packagedb in $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/package.conf.d)
	@rm -rf $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/package.conf.d
	@$(STORE_DIR)/host/$(HOST_PLATFORM)/bin/ghc-pkg init $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/package.conf.d

	$(call LOG,Finished building $(STAGE))

#  ____  _                     ____
# / ___|| |_ __ _  __ _  ___  |___ \
# \___ \| __/ _` |/ _` |/ _ \   __) |
#  ___) | || (_| | (_| |  __/  / __/
# |____/ \__\__,_|\__, |\___| |_____|
#                 |___/

# These are configuration variables for the second stage

STAGE2_EXECUTABLES = \
	ghc \
	ghc-iserv \
	ghc-pkg \
	haddock \
	hsc2hs \
	hpc \
	hp2ps \
	runghc \
	unlit

STAGE2_LIBRARIES = \
	Cabal \
	Cabal-syntax \
	array \
	base \
	binary \
	bytestring \
	containers \
	deepseq \
	directory \
	exceptions \
	filepath \
	file-io \
	ghc \
	ghc-bignum \
	ghc-boot \
	ghc-boot-th \
	ghc-compact \
	ghc-heap \
	ghc-prim \
	ghci \
	haskeline \
	hpc \
	integer-gmp \
	libffi-clib \
	mtl \
	os-string \
	parsec \
	pretty \
	process \
	rts \
	rts:nonthreaded-debug \
	rts:nonthreaded-nodebug \
	rts:threaded-debug \
	rts:threaded-nodebug \
	semaphore-compat \
	stm \
	system-cxx-std-lib \
	template-haskell \
	terminfo \
	text \
	time \
	transformers \
	unix \
	xhtml

STAGE2_EXTRA_INCLUDE_DIRS ?=
STAGE2_EXTRA_LIB_DIRS     ?=

stage2: STAGE=stage2
stage2: GHC=$(GHC1)
stage2: TARGET_PLATFORM:=$(HOST_PLATFORM)

# These are environment variables used by the configure scripts
# in the rts and ghc-internal packages. We could set an overall
# PATH but I think it's better to be explicit about what we need.
stage2: export DERIVE_CONSTANTS=$(STAGE1_PATH)/bin/deriveConstants
stage2: export GENAPPLY=$(STAGE1_PATH)/bin/genapply
stage2: export GHC=$(GHC1)

stage2: $(CABAL) $(CONFIGURED_FILES) | stage1
	$(call LOG,Starting build of $(STAGE) using $(GHC))

	$(call LOG,Building rts)
	$(CABAL_BUILD) rts

	$(call LOG,Building executables $(STAGE2_EXECUTABLES))
	$(CABAL_BUILD) $(addprefix exe:,$(STAGE2_EXECUTABLES))

	$(call LOG,Building libraries $(filter-out rts%,$(STAGE2_LIBRARIES)))
	$(CABAL_BUILD) $(STAGE2_LIBRARIES)

	$(call LOG,Building distribution in $(DIST_DIR))
	@rm -rf $(DIST_DIR)

	@mkdir -p $(DIST_DIR)/bin
	$(call DIST_COPY_EXES,$(STAGE2_EXECUTABLES))

	@mkdir -p $(DIST_DIR)/lib
	$(call DIST_COPY_LIBS,$(filter-out system-cxx-std-lib%,$(STAGE2_LIBRARIES)))

	@mkdir -p $(DIST_DIR)/lib/package.conf.d
	$(call DIST_COPY_LIBS_CONF,$(STAGE2_LIBRARIES))

	$(call LOG,Creating $(DIST_DIR)/lib/settings)
	@cp $(STAGE1_PATH)/lib/settings $(DIST_DIR)/lib/settings

	$(call LOG,Refreshing $(DIST_DIR)/lib/package.conf.d cache)
	@$(DIST_DIR)/bin/ghc-pkg recache --package-db $(DIST_DIR)/lib/package.conf.d

	$(call LOG,Verifying $(DIST_DIR)/lib/package.conf.d)
	@$(DIST_DIR)/bin/ghc-pkg check --package-db $(DIST_DIR)/lib/package.conf.d

	$(call LOG,Finished building $(STAGE) in $(DIST_DIR))

#  ____  _                     _____
# / ___|| |_ __ _  __ _  ___  |___ /
# \___ \| __/ _` |/ _` |/ _ \   |_ \
#  ___) | || (_| | (_| |  __/  ___) |
# |____/ \__\__,_|\__, |\___| |____/
#                 |___/

# these are GHC names
# TODO: x86_64-musl-linux -> x86_64-unknown-linux-musl
STAGE3_PLATFORMS := \
	x86_64-musl-linux \
	javascript-unknown-ghcjs \
	wasm32-unknown-wasi

STAGE3_EXECUTABLES := \
    ghc \
    ghc-iserv \
    ghc-pkg \
    hp2ps \
    hpc \
    hsc2hs \
    runghc \
    unlit \
    haddock

STAGE3_LIBRARIES := $(STAGE2_LIBRARIES)

STAGE3_x86_64-musl-linux_CC                 = x86_64-unknown-linux-musl-gcc
STAGE3_x86_64-musl-linux_CXX                = x86_64-unknown-linux-musl-g++
STAGE3_x86_64-musl-linux_LD                 = x86_64-unknown-linux-musl-ld
STAGE3_x86_64-musl-linux_AR                 = x86_64-unknown-linux-musl-ar
STAGE3_x86_64-musl-linux_RANLIB             = x86_64-unknown-linux-musl-ranlib
STAGE3_x86_64-musl-linux_CC_OPTS            =
STAGE3_x86_64-musl-linux_CXX_OPTS           =
STAGE3_x86_64-musl-linux_EXTRA_INCLUDE_DIRS =
STAGE3_x86_64-musl-linux_EXTRA_LIB_DIRS     =
STAGE3_x86_64-musl-linux_GHC_TOOLCHAIN_ARGS = $(GHC_TOOLCHAIN_ARGS)

STAGE3_javascript-unknown-ghcjs_CC                 = emcc
STAGE3_javascript-unknown-ghcjs_CC_OPTS            =
STAGE3_javascript-unknown-ghcjs_CXX                = em++
STAGE3_javascript-unknown-ghcjs_CXX_OPTS           =
STAGE3_javascript-unknown-ghcjs_LD                 = ld
STAGE3_javascript-unknown-ghcjs_AR                 = emar
STAGE3_javascript-unknown-ghcjs_RANLIB             = emranlib
STAGE3_javascript-unknown-ghcjs_EXTRA_INCLUDE_DIRS =
STAGE3_javascript-unknown-ghcjs_EXTRA_LIB_DIRS     =
STAGE3_javascript-unknown-ghcjs_GHC_TOOLCHAIN_ARGS = $(GHC_TOOLCHAIN_ARGS)

STAGE3_wasm32-unknown-wasi_CC                 = wasm32-wasi-clang
STAGE3_wasm32-unknown-wasi_CC_OPTS            = -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types
STAGE3_wasm32-unknown-wasi_CXX                = wasm32-wasi-clang++
STAGE3_wasm32-unknown-wasi_CXX_OPTS           = -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types
STAGE3_wasm32-unknown-wasi_AR                 = wasm32-wasi-ar
STAGE3_wasm32-unknown-wasi_RANLIB             = wasm32-wasi-ranlib
STAGE3_wasm32-unknown-wasi_EXTRA_LIB_DIRS     =
STAGE3_wasm32-unknown-wasi_EXTRA_INCLUDE_DIRS =
STAGE3_wasm32-unknown-wasi_GHC_TOOLCHAIN_ARGS = $(GHC_TOOLCHAIN_ARGS) --merge-objs wasm-ld --merge-objs-opt="-r"

# $(1): platform
define stage3

# STAGE           = stage3
# DIST_DIR        = $(BUILD_DIR)/stage3/dist
# GHC3            = $(DIST_DIR)/bin/$(1)-ghc

.PHONY: stage3-$(1)-exe
stage3-$(1)-exe: GHC=$(GHC2)
stage3-$(1)-exe:
	$$(call LOG,Building executables $(STAGE3_EXECUTABLES))
	$$(CABAL_BUILD) $(addprefix exe:,$(STAGE3_EXECUTABLES)) \
		$$(foreach dir,$$(STAGE3_$(1)_EXTRA_LIB_DIRS),--extra-lib-dirs=$$(dir)) \
 		$$(foreach dir,$$(STAGE3_$(1)_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$$(dir))

	$$(call LOG,Copying executables into distribution for target $(1))
	@mkdir -p $$(DIST_DIR)/bin
	$$(call DIST_COPY_EXES,$(STAGE3_EXECUTABLES),$(1))

.PHONY: stage3-$(1)-lib
stage3-$(1)-lib: GHC=$(GHC3)
stage3-$(1)-lib: stage3-$(1)-exe
	$$(call LOG,Building libraries $(STAGE3_LIBRARIES))
	$$(CABAL_BUILD) $(addprefix exe:,$(STAGE3_LIBRARIES)) \
		--with-ghc-options='-B $$(DIST_DIR)/lib/targets/$(1)/lib' \
		--with-hsc2hs=$1-hsc2hs \
		--hsc2hs-options='-x' \
		--configure-option='--host=$1' \
		$$(foreach dir,$$(STAGE3_$(1)_EXTRA_LIB_DIRS),--extra-lib-dirs=$$(dir)) \
 		$$(foreach dir,$$(STAGE3_$(1)_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$$(dir))

	$$(call LOG,Copying libraries into distribution for target $(1)
	@mkdir -p $$(DIST_DIR)/lib/package.conf.d
	$$(call DIST_COPY_LIBS,$(STAGE3_LIBRARIES))
	$$(call DIST_COPY_LIBS_CONF,$(STAGE3_LIBRARIES))

stage3-$(1)-settings: stage3-$(1)-exe
	$$(DIST_DIR)/bin/ghc-toolchain-bin \
		--output-settings \
		--output $$(DIST_DIR)/lib/targets/$(1)/lib/settings \
		--triple $(1) \
		--cc $$(STAGE3_$(1)_CC) \
		$$(foreach opt,$$(STAGE3_$(1)_CC_OPTS),--cc-opt=$$(opt)) \
		--cxx $$(STAGE3_$(1)_CXX) \
		$$(foreach opt,$$(STAGE3_$(1)_CXX_OPTS),--cxx-opt=$$(opt)) \
		--ld $$(STAGE3_$(1)_LD) \
		--ar $$(STAGE3_$(1)_AR) \
		--ranlib $$(STAGE3_$(1)_RANLIB) \
		--disable-ld-override \
		--disable-tables-next-to-code \
		$(GHC_TOOLCHAIN_ARGS)

.PHONY: stage3-$(1)
stage3-$(1): stage3-$(1)-exe stage3-$(1)-lib stage3-$(1)-settings

endef

# $(eval $(call stage3,javascript-unknown-ghcjs))

# $$(foreach dir,$$(STAGE3_$(1)_EXTRA_LIB_DIRS),--extra-lib-dirs=$$(dir))) \
# $$(foreach dir,$$(STAGE3_$(1)_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$$(dir))) )
# STAGE3_$(1)_DIST_BIN := $(foreach exe,$(STAGE3_EXECUTABLES),$(BUILD_DIR)/stage3/dist/bin/$(1)-$(exe)$(EXE_EXT))
# $$(STAGE3_$(1)_DIST_BIN): $(BUILD_DIR)/stage3/dist/bin/$(1)-% : $(BUILD_DIR)/stage2/dist/bin/% | $(BUILD_DIR)/stage3/dist/bin
# 	@ln -sfrv $$< $$@

# .PHONY: stage3-$(1)-lib
# stage3-$(1)-lib: $(BUILD_DIR)/stage3/dist/bin/$(1)-ghc $(BUILD_DIR)/stage3/dist/lib/targets/$(1)/lib/settings
# 	$$(CABAL_BUILD) \
# 		--with-compiler $(BUILD_DIR)/stage3/dist/bin/$(1)-ghc \
# 		--with-build-compiler $(BUILD_DIR)/stage2/dist/bin/ghc \
# 		--with-ghc-options='-B $(BUILD_DIR)/stage3/dist/lib/targets/$(1)/lib' \
# 		--with-hsc2hs=$1-hsc2hs \
# 		--hsc2hs-options='-x' \
# 		--configure-option='--host=$1' \
# 		$(STAGE3_LIBS)

# .PHONY: stage3-$(1)-settings
# stage3-$(1)-settings: $(BUILD_DIR)/stage3/dist/lib/targets/$(1)/lib/settings

# $(BUILD_DIR)/stage3/dist/lib/targets/$(1)/lib/settings: $(BUILD_DIR)/stage1/dist/bin/ghc-toolchain-bin | $(BUILD_DIR)/stage3/dist/lib/targets/$(1)/lib
# 	$(BUILD_DIR)/stage1/dist/bin/ghc-toolchain-bin \
# 		--output-settings \
# 		--output $$@ \
# 		--triple $(1) \
# 		--cc $$(STAGE3_$(1)_CC) \
# 		$$(foreach opt,$$(STAGE3_$(1)_CC_OPTS),--cc-opt=$$(opt)) \
# 		--cxx $$(STAGE3_$(1)_CXX) \
# 		$$(foreach opt,$$(STAGE3_$(1)_CXX_OPTS),--cxx-opt=$$(opt)) \
# 		--ld $$(STAGE3_$(1)_LD) \
# 		--ar $$(STAGE3_$(1)_AR) \
# 		--ranlib $$(STAGE3_$(1)_RANLIB) \
# 		--disable-ld-override \
# 		--disable-tables-next-to-code \
# 		$(GHC_TOOLCHAIN_ARGS)

# stage3-$(1): stage3-$(1)-exe stage3-$(1)-settings stage3-$(1)-lib

# $(foreach platform,$(STAGE3_PLATFORMS),$(eval $(call stage3,$(platform))))

# stage3-javascript-unknown-ghcjs: $(BUILD_DIR)/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings javascript-unknown-ghcjs-libs $(BUILD_DIR)/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache $(BUILD_DIR)/stage3/lib/targets/javascript-unknown-ghcjs/lib/dyld.mjs $(BUILD_DIR)/stage3/lib/targets/javascript-unknown-ghcjs/lib/post-link.mjs $(BUILD_DIR)/stage3/lib/targets/javascript-unknown-ghcjs/lib/prelude.mjs $(BUILD_DIR)/stage3/lib/targets/javascript-unknown-ghcjs/lib/ghc-interp.js

# .PHONY: stage3
# stage3:
# 	@rm -rf $(BUILD_DIR)/stage3/dist
# 	@mkdir -p $(BUILD_DIR)/stage3/dist/{bin,lib}
# 	@for exe in $(BINDIST3_EXECUTABLES); do ln -svfr $(BUILD_DIR)/stage2/dist/bin/$$exe $(BUILD_DIR)/stage3/dist/bin/$(TARGET_PLATFORM)-$$exe; done
# 	@mkdir -p $(BUILD_DIR)/stage3/dist/lib/targets/x86_64-unknown-linux
# 	ln -svfr $(BUILD_DIR)/stage2/dist/lib $(BUILD_DIR)/stage3/dist/lib/targets/x86_64-unknown-linux/lib

# .PHONY: bindist
# bindist: stage2

# # --- Stage 3 generic ---

# $(BUILD_DIR)/stage3/lib/targets/%/lib/dyld.mjs:
# 	@mkdir -p $(@D)
# 	@cp -f utils/jsffi/dyld.mjs $@
# 	@chmod +x $@

# $(BUILD_DIR)/stage3/lib/targets/%/lib/post-link.mjs:
# 	@mkdir -p $(@D)
# 	@cp -f utils/jsffi/post-link.mjs $@
# 	@chmod +x $@

# $(BUILD_DIR)/stage3/lib/targets/%/lib/prelude.mjs:
# 	@mkdir -p $(@D)
# 	@cp -f utils/jsffi/prelude.mjs $@
# 	@chmod +x $@

# $(BUILD_DIR)/stage3/lib/targets/%/lib/ghc-interp.js:
# 	@mkdir -p $(@D)
# 	@cp -f ghc-interp.js $@


# # --- Stage 3 javascript build ---

# .PHONY: stage3-javascript-unknown-ghcjs



# # --- Stage 3 musl build ---


# # --- Stage 3 wasm build ---

# .PHONY: stage3-wasm32-unknown-wasi
# stage3-wasm32-unknown-wasi: wasm32-unknown-wasi-libs $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/dyld.mjs $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/post-link.mjs $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/prelude.mjs $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/ghc-interp.js

# $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/settings: $(BUILD_DIR)/stage2/lib/targets/wasm32-unknown-wasi $(BUILD_DIR)/stage1/bin/ghc-toolchain-bin
# 	@mkdir -p $(@D)
# 	PATH=/home/hasufell/.ghc-wasm/wasi-sdk/bin:$(PATH) $(BUILD_DIR)/stage1/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple wasm32-unknown-wasi --output-settings -o $@ --cc wasm32-wasi-clang --cxx wasm32-wasi-clang++ --ar ar --ranlib ranlib --ld wasm-ld --merge-objs wasm-ld --merge-objs-opt="-r" --disable-ld-override --disable-tables-next-to-code $(foreach opt,$(WASM_CC_OPTS),--cc-opt=$(opt)) $(foreach opt,$(WASM_CXX_OPTS),--cxx-opt=$(opt))

# $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache: $(BUILD_DIR)/stage3/bin/wasm32-unknown-wasi-ghc-pkg $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/settings wasm32-unknown-wasi-libs
# 	@mkdir -p $(@D)
# 	@rm -rf $(@D)/*
# 	cp -rfp $(BUILD_DIR)/stage3/wasm32-unknown-wasi/packagedb/host/*/* $(@D)
# 	$(BUILD_DIR)/stage3/bin/wasm32-unknown-wasi-ghc-pkg recache

# .PHONY: wasm32-unknown-wasi-libs
# wasm32-unknown-wasi-libs: private GHC=$(abspath $(BUILD_DIR)/stage3/bin/wasm32-unknown-wasi-ghc)
# wasm32-unknown-wasi-libs: private GHC2=$(abspath $(BUILD_DIR)/stage2/bin/ghc)
# wasm32-unknown-wasi-libs: private STAGE=stage3
# wasm32-unknown-wasi-libs: private CC=wasm32-wasi-clang
# wasm32-unknown-wasi-libs: $(BUILD_DIR)/stage3/bin/wasm32-unknown-wasi-ghc-pkg $(BUILD_DIR)/stage3/bin/wasm32-unknown-wasi-ghc $(BUILD_DIR)/stage3/bin/wasm32-unknown-wasi-hsc2hs $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/settings $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/bin/unlit $(BUILD_DIR)/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d
# 	$(call build_cross,wasm32-unknown-wasi)

# # --- Bindist ---

# RTS_SUBLIBS := \
#   nonthreaded-nodebug \
#   nonthreaded-debug \
#   threaded-nodebug \
#   threaded-debug

# # patchpackageconf
# #
# # Hacky function to patch up the paths in the package .conf files
# #
# # $1 = package name (ex: 'bytestring')/lib/package.conf.d
# ghc-pkg check --package-db $(DIST_DIR)
# # TODO: package name is borked for sublibs
# # $2 = path to .conf file
# # $3 = (relative) path from $${pkgroot} to docs directory
# # $4 = host triple
# # $5 = package name and version (ex: bytestring-0.13)
# #
# # NOTE: We must make sure we keep sub-folder structures alive.  There might be
# #       references to $5/build/FOO, we must keep /FOO at the end.  One thing not
# #       retaining this that will break are pubilc sublibraries.
# #
# # FIXME: cabal should just be able to create .conf file properly relocated.  And
# #        allow us to install them into a pre-defined package-db, this would
# #        eliminate this nonsense.
# define patchpackageconf
#     case $5 in \
# 		rts-*-nonthreaded-nodebug) \
# 	      sublib="/nonthreaded-nodebug" ;; \
# 		rts-*-nonthreaded-debug) \
# 	      sublib="/nonthreaded-debug" ;; \
# 		rts-*-threaded-nodebug) \
# 	      sublib="/threaded-nodebug" ;; \
# 		rts-*-threaded-debug) \
# 	      sublib="/threaded-debug" ;; \
# 		*) \
# 		  sublib="" ;; \
# 	esac ; \
# 	$(SED) -i \
# 		-e "s|haddock-interfaces:.*|haddock-interfaces: \"\$${pkgroot}/$3/html/libraries/$5/$1.haddock\"|" \
# 		-e "s|haddock-html:.*|haddock-html: \"\$${pkgroot}/$3/html/libraries/$5\"|" \
#         -e "s|import-dirs:.*|import-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
# 		-e "s|library-dirs:.*|library-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
# 		-e "s|library-dirs-static:.*|library-dirs-static: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
# 		-e "s|dynamic-library-dirs:.*|dynamic-library-dirs: \"\$${pkgroot}/../lib/$4\"|" \
# 		-e "s|data-dir:.*|data-dir: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
# 		-e "s|include-dirs:.*|include-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}/include\"|" \
# 		-e "s|^    /.*||" \
# 		$2
# endef

# # $1 = triplet
# define copycrosslib
# 	@cp -rfp $(BUILD_DIR)/stage3/lib/targets/$1 $(BUILD_DIR)/bindist/lib/targets/
# 	@cd $(BUILD_DIR)/bindist/lib/targets/$1/lib/package.conf.d ; \
# 		for pkg in *.conf ; do \
# 		  pkgname=`echo $${pkg} | $(SED) 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
# 		  pkgnamever=`echo $${pkg} | $(SED) 's/\.conf//'` ; \
# 		  mkdir -p $(CURDIR)/$(BUILD_DIR)/bindist/lib/targets/$1/lib/$1/$${pkg%.conf} && \
# 	      cp -rfp $(CURDIR)/$(BUILD_DIR)/stage3/$1/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/$(BUILD_DIR)/bindist/lib/targets/$1/lib/$1/$${pkg%.conf}/ && \
# 	      if [ $${pkgname} = "libffi-clib" ] ; then \
# 		    ffi_incdir=`$(CURDIR)/$(BUILD_DIR)/bindist/bin/$1-ghc-pkg field libffi-clib include-dirs | grep '/libffi-clib/src/' | sed 's|.*$(CURDIR)/||'` ; \
# 		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
# 			$(call copy_headers,ffitarget.h,$(CURDIR)/$${ffi_incdir},libffi-clib,$(CURDIR)/$(BUILD_DIR)/bindist/bin/$1-ghc-pkg) ; \
# 	      else \
# 		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
# 	      fi ; \
# 		done
# endef

# # Target for creating the final binary distribution directory
# #$(BUILD_DIR)/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
# $(BUILD_DIR)/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
# 	@echo "::group::Creating binary distribution in $@"
# 	@mkdir -p $@/bin
# 	@mkdir -p $@/lib
# 	# Copy executables from stage2 bin
# 	@cp -rfp $(BUILD_DIR)/stage2/bin/* $@/bin/
# 	# Copy libraries and settings from stage2 lib
# 	@cp -rfp $(BUILD_DIR)/stage2/lib/{package.conf.d,settings,template-hsc.h} $@/lib/
# 	@mkdir -p $@/lib/$(HOST_PLATFORM)
# 	@cd $@/lib/package.conf.d ; \
# 		for pkg in *.conf ; do \
# 		  pkgname=`echo $${pkg} | $(SED) 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
# 		  pkgnamever=`echo $${pkg} | $(SED) 's/\.conf//'` ; \
# 		  mkdir -p $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
# 		  cp -rfp $(CURDIR)/$(BUILD_DIR)/stage2/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
# 	      if [ $${pkgname} = "libffi-clib" ] ; then \
# 		    ffi_incdir=`$(CURDIR)/$@/bin/ghc-pkg field libffi-clib include-dirs | grep '/libffi-clib/src/' | sed 's|.*$(CURDIR)/||'` ; \
# 		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$(HOST_PLATFORM),$${pkgnamever}) ; \
# 			$(call copy_headers,ffitarget.h,$(CURDIR)/$${ffi_incdir},libffi-clib,$(CURDIR)/$@/bin/ghc-pkg) ; \
# 	      else \
# 		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$(HOST_PLATFORM),$${pkgnamever}) ; \
# 	      fi ; \
# 		done
# 	# Copy driver usage files
# 	@cp -rfp driver/ghc-usage.txt $@/lib/
# 	@cp -rfp driver/ghci-usage.txt $@/lib/
# 	@echo "FIXME: Changing 'Support SMP' from YES to NO in settings file"
# 	@$(SED) 's/("Support SMP","YES")/("Support SMP","NO")/' -i.bck $@/lib/settings
# 	# Recache
# 	$@/bin/ghc-pkg recache
# 	# Copy headers
# 	@$(call copy_all_stage2_h,$@/bin/ghc-pkg)
# 	@echo "::endgroup::"

# $(BUILD_DIR)/bindist/ghc.tar.gz: $(BUILD_DIR)/bindist
# 	@tar czf $@ \
# 		--directory=$(BUILD_DIR)/bindist \
# 		$(foreach exe,$(BINDIST_EXECTUABLES),bin/$(exe)) \
# 		lib/ghc-usage.txt \
# 		lib/ghci-usage.txt \
# 		lib/package.conf.d \
# 		lib/settings \
# 		lib/template-hsc.h \
# 		lib/$(HOST_PLATFORM)

# $(BUILD_DIR)/bindist/lib/targets/%: $(BUILD_DIR)/bindist driver/ghc-usage.txt driver/ghci-usage.txt stage3-%
# 	@echo "::group::Creating binary distribution in $@"
# 	@mkdir -p $(BUILD_DIR)/bindist/bin
# 	@mkdir -p $(BUILD_DIR)/bindist/lib/targets
# 	# Symlinks
# 	@cd $(BUILD_DIR)/bindist/bin ; for binary in * ; do \
# 		test -L $$binary || ln -sf $$binary $(@F)-$$binary \
# 		; done
# 	# Copy libraries and settings
# 	@if [ -e $(CURDIR)/$(BUILD_DIR)/bindist/lib/targets/$(@F)/lib/$(@F) ] ; then find $(CURDIR)/$(BUILD_DIR)/bindist/lib/targets/$(@F)/lib/$(@F)/ -mindepth 1 -type f -name "*.so" -execdir mv '{}' $(CURDIR)/$(BUILD_DIR)/bindist/lib/targets/$(@F)/lib/$(@F)/'{}' \; ; fi
# 	$(call copycrosslib,$(@F))
# 	# --help
# 	@cp -rfp driver/ghc-usage.txt $(BUILD_DIR)/bindist/lib/targets/$(@F)/lib/
# 	@cp -rfp driver/ghci-usage.txt $(BUILD_DIR)/bindist/lib/targets/$(@F)/lib/
# 	# Recache
# 	@$(BUILD_DIR)/bindist/bin/$(@F)-ghc-pkg recache
# 	# Copy headers
# 	@$(call copy_all_stage3_h,$(BUILD_DIR)/bindist/bin/$(@F)-ghc-pkg,$(@F))
# 	@echo "::endgroup::"

# $(BUILD_DIR)/bindist/ghc-%.tar.gz: $(BUILD_DIR)/bindist/lib/targets/% $(BUILD_DIR)/bindist/ghc.tar.gz
# 	@triple=`basename $<` ; \
# 		tar czf $@ \
# 		--directory=$(BUILD_DIR)/bindist \
# 		$(foreach exe,$(BINDIST_EXECTUABLES),bin/$${triple}-$(exe)) \
# 		lib/targets/$${triple}

# $(BUILD_DIR)/bindist/cabal.tar.gz: $(BUILD_DIR)/stage0/bin/cabal
# 	@mkdir -p $(BUILD_DIR)/bindist/bin
# 	@cp $^ $(BUILD_DIR)/bindist/bin/cabal
# 	@tar czf $@ \
# 		--directory=$(BUILD_DIR)/bindist \
# 		bin/cabal

# $(BUILD_DIR)/bindist/haskell-toolchain.tar.gz: $(BUILD_DIR)/bindist/cabal.tar.gz $(BUILD_DIR)/bindist/ghc.tar.gz $(BUILD_DIR)/bindist/ghc-javascript-unknown-ghcjs.tar.gz
# 	@tar czf $@ \
# 		--directory=$(BUILD_DIR)/bindist \
# 		$(foreach exe,$(BINDIST_EXECTUABLES),bin/$(exe)) \
# 		lib/ghc-usage.txt \
# 		lib/ghci-usage.txt \
# 		lib/package.conf.d \
# 		lib/settings \
# 		lib/template-hsc.h \
# 		lib/$(HOST_PLATFORM) \
# 		$(foreach exe,$(BINDIST_EXECTUABLES),bin/javascript-unknown-ghcjs-$(exe)) \
# 		lib/targets/javascript-unknown-ghcjs \
# 		bin/cabal

# $(BUILD_DIR)/bindist/tests.tar.gz:
# 	@tar czf $@ \
# 		testsuite

#  _   _            _
# | | | | __ _  ___| | ____ _  __ _  ___
# | |_| |/ _` |/ __| |/ / _` |/ _` |/ _ \
# |  _  | (_| | (__|   < (_| | (_| |  __/
# |_| |_|\__,_|\___|_|\_\__,_|\__, |\___|
#                             |___/

# .PHONY: hackage
hackage: $(BUILD_DIR)/packages/hackage.haskell.org/01-index.tar.gz

$(BUILD_DIR)/packages/hackage.haskell.org/01-index.tar.gz: | $(CABAL)
	$(CABAL) --remote-repo-cache $(BUILD_DIR)/packages update

#   ____             __ _
#  / ___|___  _ __  / _(_) __ _ _   _ _ __ ___
# | |   / _ \| '_ \| |_| |/ _` | | | | '__/ _ \
# | |__| (_) | | | |  _| | (_| | |_| | | |  __/
#  \____\___/|_| |_|_| |_|\__, |\__,_|_|  \___|
#                         |___/

$(CONFIGURE_SCRIPTS) : % : %.ac
	@echo ">>> Running autoreconf $(@D)"
	autoreconf $(@D)
	@echo "::endgroup::"

# Top level configure script.
#
# NOTE: configure scripts in packages with `Build-Type: Configure`
# are run by Cabal not here.
#
# We use --no-create to avoid regenerating files if not needed.
# Each configured file is tracked independently below.
config.status: configure
	@echo ">>> Running $(@D)/configure"
	$(@D)/configure --no-create $(GHC_CONFIGURE_ARGS)
	@echo "::endgroup::"

# Configured files are obtained from their .in counterparts via config.status
$(CONFIGURED_FILES) : % : ./config.status %.in
	./config.status $@

libraries/ghc-boot-th-next/%: libraries/ghc-boot-th/%
	@mkdir -p $(@D)
	@cp -v $< $@

libraries/ghc-boot-th-next/ghc-boot-th-next.cabal: libraries/ghc-boot-th/ghc-boot-th.cabal
	@echo "::group::Synthesizing ghc-boot-th-next (copy & sed from ghc-boot-th)..."
	@mkdir -p $(@D)
	sed -e 's/^name:[[:space:]]*ghc-boot-th$$/name:           ghc-boot-th-next/' $< > $@
	@echo "::endgroup::"

.PHONY: libraries/ghc-boot-th-next
libraries/ghc-boot-th-next: \
	libraries/ghc-boot-th-next/changelog.md \
	libraries/ghc-boot-th-next/LICENSE \
	libraries/ghc-boot-th-next/ghc-boot-th-next.cabal

# --- Clean Targets ---
clean-cabal:
	@echo "::group::Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)/cabal
	@echo "::endgroup::"

clean: clean-stage1 clean-stage2 clean-stage3
	@echo "Not removing stage0 (cabal), use clean-stage0 to remove cabal too."

clean-stage1:
	@echo "::group::Cleaning stage1 build artifacts..."
	rm -rf $(BUILD_DIR)/stage1
	@echo "::endgroup::"

clean-stage2:
	@echo "::group::Cleaning stage2 build artifacts..."
	rm -rf $(BUILD_DIR)/stage2
	@echo "::endgroup::"

clean-stage3:
	@echo "::group::Cleaning stage3 build artifacts..."
	rm -rf $(BUILD_DIR)/stage3
	rm -rf $(BUILD_DIR)/stage2/lib/targets
	@echo "::endgroup::"

distclean: clean
	@echo "::group::Cleaning all generated files (distclean)..."
	rm -rf autom4te.cache
	rm -f config.status config.log config.h aclocal.m4
	rm -f $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES)
	rm -rf libraries/ghc-boot-th-next
	@echo "::endgroup::"

# Default: skip performance tests (can override with SKIP_PERF_TESTS=NO)
SKIP_PERF_TESTS ?= YES
export SKIP_PERF_TESTS

# --- Test Suite Helper Tool Paths & Flags (Hadrian parity light) ---
# We approximate Hadrian's test invocation without depending on Hadrian.
# Bindist places test tools in $(BUILD_DIR)/bindist/bin (created by the bindist target).
TEST_TOOLS_DIR := $(BUILD_DIR)/bindist/bin
TEST_GHC       := $(TEST_TOOLS_DIR)/ghc
TEST_GHC_PKG   := $(TEST_TOOLS_DIR)/ghc-pkg
TEST_HP2PS     := $(TEST_TOOLS_DIR)/hp2ps
TEST_HPC       := $(TEST_TOOLS_DIR)/hpc
TEST_RUN_GHC   := $(TEST_TOOLS_DIR)/runghc

# Canonical GHC flags used by the testsuite (mirrors testsuite/mk/test.mk & Hadrian runTestGhcFlags)
CANONICAL_TEST_HC_OPTS = \
	-dcore-lint -dstg-lint -dcmm-lint -no-user-package-db -fno-dump-with-ways \
	-fprint-error-index-links=never -rtsopts -fno-warn-missed-specialisations \
	-fshow-warning-groups -fdiagnostics-color=never -fno-diagnostics-show-caret \
	-Werror=compat -dno-debug-output

# Build timeout utility (needed for some tests) if not already built.
.PHONY: testsuite-timeout
testsuite-timeout:
	$(MAKE) -C testsuite/timeout

# --- Test Target ---

test: $(BUILD_DIR)/bindist testsuite-timeout
	@echo "::group::Running tests with THREADS=$(THREADS)" >&2
	# If any required tool is missing, testsuite logic will skip related tests.
	TEST_HC='$(TEST_GHC)' \
	GHC_PKG='$(TEST_GHC_PKG)' \
	HP2PS_ABS='$(TEST_HP2PS)' \
	HPC='$(TEST_HPC)' \
	RUNGHC='$(TEST_RUN_GHC)' \
	TEST_CC='$(CC)' \
	TEST_CXX='$(CXX)' \
	TEST_HC_OPTS='$(CANONICAL_TEST_HC_OPTS)' \
	METRICS_FILE='$(CURDIR)/$(BUILD_DIR)/test-perf.csv' \
	SUMMARY_FILE='$(CURDIR)/$(BUILD_DIR)/test-summary.txt' \
	JUNIT_FILE='$(CURDIR)/$(BUILD_DIR)/test-junit.xml' \
	SKIP_PERF_TESTS='$(SKIP_PERF_TESTS)' \
	THREADS='$(THREADS)' \
	$(MAKE) -C testsuite/tests test
	@echo "::endgroup::"

# Inform Make that these are not actual files if they get deleted by other means
.PHONY: clean clean-stage1 clean-stage2 clean-stage3 distclean test
