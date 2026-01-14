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
# |  • ghc0 and ghc1 are not guaruateed to be ABI compatible                |
# │  • ghc1 is linked against rts0, ghc2 against rts1                       │
# |  • augmented packages are needed because ghc1 may require newer         |
# |    versions or even new pacakges, not shipped with the boot compiler    |
# │                                                                         │
# └─────────────────────────────────────────────────────────────────────────┘


# ISSUES:
# - [ ] Where do we get the version number from? The configure script _does_ contain
#       one and sets it, but should it come from the last release tag this branch is
#       contains?
# - [ ] HADRIAN_SETTINGS needs to be removed.
# - [ ] The hadrian folder needs to be removed.
# - [ ] All sublibs should be SRPs in the relevant cabal.project files. No more
#       submodules.

SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c

VERBOSE ?= 0

# Enable dynamic runtime/linking support when DYNAMIC=1 is passed on the make
# command line. This will build shared libraries, a dynamic RTS (defining
# -DDYNAMIC) and allow tests requiring dynamic linking (e.g. plugins-external)
# to run. The default remains static to keep rebuild cost low.
DYNAMIC ?= 0

# If using autoconf feature toggles you can instead run:
#   ./configure --enable-dynamic --enable-profiling --enable-debug
# which generates cabal.project.stage2.settings (imported by cabal.project.stage2).
# The legacy DYNAMIC=1 path still appends flags directly; if both are used the
# configure-generated settings file (import) and these args should agree.

ROOT_DIR := $(patsubst %/,%,$(dir $(realpath $(lastword $(MAKEFILE_LIST)))))

GHC0 ?= ghc-9.8.4
PYTHON ?= python3
CABAL ?= $(shell which cabal)
SED ?= sed
LD ?= ld

GHC_CONFIGURE_ARGS ?=

ifeq ($(OS),Windows_NT)
EXE_EXT := .exe
else
EXE_EXT :=
endif

# :exploding-head: It turns out override doesn't override the command-line
# value but it overrides Make's normal behavior of ignoring assignments to
# command-line variables. This allows the += operations to append to whatever
# was passed from the command line.

override CABAL_ARGS += \
	--remote-repo-cache _build/packages \
	--store-dir=_build/$(STAGE)/store \
	--logs-dir=_build/$(STAGE)/logs

override CABAL_BUILD_ARGS += \
	-j1 --with-gcc=$(CC) --with-ld=$(LD) \
	--project-file=cabal.project.$(STAGE) \
	--builddir=_build/$(STAGE) \
	--ghc-options="-fhide-source-paths"

ifeq ($(DYNAMIC),1)
GHC_CONFIGURE_ARGS += --enable-dynamic
endif

GHC_TOOLCHAIN_ARGS ?= --disable-ld-override

CABAL_BUILD = $(CABAL) $(CABAL_ARGS) build $(CABAL_BUILD_ARGS)

# Cache GHC0 --info output to avoid zombie processes
# The := forces immediate evaluation, creating the file before GHC0_INFO is used
_ := $(shell mkdir -p _build && $(GHC0) --info > _build/ghc0-info.txt)

# Use file input redirection instead of pipe to avoid zombie processes
define GHC0_INFO
$(shell $(GHC0) -e 'getContents >>= foldMap putStrLn . lookup "$1" . read' < _build/ghc0-info.txt)
endef

HOST_PLATFORM   := $(call GHC0_INFO,Host platform)
TARGET_PLATFORM := $(call GHC0_INFO,target platform string)
TARGET_ARCH     := $(call GHC0_INFO,target arch)
TARGET_OS       := $(call GHC0_INFO,target os)
TARGET_TRIPLE   := $(call GHC0_INFO,Target platform)
GIT_COMMIT_ID   := $(shell git rev-parse HEAD)

GHC1 := _build/stage1/dist/bin/ghc
GHC2 := _build/stage2/store/host/$(TARGET_PLATFORM)/bin/ghc

define HADRIAN_SETTINGS
[ ("hostPlatformArch",    "$(TARGET_ARCH)") \
, ("hostPlatformOS",      "$(TARGET_OS)") \
, ("cProjectGitCommitId", "$(GIT_COMMIT_ID)") \
, ("cProjectVersion",     "9.14") \
, ("cProjectVersionInt",  "914") \
, ("cProjectPatchLevel",  "0") \
, ("cProjectPatchLevel1", "0") \
, ("cProjectPatchLevel2", "0") \
]
endef

export HADRIAN_SETTINGS

# Handle CPUS and THREADS
CPUS_DETECT_SCRIPT := ./mk/detect-cpu-count.sh
CPUS := $(shell if [ -x $(CPUS_DETECT_SCRIPT) ]; then $(CPUS_DETECT_SCRIPT); else echo 2; fi)
THREADS ?= $(shell echo $$(( $(CPUS) + 1 )))

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
# FIXME: This is stupid. Why do we patch versions across multiple libraries? Idiotic.
#        also, why on earth do we use a non standard SnakeCase convention for substitutions
#        when CAPITAL_CASE is the standard?
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

# --- Main Targets ---
all: _build/bindist

# STAGE_UTIL_TARGETS := \
# 	deriveConstants:deriveConstants \
# 	genapply:genapply \
# 	genprimopcode:genprimopcode \
# 	ghc-pkg:ghc-pkg \
# 	hsc2hs:hsc2hs \
# 	rts-headers:rts-headers \
# 	unlit:unlit

# STAGE1_TARGETS := $(STAGE_UTIL_TARGETS) ghc-bin:ghc ghc-toolchain-bin:ghc-toolchain-bin

STAGE1_EXTRA_INCLUDE_DIRS ?=
STAGE1_EXTRA_LIB_DIRS	 ?=

STAGE2_EXTRA_INCLUDE_DIRS ?=
STAGE2_EXTRA_LIB_DIRS	 ?=

# TODO: dedup


# # rts:threaded-nodebug need it for compiling Setup.hs
# STAGE2_UTIL_TARGETS := \
# 	$(STAGE_UTIL_TARGETS) \
# 	ghc-iserv:ghc-iserv \
# 	rts:nonthreaded-debug \
# 	rts:nonthreaded-nodebug \
# 	rts:threaded-nodebug \
# 	hp2ps:hp2ps \
# 	hpc-bin:hpc \
# 	runghc:runghc \
# 	ghc-bignum:ghc-bignum \
# 	ghc-compact:ghc-compact \
# 	ghc-experimental:ghc-experimental \
# 	ghc-toolchain:ghc-toolchain \
# 	integer-gmp:integer-gmp \
# 	system-cxx-std-lib:system-cxx-std-lib \
# 	terminfo:terminfo \
# 	xhtml:xhtml

# These things should be built on demand.
# hp2ps:hp2ps \
# hpc-bin:hpc \
# ghc-iserv:ghc-iserv \
# runghc:runghc \

# This package is just utterly retarded
# I don't understand why this following line somehow breaks the build...
# STAGE2_TARGETS += system-cxx-std-lib:system-cxx-std-lib

# TODO: dedup

# STAGE2_UTIL_EXECUTABLES := \
# 	deriveConstants \
# 	genapply \
# 	genprimopcode \
# 	hsc2hs \
# 	ghc-iserv \
# 	ghc-pkg \
# 	hp2ps \
# 	hpc \
# 	runghc \
# 	unlit

# BINDIST_EXECTUABLES := \
# 	ghc \
# 	ghc-iserv \
# 	ghc-pkg \
# 	hp2ps \
# 	hpc \
# 	hsc2hs \
# 	runghc \
# 	unlit

# STAGE3_LIBS := \
#     rts:nonthreaded-nodebug \
# 	Cabal \
# 	Cabal-syntax \
# 	array \
# 	base \
# 	binary \
# 	bytestring \
# 	containers \
# 	deepseq \
# 	directory \
# 	exceptions \
# 	file-io \
# 	filepath \
# 	ghc-bignum \
# 	ghci \
# 	hpc \
# 	integer-gmp \
# 	mtl \
# 	os-string \
# 	parsec \
# 	pretty \
# 	process \
# 	stm \
# 	template-haskell \
# 	text \
# 	time \
# 	transformers \
# 	xhtml

# --- Stage 1 build ---

# LOG_GROUP_START = @echo "::group::$1"
# LOG_GROUP_END = @echo "::endgroup::"

BOLD = $(shell tput bold)
NORMAL = $(shell tput sgr0)

LOG_GROUP_START = @echo "$(BOLD)>>>>> $1$(NORMAL)"
LOG_GROUP_END = @echo ""

# ---- Stage 1 build ----

STAGE1_EXECUTABLES := \
	deriveConstants \
	genapply \
	genprimopcode \
	ghc \
	ghc-pkg \
	ghc-toolchain-bin \
	hsc2hs \
	unlit

_build/stage1/% : private STAGE=stage1
_build/stage1/% : private CABAL_BUILD+=--with-compiler $(GHC0) --with-build-compiler $(GHC0)

.PHONY: stage1-dist-dirs
stage1-dist-dirs:
	@mkdir -p _build/stage1/dist/{bin,lib}

STAGE1_STORE_BIN := $(addprefix _build/stage1/store/host/$(TARGET_PLATFORM)/bin/,$(addsuffix $(EXE_EXT),$(STAGE1_EXECUTABLES)))

$(STAGE1_STORE_BIN) &: | hackage
	$(CABAL_BUILD) $(addprefix exe:,$(STAGE1_EXECUTABLES))

STAGE1_DIST_BIN := $(addprefix _build/stage1/dist/bin/,$(addsuffix $(EXE_EXT),$(STAGE1_EXECUTABLES)))

$(STAGE1_DIST_BIN) : _build/stage1/dist/bin/% : _build/stage1/store/host/$(TARGET_PLATFORM)/bin/% | stage1-dist-dirs
	@cp -av $< $@

_build/stage1/dist/lib/package.conf.d: _build/stage1/dist/bin/ghc-pkg | stage1-dist-dirs
	@rm -rf $@
	_build/stage1/dist/bin/ghc-pkg init $@

_build/stage1/dist/lib/settings: _build/stage1/dist/bin/ghc-toolchain-bin | stage1-dist-dirs
	_build/stage1/dist/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple $(TARGET_TRIPLE) --cc $(CC) --cxx $(CXX) --output-settings -o $@

.PHONY: stage1-bin
stage1-bin: $(STAGE1_DIST_BIN)

.PHONY: stage1
stage1: stage1-bin _build/stage1/dist/lib/package.conf.d _build/stage1/dist/lib/settings

# --- Stage 2 build ---

STAGE2_EXECUTABLES := \
	ghc \
	ghc-iserv \
	ghc-pkg \
	haddock \
	hsc2hs \
	hpc \
	hp2ps \
	runghc \
	unlit

STAGE2_LIBRARIES := \
	integer-gmp \
	threaded-debug \
	nonthreaded-debug \
	system-cxx-std-lib \
	terminfo \
	xhtml

STAGE2_DIST_BIN := $(foreach exe,$(STAGE2_EXECUTABLES),_build/stage2/dist/bin/$(exe)$(EXE_EXT))

_build/stage2/% : private STAGE=stage2
_build/stage2/% : private CABAL_BUILD+=--with-compiler $(realpath $(GHC1)) --with-build-compiler $(GHC0) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)"
_build/stage2/% : export PATH:=$(realpath _build/stage1/dist/bin):$(PATH)

stage2-dist-dirs:
	@mkdir -p _build/stage2/dist/{bin,lib}

STAGE2_STORE_BIN := $(addprefix _build/stage2/store/host/$(TARGET_PLATFORM)/bin/,$(addsuffix $(EXE_EXT),$(STAGE2_EXECUTABLES)))

$(STAGE2_STORE_BIN) &: stage1 | hackage
	$(CABAL_BUILD) rts
	$(CABAL_BUILD) $(addprefix lib:,$(STAGE2_LIBRARIES))
	$(CABAL_BUILD) $(addprefix exe:,$(STAGE2_EXECUTABLES))

STAGE2_DIST_BIN := $(addprefix _build/stage2/dist/bin/,$(addsuffix $(EXE_EXT),$(STAGE2_EXECUTABLES)))

$(STAGE2_DIST_BIN) : _build/stage2/dist/bin/% : _build/stage2/store/host/$(TARGET_PLATFORM)/bin/% | stage2-dist-dirs
	@cp -av $< $@

_build/stage2/dist/lib/package.conf.d: stage2-bin | stage2-dist-dirs
	@cp -avr _build/$(STAGE)/store/host/$(TARGET_PLATFORM)/* $(@D)

_build/stage2/dist/lib/settings: _build/stage1/dist/lib/settings | stage2-dist-dirs
	@cp -av $< $@

.PHONY: stage2-bin
stage2-bin: $(STAGE2_DIST_BIN)

stage2: stage2-bin _build/stage2/dist/lib/package.conf.d _build/stage2/dist/lib/settings

# --- Stage 3 build ---

BINDIST3_EXECUTABLES := \
    ghc$(EXE_EXT) \
    ghc-iserv$(EXE_EXT) \
    ghc-pkg$(EXE_EXT) \
    hp2ps$(EXE_EXT) \
    hpc$(EXE_EXT) \
    hsc2hs$(EXE_EXT) \
    runghc$(EXE_EXT) \
    unlit$(EXE_EXT) \
    haddock$(EXE_EXT)

STAGE3_EXECUTABLES := \
    ghc$(EXE_EXT) \
    ghc-iserv$(EXE_EXT) \
    ghc-pkg$(EXE_EXT) \
    hp2ps$(EXE_EXT) \
    hpc$(EXE_EXT) \
    hsc2hs$(EXE_EXT) \
    runghc$(EXE_EXT) \
    unlit$(EXE_EXT) \
    haddock$(EXE_EXT)

# these are GHC names
# TODO: x86_64-musl-linux -> x86_64-unknown-linux-musl
STAGE3_PLATFORMS := \
	x86_64-musl-linux \
	javascript-unknown-ghcjs \
	wasm32-unknown-wasi

STAGE3_x86_64-musl-linux_CC ?= x86_64-unknown-linux-musl-gcc
STAGE3_x86_64-musl-linux_CXX ?= x86_64-unknown-linux-musl-g++
STAGE3_x86_64-musl-linux_LD ?= x86_64-unknown-linux-musl-ld
STAGE3_x86_64-musl-linux_AR ?= x86_64-unknown-linux-musl-ar
STAGE3_x86_64-musl-linux_RANLIB ?= x86_64-unknown-linux-musl-ranlib
STAGE3_x86_64-musl-linux_CC_OPTS ?=
STAGE3_x86_64-musl-linux_CXX_OPTS ?=
STAGE3_x86_64-musl-linux_EXTRA_INCLUDE_DIRS ?=
STAGE3_x86_64-musl-linux_EXTRA_LIB_DIRS ?=
STAGE3_x86_64-musl-linux_GHC_TOOLCHAIN_ARGS ?= $(GHC_TOOLCHAIN_ARGS)

STAGE3_javascript-unknown-ghcjs_CC ?= emcc
STAGE3_javascript-unknown-ghcjs_CC_OPTS ?=
STAGE3_javascript-unknown-ghcjs_CXX ?= em++
STAGE3_javascript-unknown-ghcjs_CXX_OPTS ?=
STAGE3_javascript-unknown-ghcjs_LD ?= ld
STAGE3_javascript-unknown-ghcjs_AR ?= emar
STAGE3_javascript-unknown-ghcjs_RANLIB ?= emranlib
STAGE3_javascript-unknown-ghcjs_EXTRA_INCLUDE_DIRS ?=
STAGE3_javascript-unknown-ghcjs_EXTRA_LIB_DIRS ?=
STAGE3_javascript-unknown-ghcjs_GHC_TOOLCHAIN_ARGS ?= $(GHC_TOOLCHAIN_ARGS)

STAGE3_wasm32-unknown-wasi_CC ?= wasm32-wasi-clang
STAGE3_wasm32-unknown-wasi_CC_OPTS ?= -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types
STAGE3_wasm32-unknown-wasi_CXX ?= wasm32-wasi-clang++
STAGE3_wasm32-unknown-wasi_CXX_OPTS ?= -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types
STAGE3_wasm32-unknown-wasi_AR ?= wasm32-wasi-ar
STAGE3_wasm32-unknown-wasi_RANLIB ?= wasm32-wasi-ranlib
STAGE3_wasm32-unknown-wasi_EXTRA_LIB_DIRS ?=
STAGE3_wasm32-unknown-wasi_EXTRA_INCLUDE_DIRS ?=
STAGE3_wasm32-unknown-wasi_GHC_TOOLCHAIN_ARGS ?= $(GHC_TOOLCHAIN_ARGS) --merge-objs wasm-ld --merge-objs-opt="-r"

STAGE3_EXECUTABLES := $(STAGE2_EXECUTABLES)

_build/stage3/dist/bin:
	@mkdir -p $@

_build/stage3/dist/lib/targets/%/lib:
	@mkdir -p $@

# $(1): platform
define stage3
STAGE3_$(1)_DIST_BIN := $(foreach exe,$(STAGE3_EXECUTABLES),_build/stage3/dist/bin/$(1)-$(exe)$(EXE_EXT))

stage3-$(1)-exe: $$(STAGE3_$(1)_DIST_BIN)

$$(STAGE3_$(1)_DIST_BIN): _build/stage3/dist/bin/$(1)-% : _build/stage2/dist/bin/% | _build/stage3/dist/bin
	@ln -sfrv $$< $$@

.PHONY: stage3-$(1)-lib
stage3-$(1)-lib: _build/stage3/dist/bin/$(1)-ghc _build/stage3/dist/lib/targets/$(1)/lib/settings
	$$(CABAL_BUILD) \
		--with-compiler _build/stage3/dist/bin/$(1)-ghc \
		--with-build-compiler _build/stage2/dist/bin/ghc \
		--with-ghc-options='-B _build/stage3/dist/lib/targets/$(1)/lib' \
		--with-hsc2hs=$1-hsc2hs \
		--hsc2hs-options='-x' \
		--configure-option='--host=$1' \
		$$(foreach dir,$$(STAGE3_$(1)_EXTRA_LIB_DIRS),--extra-lib-dirs=$$(dir)) \
		$$(foreach dir,$$(STAGE3_$(1)_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$$(dir)) \
		$(STAGE3_LIBS)

.PHONY: stage3-$(1)-settings
stage3-$(1)-settings: _build/stage3/dist/lib/targets/$(1)/lib/settings

_build/stage3/dist/lib/targets/$(1)/lib/settings: _build/stage1/dist/bin/ghc-toolchain-bin | _build/stage3/dist/lib/targets/$(1)/lib
	_build/stage1/dist/bin/ghc-toolchain-bin \
		--output-settings \
		--output $$@ \
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

stage3-$(1): stage3-$(1)-exe stage3-$(1)-settings stage3-$(1)-lib
endef

$(foreach platform,$(STAGE3_PLATFORMS),$(eval $(call stage3,$(platform))))

# stage3-javascript-unknown-ghcjs: _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings javascript-unknown-ghcjs-libs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/dyld.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/post-link.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/prelude.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/ghc-interp.js

# .PHONY: stage3
# stage3:
# 	@rm -rf _build/stage3/dist
# 	@mkdir -p _build/stage3/dist/{bin,lib}
# 	@for exe in $(BINDIST3_EXECUTABLES); do ln -svfr _build/stage2/dist/bin/$$exe _build/stage3/dist/bin/$(TARGET_PLATFORM)-$$exe; done
# 	@mkdir -p _build/stage3/dist/lib/targets/x86_64-unknown-linux
# 	ln -svfr _build/stage2/dist/lib _build/stage3/dist/lib/targets/x86_64-unknown-linux/lib

.PHONY: bindist
bindist: stage2
	# @mkdir -p _build/bindist/$(TARGET_PLATFORM)
	# @cp -av _build/stage2/bin/                          _build/bindist/$(TARGET_PLATFROM)/bin
	# @cp -av _build/stage2/store/host/$(TARGET_PLATFORM) _build/bindist/$(TARGET_PLATFORM)/lib

# # --- Stage 3 generic ---

# _build/stage3/lib/targets/%/lib/dyld.mjs:
# 	@mkdir -p $(@D)
# 	@cp -f utils/jsffi/dyld.mjs $@
# 	@chmod +x $@

# _build/stage3/lib/targets/%/lib/post-link.mjs:
# 	@mkdir -p $(@D)
# 	@cp -f utils/jsffi/post-link.mjs $@
# 	@chmod +x $@

# _build/stage3/lib/targets/%/lib/prelude.mjs:
# 	@mkdir -p $(@D)
# 	@cp -f utils/jsffi/prelude.mjs $@
# 	@chmod +x $@

# _build/stage3/lib/targets/%/lib/ghc-interp.js:
# 	@mkdir -p $(@D)
# 	@cp -f ghc-interp.js $@


# # --- Stage 3 javascript build ---

# .PHONY: stage3-javascript-unknown-ghcjs



# # --- Stage 3 musl build ---


# # --- Stage 3 wasm build ---

# .PHONY: stage3-wasm32-unknown-wasi
# stage3-wasm32-unknown-wasi: wasm32-unknown-wasi-libs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache _build/stage3/lib/targets/wasm32-unknown-wasi/lib/dyld.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/post-link.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/prelude.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/ghc-interp.js

# _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings: _build/stage2/lib/targets/wasm32-unknown-wasi _build/stage1/bin/ghc-toolchain-bin
# 	@mkdir -p $(@D)
# 	PATH=/home/hasufell/.ghc-wasm/wasi-sdk/bin:$(PATH) _build/stage1/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple wasm32-unknown-wasi --output-settings -o $@ --cc wasm32-wasi-clang --cxx wasm32-wasi-clang++ --ar ar --ranlib ranlib --ld wasm-ld --merge-objs wasm-ld --merge-objs-opt="-r" --disable-ld-override --disable-tables-next-to-code $(foreach opt,$(WASM_CC_OPTS),--cc-opt=$(opt)) $(foreach opt,$(WASM_CXX_OPTS),--cxx-opt=$(opt))

# _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache: _build/stage3/bin/wasm32-unknown-wasi-ghc-pkg _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings wasm32-unknown-wasi-libs
# 	@mkdir -p $(@D)
# 	@rm -rf $(@D)/*
# 	cp -rfp _build/stage3/wasm32-unknown-wasi/packagedb/host/*/* $(@D)
# 	_build/stage3/bin/wasm32-unknown-wasi-ghc-pkg recache

# .PHONY: wasm32-unknown-wasi-libs
# wasm32-unknown-wasi-libs: private GHC=$(abspath _build/stage3/bin/wasm32-unknown-wasi-ghc)
# wasm32-unknown-wasi-libs: private GHC2=$(abspath _build/stage2/bin/ghc)
# wasm32-unknown-wasi-libs: private STAGE=stage3
# wasm32-unknown-wasi-libs: private CC=wasm32-wasi-clang
# wasm32-unknown-wasi-libs: _build/stage3/bin/wasm32-unknown-wasi-ghc-pkg _build/stage3/bin/wasm32-unknown-wasi-ghc _build/stage3/bin/wasm32-unknown-wasi-hsc2hs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings _build/stage3/lib/targets/wasm32-unknown-wasi/bin/unlit _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d
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
# # $1 = package name (ex: 'bytestring')
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
# 	@cp -rfp _build/stage3/lib/targets/$1 _build/bindist/lib/targets/
# 	@cd _build/bindist/lib/targets/$1/lib/package.conf.d ; \
# 		for pkg in *.conf ; do \
# 		  pkgname=`echo $${pkg} | $(SED) 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
# 		  pkgnamever=`echo $${pkg} | $(SED) 's/\.conf//'` ; \
# 		  mkdir -p $(CURDIR)/_build/bindist/lib/targets/$1/lib/$1/$${pkg%.conf} && \
# 	      cp -rfp $(CURDIR)/_build/stage3/$1/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/_build/bindist/lib/targets/$1/lib/$1/$${pkg%.conf}/ && \
# 	      if [ $${pkgname} = "libffi-clib" ] ; then \
# 		    ffi_incdir=`$(CURDIR)/_build/bindist/bin/$1-ghc-pkg field libffi-clib include-dirs | grep '/libffi-clib/src/' | sed 's|.*$(CURDIR)/||'` ; \
# 		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
# 			$(call copy_headers,ffitarget.h,$(CURDIR)/$${ffi_incdir},libffi-clib,$(CURDIR)/_build/bindist/bin/$1-ghc-pkg) ; \
# 	      else \
# 		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
# 	      fi ; \
# 		done
# endef

# # Target for creating the final binary distribution directory
# #_build/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
# _build/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
# 	@echo "::group::Creating binary distribution in $@"
# 	@mkdir -p $@/bin
# 	@mkdir -p $@/lib
# 	# Copy executables from stage2 bin
# 	@cp -rfp _build/stage2/bin/* $@/bin/
# 	# Copy libraries and settings from stage2 lib
# 	@cp -rfp _build/stage2/lib/{package.conf.d,settings,template-hsc.h} $@/lib/
# 	@mkdir -p $@/lib/$(HOST_PLATFORM)
# 	@cd $@/lib/package.conf.d ; \
# 		for pkg in *.conf ; do \
# 		  pkgname=`echo $${pkg} | $(SED) 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
# 		  pkgnamever=`echo $${pkg} | $(SED) 's/\.conf//'` ; \
# 		  mkdir -p $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
# 		  cp -rfp $(CURDIR)/_build/stage2/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
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

# _build/bindist/ghc.tar.gz: _build/bindist
# 	@tar czf $@ \
# 		--directory=_build/bindist \
# 		$(foreach exe,$(BINDIST_EXECTUABLES),bin/$(exe)) \
# 		lib/ghc-usage.txt \
# 		lib/ghci-usage.txt \
# 		lib/package.conf.d \
# 		lib/settings \
# 		lib/template-hsc.h \
# 		lib/$(HOST_PLATFORM)

# _build/bindist/lib/targets/%: _build/bindist driver/ghc-usage.txt driver/ghci-usage.txt stage3-%
# 	@echo "::group::Creating binary distribution in $@"
# 	@mkdir -p _build/bindist/bin
# 	@mkdir -p _build/bindist/lib/targets
# 	# Symlinks
# 	@cd _build/bindist/bin ; for binary in * ; do \
# 		test -L $$binary || ln -sf $$binary $(@F)-$$binary \
# 		; done
# 	# Copy libraries and settings
# 	@if [ -e $(CURDIR)/_build/bindist/lib/targets/$(@F)/lib/$(@F) ] ; then find $(CURDIR)/_build/bindist/lib/targets/$(@F)/lib/$(@F)/ -mindepth 1 -type f -name "*.so" -execdir mv '{}' $(CURDIR)/_build/bindist/lib/targets/$(@F)/lib/$(@F)/'{}' \; ; fi
# 	$(call copycrosslib,$(@F))
# 	# --help
# 	@cp -rfp driver/ghc-usage.txt _build/bindist/lib/targets/$(@F)/lib/
# 	@cp -rfp driver/ghci-usage.txt _build/bindist/lib/targets/$(@F)/lib/
# 	# Recache
# 	@_build/bindist/bin/$(@F)-ghc-pkg recache
# 	# Copy headers
# 	@$(call copy_all_stage3_h,_build/bindist/bin/$(@F)-ghc-pkg,$(@F))
# 	@echo "::endgroup::"

# _build/bindist/ghc-%.tar.gz: _build/bindist/lib/targets/% _build/bindist/ghc.tar.gz
# 	@triple=`basename $<` ; \
# 		tar czf $@ \
# 		--directory=_build/bindist \
# 		$(foreach exe,$(BINDIST_EXECTUABLES),bin/$${triple}-$(exe)) \
# 		lib/targets/$${triple}

# _build/bindist/cabal.tar.gz: _build/stage0/bin/cabal
# 	@mkdir -p _build/bindist/bin
# 	@cp $^ _build/bindist/bin/cabal
# 	@tar czf $@ \
# 		--directory=_build/bindist \
# 		bin/cabal

# _build/bindist/haskell-toolchain.tar.gz: _build/bindist/cabal.tar.gz _build/bindist/ghc.tar.gz _build/bindist/ghc-javascript-unknown-ghcjs.tar.gz
# 	@tar czf $@ \
# 		--directory=_build/bindist \
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

# _build/bindist/tests.tar.gz:
# 	@tar czf $@ \
# 		testsuite

# --- Hackage ---

# .PHONY: hackage
hackage: _build/packages/hackage.haskell.org/01-index.tar.gz

# Always run cabal update. This makes sure that the index file won't go stale,
# whatever index-state we set in the project file. Reproducibility is left to
# index-state.
.PHONY: _build/packages/hackage.haskell.org/01-index.tar.gz
_build/packages/hackage.haskell.org/01-index.tar.gz:
	$(CABAL) --remote-repo-cache _build/packages update

# --- Configure and source preparation ---

$(CONFIGURE_SCRIPTS) : % : %.ac
	@echo ">>> Running autoreconf $(@D)"
	autoreconf $(@D)
	@echo "::endgroup::"

# Top level configure script.
#
# NOTE: other configure scripts are run by Cabal
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
clean-cabal: clean-stage0

clean-stage0:
	@echo "::group::Cleaning build artifacts..."
	rm -rf _build/stage0
	rm -f libraries/ghc-boot-th-next/ghc-boot-th-next.cabal
	rm -f libraries/ghc-boot-th-next/ghc-boot-th-next.cabal.in
	rm -f libraries/ghc-boot-th-next/.synth-stamp
	@echo "::endgroup::"

clean: clean-stage1 clean-stage2 clean-stage3
	@echo "Not removing stage0 (cabal), use clean-stage0 to remove cabal too."

clean-stage1:
	@echo "::group::Cleaning stage1 build artifacts..."
	rm -rf _build/stage1
	@echo "::endgroup::"

clean-stage2:
	@echo "::group::Cleaning stage2 build artifacts..."
	rm -rf _build/stage2
	@echo "::endgroup::"

clean-stage3:
	@echo "::group::Cleaning stage3 build artifacts..."
	rm -rf _build/stage3
	rm -rf _build/stage2/lib/targets
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
# Bindist places test tools in _build/bindist/bin (created by the bindist target).
TEST_TOOLS_DIR := _build/bindist/bin
TEST_GHC       := $(abspath $(TEST_TOOLS_DIR)/ghc)
TEST_GHC_PKG   := $(abspath $(TEST_TOOLS_DIR)/ghc-pkg)
TEST_HP2PS     := $(abspath $(TEST_TOOLS_DIR)/hp2ps)
TEST_HPC       := $(abspath $(TEST_TOOLS_DIR)/hpc)
TEST_RUN_GHC   := $(abspath $(TEST_TOOLS_DIR)/runghc)

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

test: _build/bindist testsuite-timeout
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
	METRICS_FILE='$(CURDIR)/_build/test-perf.csv' \
	SUMMARY_FILE='$(CURDIR)/_build/test-summary.txt' \
	JUNIT_FILE='$(CURDIR)/_build/test-junit.xml' \
	SKIP_PERF_TESTS='$(SKIP_PERF_TESTS)' \
	THREADS='$(THREADS)' \
	$(MAKE) -C testsuite/tests test
	@echo "::endgroup::"

# Inform Make that these are not actual files if they get deleted by other means
.PHONY: clean clean-stage1 clean-stage2 clean-stage3 distclean test all
