MK_TOOLCHAIN := 1

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

GHC0 ?= ghc-9.8.4
PYTHON ?= python3
CABAL ?= cabal
SED ?= sed

ifeq ($(OS),Windows_NT)
CC := x86_64-w64-mingw32-clang.exe
CXX := x86_64-w64-mingw32-clang++.exe
CC_LINK_OPT := -Wl,CRT_fp8.o
LD := ld.lld.exe
CYGPATH = cygpath --unix -f -
CYGPATH_MIXED = cygpath --mixed -f -
# Windows executables require .exe extension for native programs to find them
EXE_EXT := .exe
else
CYGPATH_MIXED = cat
CYGPATH = cat
CC_LINK_OPT ?=
LD ?= ld
EXE_EXT :=
endif

EMCC ?= emcc
EMCXX ?= em++
EMAR ?= emar
EMRANLIB ?= emranlib

GHC_CONFIGURE_ARGS ?=

EXTRA_LIB_DIRS ?=
EXTRA_INCLUDE_DIRS ?=

MUSL_EXTRA_LIB_DIRS ?=
MUSL_EXTRA_INCLUDE_DIRS ?=

JS_EXTRA_LIB_DIRS ?=
JS_EXTRA_INCLUDE_DIRS ?=

WASM_EXTRA_LIB_DIRS ?=
WASM_EXTRA_INCLUDE_DIRS ?=
WASM_CC_OPTS = -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types
WASM_CXX_OPTS = -fno-exceptions -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types

# :exploding-head: It turns out override doesn't override the command-line
# value but it overrides Make's normal behavior of ignoring assignments to
# command-line variables. This allows the += operations to append to whatever
# was passed from the command line.

override CABAL_ARGS += \
	--remote-repo-cache _build/packages \
	--store-dir=_build/$(STAGE)/$(TARGET_PLATFORM)/store \
	--logs-dir=_build/$(STAGE)/logs

override CABAL_BUILD_ARGS += \
	-j -w $(GHC) --with-gcc=$(CC) --with-ld=$(LD) \
	--project-file=cabal.project.$(STAGE) \
	$(foreach lib,$(EXTRA_LIB_DIRS),--extra-lib-dirs=$(lib)) \
	$(foreach include,$(EXTRA_INCLUDE_DIRS),--extra-include-dirs=$(include)) \
	--builddir=_build/$(STAGE)/$(TARGET_PLATFORM) \
	--ghc-options="-fhide-source-paths"

ifeq ($(DYNAMIC),1)
GHC_CONFIGURE_ARGS += --enable-dynamic
endif

GHC_TOOLCHAIN_ARGS ?= --disable-ld-override

# just some defaults
STAGE ?= stage1
GHC ?= $(GHC0)

CABAL_BUILD = $(CABAL) $(CABAL_ARGS) build $(CABAL_BUILD_ARGS)

GHC_BIN1 ?= _build/stage1/bin
GHC1 = $(GHC_BIN1)/ghc$(EXE_EXT)
GHC_PKG1 = $(GHC_BIN1)/ghc$(EXE_EXT)
GHC_TOOLCHAIN1 = $(GHC_BIN1)/ghc-toolchain-bin$(EXE_EXT)

GHC_BIN2 ?= _build/stage2/bin
GHC2 = $(GHC_BIN2)/ghc$(EXE_EXT)
GHC_PKG2 = $(GHC_BIN2)/ghc-pkg$(EXE_EXT)
GHC_HSC2HS2 = $(GHC_BIN2)/hsc2hs$(EXE_EXT)
GHC_UNLIT2 = $(GHC_BIN2)/unlit$(EXE_EXT)

define GHC_INFO
$(shell sh -c "$(GHC) --info | $(GHC0) -e 'getContents >>= foldMap putStrLn . lookup \"$1\" . read'")
endef

HOST_PLATFORM   = $(call GHC_INFO,Host platform)
TARGET_PLATFORM = $(call GHC_INFO,target platform string)
TARGET_ARCH     = $(call GHC_INFO,target arch)
TARGET_OS       = $(call GHC_INFO,target os)
TARGET_TRIPLE   = $(call GHC_INFO,Target platform)
GHC_LIBDIR      = $(call GHC_INFO,LibDir)
GIT_COMMIT_ID  := $(shell git rev-parse HEAD)

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

# Handle CPUS and THREADS
CPUS_DETECT_SCRIPT := ./mk/detect-cpu-count.sh
CPUS := $(shell if [ -x $(CPUS_DETECT_SCRIPT) ]; then $(CPUS_DETECT_SCRIPT); else echo 2; fi)
THREADS ?= $(shell echo $$(( $(CPUS) + 1 )))

