SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables --no-builtin-rules

# Default target
all: stage2 rts-debug rts-threaded rts-debug-threaded

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

CABAL_FLAGS += --store-dir $(abspath _build/store) --logs-dir $(abspath _build/logs) -j --ghc-option=-fhide-source-paths --ghc-option=-j
CABAL_BUILD_FLAGS += --builddir $(abspath _build/build) --with-compiler $(GHC) --with-hc-pkg $(GHC)-pkg --with-build-compiler $(GHC0) --with-build-hc-pkg $(GHC0)-pkg
CABAL_INSTALL_FLAGS += --installdir $(OUT)/bin --overwrite-policy=always --install-method=copy --write-ghc-environment-files=never

CABAL_BUILD = $(CABAL) $(CABAL_FLAGS) build $(CABAL_BUILD_FLAGS)
CABAL_INSTALL = $(CABAL) $(CABAL_FLAGS) install $(CABAL_BUILD_FLAGS) $(CABAL_INSTALL_FLAGS)

STAGE1_EXE = ghc ghc-pkg ghc-toolchain-bin
STAGE1_BIN = $(addprefix _stage1/bin/,$(STAGE1_EXE))

STAGE2_EXE = ghc ghc-pkg
STAGE2_BIN = $(addprefix _stage2/bin/,$(STAGE2_EXE))

define GHC_INFO
$(shell $(GHC0) --info | $(GHC0) -e 'getContents >>= foldMap putStrLn . lookup "$1" . read')
endef

BUILD_HOST := $(shell uname -m | tr A-Z a-z)-$(shell uname -s | tr A-Z a-z)
TARGET_HOST ?= $(BUILD_HOST)

TARGET_PLATFORM := $(call GHC_INFO,target platform string)
TARGET_ARCH     := $(call GHC_INFO,target arch)
TARGET_OS       := $(call GHC_INFO,target os)
GIT_COMMIT_ID   := $(shell git rev-parse HEAD)

define HADRIAN_SETTINGS
[ ("hostPlatformArch",    "$(TARGET_ARCH)")
, ("hostPlatformOS",      "$(TARGET_OS)")
, ("cProjectGitCommitId", "$(GIT_COMMIT_ID)")
, ("cProjectVersion",     "9.13")
, ("cProjectVersionInt",  "913")
, ("cProjectPatchLevel",  "0")
, ("cProjectPatchLevel1", "0")
, ("cProjectPatchLevel2", "0")
]
endef

_stage1/cabal:
	$(CABAL0) install --project-dir libraries/Cabal --project-file cabal.release.project --installdir _stage1 cabal-install:exe:cabal

stage1: STAGE=stage1
stage1: $(STAGE1_BIN)

# Stage 1 -- should we pass prefix=stage1- here? And get stage1-ghc, stage1-ghc-pkg, ...?
$(STAGE1_BIN) &: OUT ?= $(abspath _stage1)
$(STAGE1_BIN) &: override GHC=$(GHC0)
$(STAGE1_BIN) &: libraries/directory/configure libraries/unix/configure libraries/time/configure libraries/process/configure libraries/terminfo/configure
	@$(LIB)
	log mkdir -p $(@D)
	log export HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)'
	log $(CABAL_INSTALL) --project-file cabal.project.stage1.$(BUILD_HOST) $(addprefix exe:,$(STAGE1_EXE))

%/settings: _stage1/bin/ghc-toolchain-bin
	@$(LIB)
	mkdir -p $(@D)
	log _stage1/bin/ghc-toolchain-bin --cc=cc --cxx=c++ --install-name-tool=install_name_tool --otool=otool --output-settings -t $(TARGET_PLATFORM) -o $@

_stage1/lib/package.conf.d:
	@$(LIB)
	mkdir -p _stage1/lib _stage2/lib
	log _stage1/bin/ghc-pkg init $(abspath _build/store)/$$(_stage1/bin/ghc --info | grep "Project Unit Id" | sed -E 's/.*_(ghc-[0-9.]+-[0-9a-z]+).*/\1/')/package.db
	log ln -sf $(abspath _build/store)/$$(_stage1/bin/ghc --info | grep "Project Unit Id" | sed -E 's/.*_(ghc-[0-9.]+-[0-9a-z]+).*/\1/')/package.db _stage1/lib/package.conf.d
	log ln -sf $(abspath _build/store)/$$(_stage1/bin/ghc --info | grep "Project Unit Id" | sed -E 's/.*_(ghc-[0-9.]+-[0-9a-z]+).*/\1/')/package.db _stage2/lib/package.conf.d

# stage0 is boot
_stage0/lib/package.conf.d:
	@$(LIB)
	mkdir -p _stage0/lib
	log ln -sf $(abspath _build/store)/$$(ghc --info | grep "Project Unit Id" | sed -E 's/.*[_]*(ghc-[0-9.]+-[0-9a-z]+).*/\1/')/package.db _stage0/lib/package.conf.d

stage1: $(STAGE1_BIN) _stage1/lib/settings _stage1/lib/package.conf.d

# Stage 2
$(STAGE2_BIN) &: OUT ?= $(abspath _stage2)
$(STAGE2_BIN) &: GHC = $(abspath _stage1/bin/ghc)
$(STAGE2_BIN) &: GHC0 = $(shell which ghc)
$(STAGE2_BIN) &: _stage1/bin/ghc _stage1/lib/settings rts/configure libraries/ghc-internal/configure _stage0/lib/package.conf.d _stage1/lib/package.conf.d
	@$(LIB)
	log mkdir -p $(@D)
	log export HADRIAN_SETTINGS="$$(cat ./HADRIAN_SETTINGS)"
	log $(CABAL_INSTALL) --package-db=$(abspath _stage1/lib/package.conf.d) --build-package-db=$(abspath _stage0/lib/package.conf.d) --project-file cabal.project.stage2.$(TARGET_HOST) $(addprefix exe:,$(STAGE2_EXE))

# Build a few extra RTS variants (debug, threaded, debug-threaded)
# We build these with the stage1 compiler, as we build all other libraries
rts-debug: OUT = $(abspath _stage2)
rts-debug: GHC = $(abspath _stage1/bin/ghc)
rts-debug: _stage2/bin/ghc
	@$(LIB)
	log rm -f ~/.ghc/*/*/default
	log $(CABAL_INSTALL) --write-ghc-environment-files=never --package-db=$(abspath _stage1/lib/package.conf.d) --build-package-db=$(abspath _stage0/lib/package.conf.d) --project-file cabal.project.stage2.$(TARGET_HOST) --lib rts:rts --constraint="rts+debug"
	# cabal still writes the environment files even if we pass --write-ghc-environment-files=never
	log rm -f ~/.ghc/*/*/default

rts-threaded: OUT = $(abspath _stage2)
rts-threaded: GHC = $(abspath _stage2/bin/ghc)
rts-threaded: _stage2/bin/ghc
	@$(LIB)
	log rm -f ~/.ghc/*/*/default
	log $(CABAL_INSTALL) --write-ghc-environment-files=never --package-db=$(abspath _stage1/lib/package.conf.d) --build-package-db=$(abspath _stage0/lib/package.conf.d) --project-file cabal.project.stage2.$(TARGET_HOST) --lib rts:rts --constraint="rts+threaded"
	# cabal still writes the environment files even if we pass --write-ghc-environment-files=never
	log rm -f ~/.ghc/*/*/default


rts-debug-threaded: OUT = $(abspath _stage2)
rts-debug-threaded: GHC = $(abspath _stage2/bin/ghc)
rts-debug-threaded: _stage2/bin/ghc
	@$(LIB)
	log rm -f ~/.ghc/*/*/default
	log $(CABAL_INSTALL) --write-ghc-environment-files=never --package-db=$(abspath _stage1/lib/package.conf.d) --build-package-db=$(abspath _stage0/lib/package.conf.d) --project-file cabal.project.stage2.$(TARGET_HOST) --lib rts:rts --constraint="rts+debug" --constraint="rts+threaded"
	# cabal still writes the environment files even if we pass --write-ghc-environment-files=never
	log rm -f ~/.ghc/*/*/default

stage2: stage1 _stage2/lib/settings $(STAGE2_BIN)

# Clean up
clean:
	rm -rf _stage0 _stage1 _stage2 _build

# Usage instructions
# 1. make clean
# 2. make stage1
# 3. make stage2