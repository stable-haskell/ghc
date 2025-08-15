# Top-level Makefile
#
# This file is still _TOO_ large (should be < 100L). There are too many moving
# _global_ parts, most of this should be relegated to the respective packages.
# The whole version replacement therapy is utterly ridiculous. It should be done
# in the respective packages.

SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c

# Sane defaults
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ROOT_DIR := $(patsubst %/,%,$(dir $(realpath $(lastword $(MAKEFILE_LIST)))))

# NOTE: Variable definitions like these can be overridden from the command line
# but take precedence over environment variables. E.g.
# 
# make GHC=ghc-9.12.2
#
# will use ghc 9.12.2, but
#
# GHC=9.12.2 make
#
# will still use ghc 9.8.4.
#
# Explicit is better than implicit so I do not mind this.

GHC0   = ghc-9.8.4
CABAL0 = cabal

# Build directories and files
BUILD_DIR  = _build
STAGE_DIR  = $(BUILD_DIR)/$(STAGE)
LOGS_DIR   = $(STAGE_DIR)/logs
STORE_DIR  = $(STAGE_DIR)/store

# The Hackage index state we want to use.
INDEX_STATE = 2025-07-01T00:00:00Z

# These are merely shortcuts
CABAL   := $(BUILD_DIR)/stage0/bin/cabal
GHC1    := $(BUILD_DIR)/stage1/bin/ghc
GHC2    := $(BUILD_DIR)/stage2/bin/ghc
HACKAGE := $(BUILD_DIR)/packages/hackage.haskell.org/01-index.tar.gz

ifdef GITHUB_ACTIONS
GROUP = @echo "::group::$(1)"
END_GROUP = @echo "::endgroup::"
else
GROUP = @echo "$(1)"
END_GROUP =  
endif

# User supplied build arguments (e.g. "-j1 -v3")
CABAL_BUILD_ARGS =

# Notes:
# - The absolute paths here are important. Do not change them.
# - Only global packages go into logs. Cabal does not store
#   logs for local packages by default.
CABAL_BUILD = \
	$(CABAL) \
  --remote-repo-cache $(BUILD_DIR)/packages \
	--store-dir $(abspath $(STORE_DIR)) \
	--logs-dir $(abspath $(LOGS_DIR)) \
	build \
	-j \
	--builddir $(abspath $(STAGE_DIR)) \
	--index-state=$(INDEX_STATE) \
	--project-file=cabal.project.$(STAGE) \
	--with-gcc $(CC) \
	--ghc-options "-fhide-source-paths" \
	$(CABAL_BUILD_ARGS)

TARGET_ARCH     := $(shell $(GHC0) -e "print GHC.Platform.Host.hostPlatformArch")
TARGET_OS       := $(shell $(GHC0) -e "print GHC.Platform.Host.hostPlatformOS")
TARGET_TRIPLE   := $(shell $(GHC0) --print-target-platform)
GIT_COMMIT_ID   := $(shell git rev-parse HEAD)

define HADRIAN_SETTINGS
[ ("hostPlatformArch",    "$(TARGET_ARCH)") \
, ("hostPlatformOS",      "$(TARGET_OS)") \
, ("cProjectGitCommitId", "$(GIT_COMMIT_ID)") \
, ("cProjectVersion",     "9.13") \
, ("cProjectVersionInt",  "913") \
, ("cProjectPatchLevel",  "0") \
, ("cProjectPatchLevel1", "0") \
, ("cProjectPatchLevel2", "0") \
]
endef

# Handle CPUS and THREADS
CPUS_DETECT_SCRIPT := ./mk/detect-cpu-count.sh
CPUS := $(shell if [ -x $(CPUS_DETECT_SCRIPT) ]; then $(CPUS_DETECT_SCRIPT); else echo 2; fi)
THREADS ?= $(shell echo $$(( $(CPUS) + 1 )))

# Macro to install executables after cabal build. This does not require calling cabal again,
# which is slower and can cause replanning.
define INSTALL_BINS
  install -D $$(jq -r '."install-plan"[] | select((."stage" // "host") == "host" and ([."component-name"] | inside($$ARGS.positional))) | ."bin-file"' $(STAGE_DIR)/cache/plan.json --args $(addprefix exe:,$(2))) $(1)
endef

# --- Main Targets ---
all: $(BUILD_DIR)/bindist # booted will depend on prepare-sources

# --- Common variables ---

$(BUILD_DIR)/stage0/% : private STAGE=stage0
$(BUILD_DIR)/stage1/% : private STAGE=stage1
$(BUILD_DIR)/stage2/% : private STAGE=stage2
# --- Common Targets ---

$(BUILD_DIR)/%/lib/package.conf.d/package.cache: $(BUILD_DIR)/%/bin/ghc-pkg $(BUILD_DIR)/%/lib/settings
	@rm -rf $(@D)
	@mkdir -p $(@D)
	cp -rfp $(BUILD_DIR)/$(STAGE)/packagedb/host/*/* $(@D)
	$(BUILD_DIR)/$(STAGE)/bin/ghc-pkg recache

$(BUILD_DIR)/%/lib/template-hsc.h: utils/hsc2hs/data/template-hsc.h
	@mkdir -p $(@D)
	cp -rfp $< $@

# [Note] Make phony targets and cabal
# 
# Getting Make to do the right thing is tricky. Here is what we want:
# 1. cabal build should always be re-run. We cannot capture its dependencies
#    correctly and cabal already avoids unnecessary re-builds and would not
#    update the final binary unless it has to be recompiled.
# 2. at the same time we want the final binary to be considered dirty only
#    when it is actually recompiled.
#
# Making $(BUILD_DIR)/stage0/bin/cabal a phony target will give you 1. but it
# would also cause $(BUILD_DIR)/stage0/bin/cabal to always be considered dirty
# (because that is what phony dependencies are meant for). Thus it would not
# deliver 2.
#
# So we do this. We make a phony FORCE target that causes the recipe to always
# run, but $(BUILD_DIR)/stage0/bin/cabal is not phony so it will only be considered
# dirty only when the file changes (i.e. when its mtime changes). We also use
# install -C so the destination file is only updated if contents actually change.

.PHONY: FORCE
FORCE:

# --- Stage 0 build ---
#
# This builds our cabal-install, which is used to build the rest of the project.

$(BUILD_DIR)/stage0/bin/cabal: FORCE
	$(call GROUP,Building Cabal...)
	@mkdir -p $(@D)
	$(CABAL0) build \
		-j \
		--with-compiler $(GHC0) \
		--project-dir libraries/Cabal \
		--project-file cabal.release.project \
		--builddir $(abspath $(STAGE_DIR)) \
		--ghc-options "-fhide-source-paths" \
		exe:$(@F) \
		2>&1 | tee $(STAGE_DIR)/build.log
	$(call INSTALL_BINS,$(@D),$(@F))
	$(END_GROUP)

# --- Stage 1 build ---

# STAGE_UTIL_TARGETS := \
# 	deriveConstants \
# 	genapply \
# 	genprimopcode \
# 	ghc-pkg \
# 	hsc2hs \
# 	rts-headers \
# 	unlit

# STAGE1_TARGETS := $(STAGE_UTIL_TARGETS) ghc-bin:ghc ghc-toolchain-bin:ghc-toolchain-bin

# TODO: dedup
STAGE1_EXECUTABLES := \
	deriveConstants \
	genapply \
	genprimopcode \
	ghc \
	ghc-pkg \
	ghc-toolchain-bin \
	hsc2hs \
	unlit

STAGE1_EXE := $(addprefix $(BUILD_DIR)/stage1/bin/,$(STAGE1_EXECUTABLES))

.PHONY: stage1
stage1: \
	$(GHC1) \
	$(BUILD_DIR)/stage1/lib/settings \
	$(BUILD_DIR)/stage1/lib/package.conf.d/package.cache \
	$(BUILD_DIR)/stage1/lib/template-hsc.h

# Re-run cabal every time for stage1, but don't mark outputs dirty unless they change.
$(STAGE1_EXE) &: FORCE prepare
	$(call GROUP,Building stage1 executables ($(STAGE1_EXECUTABLES))...)
	@mkdir -p $(@D)
	# Force cabal to replan
	rm -rf $(BUILD_DIR)/stage1/cache
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
	$(CABAL_BUILD) \
		--with-compiler $(GHC0) \
		$(addprefix exe:,$(STAGE1_EXECUTABLES)) \
		2>&1 | tee $(STAGE_DIR)/build.log
	# We do not need to install the executables here, our cabal-install will do that
	$(END_GROUP)

$(BUILD_DIR)/stage1/lib/settings: $(BUILD_DIR)/stage1/bin/ghc-toolchain-bin
	$(call GROUP,Creating settings for $(TARGET_TRIPLE)...)
	@mkdir -p $(@D)
	$(BUILD_DIR)/stage1/bin/ghc-toolchain-bin --triple $(TARGET_TRIPLE) --cc $(CC) --cxx $(CXX) --output-settings -o $@
	$(END_GROUP)

# Create a wrapper around the stage1 ghc that logs invocations and delegates to the real ghc
$(BUILD_DIR)/stage1/bin/wrapped-ghc: mk/wrapped-ghc.in $(BUILD_DIR)/stage1/bin/ghc
	$(call GROUP,Creating wrapped-ghc (stage1 wrapper)...)
	@mkdir -p $(@D)
	@cp -fp mk/wrapped-ghc.in $@
	@chmod +x $@
	$(END_GROUP)

# --- Stage 2 build ---

STAGE2_EXE_TARGETS := \
	deriveConstants \
	genapply \
	genprimopcode \
	ghc \
	ghc-iserv \
	ghc-pkg \
	hp2ps \
	hpc \
	hsc2hs \
	runghc \
	unlit

STAGE2_EXE := $(addprefix $(BUILD_DIR)/stage2/bin/,$(STAGE2_EXE_TARGETS))

.PHONY: stage2
stage2: \
	$(GHC2) \
	$(BUILD_DIR)/stage2/lib/settings \
	$(BUILD_DIR)/stage2/lib/package.conf.d/package.cache \
	$(BUILD_DIR)/stage2/lib/template-hsc.h

# Same pattern for stage2 executables: always run cabal, only dirty on change.
$(STAGE2_EXE) &: FORCE stage1 $(BUILD_DIR)/stage1/bin/wrapped-ghc prepare
	$(call GROUP,building $(STAGE) executables $(STAGE2_EXE_TARGETS))
	@mkdir -p $(@D)
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
	PATH=$(abspath $(BUILD_DIR)/stage1/bin):$(PATH) \
	$(CABAL_BUILD) \
		--with-build-compiler=$(GHC0) \
		--with-compiler=$(abspath $(BUILD_DIR)/stage1/bin/wrapped-ghc) \
		--ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" \
		$(addprefix exe:,$(STAGE2_EXE_TARGETS)) \
		2>&1 | tee $(STAGE_DIR)/build.log
	# We do not need to install the executables here, our cabal-install will do that
	$(END_GROUP)

$(BUILD_DIR)/stage2/lib/settings: $(BUILD_DIR)/stage1/lib/settings
	@mkdir -p $(@D)
	cp -rfp $(BUILD_DIR)/stage1/lib/settings $(BUILD_DIR)/stage2/lib/settings

# --- Prepare ---

# The hackage index can be order-only because although the file would change
# after a cabal update, the index state is fixed and the build would not change.
prepare: $(BUILD_DIR)/booted $(CABAL) | $(HACKAGE)

$(HACKAGE): $(CABAL)
	$(call GROUP,Updating Hackage index...)
	@mkdir -p $(@D)
	$(CABAL) --remote-repo-cache $(BUILD_DIR)/packages update
	$(END_GROUP)

CONFIGURE_SCRIPTS := \
	configure \
	libraries/directory/configure \
	libraries/ghc-internal/configure \
	libraries/process/configure \
	libraries/terminfo/configure \
	libraries/time/configure \
	libraries/unix/configure \
	rts/configure

$(CONFIGURE_SCRIPTS): %: %.ac
	$(call GROUP,Running autoconf...)
	autoreconf --install $(@D)
	$(END_GROUP)

# booted depends on successful source preparation
$(BUILD_DIR)/booted: $(CONFIGURE_SCRIPTS)
	$(call GROUP,Running ./configure script...)
	./configure
	@mkdir -p $(@D)
	@touch $@
	$(END_GROUP)

# --- Bindist Target ---

# Target for creating the final binary distribution directory
$(BUILD_DIR)/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
	$(call GROUP,Creating binary distribution in $(BUILD_DIR)/bindist)
	@mkdir -p $(BUILD_DIR)/bindist/bin
	@mkdir -p $(BUILD_DIR)/bindist/lib
	# Copy executables from stage2 bin
	@cp -rfp $(BUILD_DIR)/stage2/bin/* $(BUILD_DIR)/bindist/bin/
	# Copy libraries and settings from stage2 lib
	@cp -rfp $(BUILD_DIR)/stage2/lib/* $(BUILD_DIR)/bindist/lib/
	# Copy driver usage files
	@cp -rfp driver/ghc-usage.txt $(BUILD_DIR)/bindist/lib/
	@cp -rfp driver/ghci-usage.txt $(BUILD_DIR)/bindist/lib/
	@echo "FIXME: Changing 'Support SMP' from YES to NO in settings file"
	@sed 's/("Support SMP","YES")/("Support SMP","NO")/' -i.bck $(BUILD_DIR)/bindist/lib/settings
	$(END_GROUP)


# --- Clean Targets ---

# Files that will be generated by config.status from their .in counterparts
CONFIGURED_FILES := \
	compiler/ghc.cabal \
	ghc/ghc-bin.cabal \
	libraries/base/base.cabal \
	libraries/ghc-boot-th/ghc-boot-th.cabal \
	libraries/ghc-boot/ghc-boot.cabal \
	libraries/ghc-experimental/ghc-experimental.cabal \
	libraries/ghc-heap/ghc-heap.cabal \
	libraries/ghc-internal/ghc-internal.cabal \
	libraries/ghci/ghci.cabal \
	libraries/template-haskell/template-haskell.cabal \
	rts/include/ghcversion.h \
	utils/ghc-iserv/ghc-iserv.cabal \
	utils/ghc-pkg/ghc-pkg.cabal \
	utils/runghc/runghc.cabal

clean-everything:
	$(call GROUP,Cleaning all build artifacts...)
	rm -rf $(BUILD_DIR)
	$(END_GROUP)

clean:
	$(call GROUP,Cleaning stage1 and stage2 build artifacts...)
	rm -rf $(BUILD_DIR)/stage1 $(BUILD_DIR)/stage2
	$(END_GROUP)

clean-stage1:
	$(call GROUP,Cleaning stage1 build artifacts...)
	rm -rf $(BUILD_DIR)/stage1
	$(END_GROUP)

clean-stage2:
	$(call GROUP,Cleaning stage2 build artifacts...)
	rm -rf $(BUILD_DIR)/stage2
	$(END_GROUP)

distclean: clean
	$(call GROUP,Cleaning all generated files (distclean)...)
	rm -rf autom4te.cache
	rm -f config.status config.log config.h configure aclocal.m4
	rm -rf build-aux/config.guess build-aux/config.sub build-aux/install-sh build-aux/missing build-aux/compile depcomp
	find . -name 'Makefile.in' -delete
	rm -f $(CONFIGURED_FILES)
	$(END_GROUP)


# --- Test Target ---
test: $(BUILD_DIR)/bindist
	$(call GROUP,Running tests with THREADS=${THREADS})
	TEST_HC=`pwd`/$(BUILD_DIR)/bindist/bin/ghc \
	TEST_CC=$(CC) \
	TEST_CXX=$(CXX) \
	METRICS_FILE=`pwd`/$(BUILD_DIR)/test-perf.csv \
	SUMMARY_FILE=`pwd`/$(BUILD_DIR)/test-summary.txt \
	JUNIT_FILE=`pwd`/$(BUILD_DIR)/test-junit.xml \
	make -C testsuite/tests test THREADS=${THREADS}
	$(END_GROUP) >&2

# Inform Make that these are not actual files if they get deleted by other means
.PHONY: clean distclean test all
