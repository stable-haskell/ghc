ifndef MK_TOOLCHAIN
include mk/stable-haskell/toolchain.mk
endif

STAGE1_UTIL_TARGETS := \
	deriveConstants:deriveConstants \
	genapply:genapply \
	genprimopcode:genprimopcode \
	ghc-pkg:ghc-pkg \
	hsc2hs:hsc2hs \
	rts-headers:rts-headers \
	unlit:unlit

STAGE1_TARGETS := $(STAGE1_UTIL_TARGETS) ghc-bin:ghc ghc-toolchain-bin:ghc-toolchain-bin

BINDIST1_EXECTUABLES := \
	ghc$(EXE_EXT) \
	ghc-pkg$(EXE_EXT) \
	ghc-toolchain-bin$(EXE_EXT) \
	hsc2hs$(EXE_EXT) \
	unlit$(EXE_EXT)

# TODO: dedup
STAGE1_EXECUTABLES := \
	deriveConstants$(EXE_EXT) \
	genapply$(EXE_EXT) \
	genprimopcode$(EXE_EXT) \
	ghc$(EXE_EXT) \
	ghc-pkg$(EXE_EXT) \
	ghc-toolchain-bin$(EXE_EXT) \
	hsc2hs$(EXE_EXT) \
	unlit$(EXE_EXT)

# --- Stage 1 build ---

_build/stage1/%: private STAGE=stage1
_build/stage1/%: private GHC=$(GHC0)

.PHONY: cabal.project.stage1.local

cabal.project.stage1.local: cabal.project.stage1
ifeq ($(OS),Windows_NT)
	echo "extra-prog-path: $(shell echo '$(GHC_LIBDIR)' | $(CYGPATH_MIXED))/../mingw/bin" > $@
else
	echo "" > $@
endif

_build/stage1/bin: stage1
	@mkdir -p $@

.PHONY: $(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES))
$(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) &: $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES) libraries/ghc-boot-th-next/ghc-boot-th-next.cabal cabal.project.stage1 cabal.project.stage1.local
	@echo "::group::Building stage1 executables ($(STAGE1_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage1/cache
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' $(CABAL_BUILD) $(STAGE1_TARGETS)
	@echo "::endgroup::"

_build/stage1/lib/settings: _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT)
	@echo "::group::Creating settings for $(TARGET_TRIPLE)..."
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin$(EXE_EXT) $(GHC_TOOLCHAIN_ARGS) --triple $(TARGET_TRIPLE) --output-settings -o $@ --cc $(CC) --cxx $(CXX) --cc-link-opt "$(CC_LINK_OPT)"
	@echo "::endgroup::"

# The somewhat strange thing is, we might not even need this at all now anymore. cabal seems to
# pass all the necessary flags correctly. Thus even with an _empty_ package-db here (and it will
# stay empty until we are done with the build), the build succeeds.
#
# For now, we are tying the knot here by making sure the stage1 compiler (stage1/bin/ghc) sees
# the packages it builds (to build stage2/bin/ghc), by symlining cabal's target package-db into
# the compilers global package-db. Another maybe even better solution might be to set the
# Global Package DB in the settings file to the absolute path where cabal will place the
# package db. This would elminate this rule outright.
_build/stage1/lib/package.conf.d/package.cache: _build/stage1/bin/ghc-pkg$(EXE_EXT) _build/stage1/lib/settings
	@echo "::group::Creating stage1 package cache..."
	@mkdir -p _build/stage1/lib/package.conf.d
# 	@mkdir -p _build/stage2/packagedb/host
# 	ln -s $(abspath ./_build/stage2/packagedb/host/ghc-9.14) _build/stage1/lib/package.conf.d
# 	_build/stage1/bin/ghc-pkg init $(abspath ./_build/stage2/packagedb/host/ghc-9.14)
	@echo "::endgroup::"

_build/stage1/lib/template-hsc.h: utils/hsc2hs/data/template-hsc.h
	@mkdir -p $(@D)
	cp -rfp $< $@

.PHONY: stage1
stage1: $(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) _build/stage1/lib/settings _build/stage1/lib/package.conf.d/package.cache _build/stage1/lib/template-hsc.h

_build/bindist/stage1: stage1 driver/ghc-usage.txt driver/ghci-usage.txt
	@echo "::group::Creating binary distribution in $@"
	@mkdir -p $@/bin
	@mkdir -p $@/lib
	# Copy executables from stage bin
	@cp -rfp _build/$(@F)/bin/* $@/bin/
	# Copy libraries and settings from stage lib
	@cp -rfp _build/$(@F)/lib/{package.conf.d,settings,template-hsc.h} $@/lib/
	@mkdir -p $@/lib/$(HOST_PLATFORM)
	# Copy driver usage files
	@cp -rfp driver/ghc-usage.txt $@/lib/
	@cp -rfp driver/ghci-usage.txt $@/lib/
	@echo "FIXME: Changing 'Support SMP' from YES to NO in settings file"
	@$(SED) 's/("Support SMP","YES")/("Support SMP","NO")/' -i.bck $@/lib/settings
	@echo "::endgroup::"

_build/bindist/stage1/ghc.tar.gz: _build/bindist/stage1
	@tar czf $@ \
		--directory=_build/bindist/stage1 \
		$(foreach exe,$(BINDIST1_EXECTUABLES),bin/$(exe)) \
		lib/ghc-usage.txt \
		lib/ghci-usage.txt \
		lib/package.conf.d \
		lib/settings \
		lib/template-hsc.h \
		lib/$(HOST_PLATFORM)

.PHONY: clean-stage1
clean-stage1:
	@echo "::group::Cleaning stage1 build artifacts..."
	rm -rf _build/stage1
	@echo "::endgroup::"
