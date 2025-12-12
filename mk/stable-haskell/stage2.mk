ifndef MK_TOOLCHAIN
include mk/stable-haskell/toolchain.mk
endif

ifndef MK_COPY_HEADERS
include mk/stable-haskell/headers.mk
endif

ifndef MK_PKG_CONF
include mk/stable-haskell/pkg-conf.mk
endif

STAGE2_UTIL_TARGETS := \
	deriveConstants:deriveConstants \
	genapply:genapply \
	genprimopcode:genprimopcode \
	ghc-pkg:ghc-pkg \
	hsc2hs:hsc2hs \
	rts-headers:rts-headers \
	unlit:unlit

# We really want to work towards `cabal build/instsall ghc-bin:ghc`.
STAGE2_TARGETS := \
	ghc-bin:ghc

# we need to build these before all else
STAGE2_UTIL_RTS := \
	rts:nonthreaded-debug \
	rts:nonthreaded-nodebug \
	rts:threaded-nodebug \
	rts:threaded-debug

# rts:threaded-nodebug need it for compiling Setup.hs
STAGE2_UTIL_TARGETS := \
	$(STAGE2_UTIL_TARGETS) \
	ghc-iserv:ghc-iserv \
	$(STAGE2_UTIL_RTS) \
	hp2ps:hp2ps \
	hpc-bin:hpc \
	runghc:runghc \
	ghc-bignum:ghc-bignum \
	ghc-compact:ghc-compact \
	ghc-experimental:ghc-experimental \
	ghc-toolchain:ghc-toolchain \
	integer-gmp:integer-gmp \
	system-cxx-std-lib:system-cxx-std-lib \
	xhtml:xhtml \
	haddock:haddock

ifneq ($(OS),Windows_NT)
STAGE2_UTIL_TARGETS += terminfo:terminfo
endif

# These things should be built on demand.
# hp2ps:hp2ps \
# hpc-bin:hpc \
# ghc-iserv:ghc-iserv \
# runghc:runghc \

# This package is just utterly retarded
# I don't understand why this following line somehow breaks the build...
# STAGE2_TARGETS += system-cxx-std-lib:system-cxx-std-lib

BINDIST2_EXECTUABLES := \
	ghc$(EXE_EXT) \
	ghc-iserv$(EXE_EXT) \
	ghc-pkg$(EXE_EXT) \
	hp2ps$(EXE_EXT) \
	hpc$(EXE_EXT) \
	hsc2hs$(EXE_EXT) \
	runghc$(EXE_EXT) \
	unlit$(EXE_EXT) \
	haddock$(EXE_EXT)

# TODO: dedup
STAGE2_EXECUTABLES := \
	ghc$(EXE_EXT)

STAGE2_UTIL_EXECUTABLES := \
	deriveConstants$(EXE_EXT) \
	genapply$(EXE_EXT) \
	genprimopcode$(EXE_EXT) \
	hsc2hs$(EXE_EXT) \
	ghc-iserv$(EXE_EXT) \
	ghc-pkg$(EXE_EXT) \
	hp2ps$(EXE_EXT) \
	hpc$(EXE_EXT) \
	runghc$(EXE_EXT) \
	unlit$(EXE_EXT) \
	haddock$(EXE_EXT)

# --- Stage 2 build ---

_build/stage2/%: private STAGE=stage2
_build/stage2/%: private GHC=$(realpath $(GHC1))

.PHONY: $(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES))
$(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) &: $(GHC_BIN1) $(GHC1) $(GHC_PKG1) $(GHC_TOOLCHAIN1) cabal.project.stage2 stage2-rts
	@echo "::group::Building stage2 executables ($(STAGE2_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	GHC=$(GHC) HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
		PATH='$(realpath $(GHC_BIN1)):$(PATH)' \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_TARGETS)
	@echo "::endgroup::"

.PHONY: stage2-rts
stage2-rts: private STAGE=stage2
stage2-rts: private GHC=$(realpath $(GHC1))
stage2-rts: private TARGET_PLATFORM=
stage2-rts: $(GHC_BIN1) $(GHC1) $(GHC_PKG1) $(GHC_TOOLCHAIN1) cabal.project.stage2
	@echo "::group::Building stage2 RTSes..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	GHC=$(GHC) HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
		PATH='$(realpath $(GHC_BIN1)):$(PATH)' \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_UTIL_RTS)
	@echo "::endgroup::"


# Do we want to build these with the stage2 GHC or the stage1 GHC?
# Traditionally we build them with the stage1 ghc, but we could just as well
# build them with the stage2 ghc; seems like a better/cleaner idea to me (moritz).
.PHONY: $(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES))
$(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES)) &: $(GHC_BIN1) $(GHC1) $(GHC_PKG1) $(GHC_TOOLCHAIN1) cabal.project.stage2.settings stage2-rts
	@echo "::group::Building stage2 utilities ($(STAGE2_UTIL_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	GHC=$(GHC) HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
		PATH='$(realpath $(GHC_BIN1)):$(PATH)' \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_UTIL_TARGETS)
	@echo "::endgroup::"

_build/stage2/lib/settings: $(GHC_TOOLCHAIN1)
	@echo "::group::Creating settings for $(TARGET_TRIPLE)..."
	@mkdir -p $(@D)
	$(GHC_TOOLCHAIN1) $(GHC_TOOLCHAIN_ARGS) --triple $(TARGET_TRIPLE) --output-settings -o $@ --cc $(CC) --cxx $(CXX) --cc-link-opt "$(CC_LINK_OPT)"
	@echo "::endgroup::"

_build/stage2/lib/package.conf.d/package.cache: _build/stage2/bin/ghc-pkg$(EXE_EXT) _build/stage2/lib/settings
	@echo "::group::Creating stage2 package cache..."
	@mkdir -p _build/stage2/lib/package.conf.d
	@rm -rf _build/stage2/lib/package.conf.d/*
	cp -rfp _build/stage2/packagedb/host/*/* _build/stage2/lib/package.conf.d
	_build/stage2/bin/ghc-pkg$(EXE_EXT) recache
	@echo "::endgroup::"

_build/stage2/lib/template-hsc.h: utils/hsc2hs/data/template-hsc.h
	@mkdir -p $(@D)
	cp -rfp $< $@

.PHONY: stage2
stage2: $(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) _build/stage2/lib/settings _build/stage2/lib/package.conf.d/package.cache _build/stage2/lib/template-hsc.h

# Target for creating the final binary distribution directory
#_build/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
_build/bindist/stage2: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
	@echo "::group::Creating binary distribution in $@"
	@mkdir -p $@/bin
	@mkdir -p $@/lib
	# Copy executables from stage bin
	@cp -rfp _build/$(@F)/bin/* $@/bin/
	# Copy libraries and settings from stage lib
	@cp -rfp _build/$(@F)/lib/{package.conf.d,settings,template-hsc.h} $@/lib/
	@mkdir -p $@/lib/$(HOST_PLATFORM)
	@ffi_incdir=`$(CURDIR)/$@/bin/ghc-pkg$(EXE_EXT) field libffi-clib include-dirs | grep 'libffi-clib[/\\]src/' | sed 's/^[ \t]*//' | $(CYGPATH) | sed 's|.*$(CURDIR)/||'` ; \
		cd $@/lib/package.conf.d ; \
			for pkg in *.conf ; do \
		  	pkgname=`echo $${pkg} | $(SED) 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
		  	pkgnamever=`echo $${pkg} | $(SED) 's/\.conf//'` ; \
		  	mkdir -p $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
		  	cp -rfp $(CURDIR)/_build/$(@F)/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
	      	if [ $${pkgname} = "libffi-clib" ] ; then \
			    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$(HOST_PLATFORM),$${pkgnamever}) ; \
	    	  else \
		    	$(call patchpackageconf,$${pkgname},$${pkg},../../..,$(HOST_PLATFORM),$${pkgnamever}) ; \
	      	fi ; \
			done ; \
			$(call copy_headers,ffitarget.h,$(CURDIR)/$${ffi_incdir},libffi-clib,$(CURDIR)/$@/bin/ghc-pkg$(EXE_EXT))
	# Copy driver usage files
	@cp -rfp driver/ghc-usage.txt $@/lib/
	@cp -rfp driver/ghci-usage.txt $@/lib/
	@echo "FIXME: Changing 'Support SMP' from YES to NO in settings file"
	@$(SED) 's/("Support SMP","YES")/("Support SMP","NO")/' -i.bck $@/lib/settings
	# Recache
	$@/bin/ghc-pkg$(EXE_EXT) recache
	# Copy headers
	@$(call copy_all_stage2_h,$@/bin/ghc-pkg$(EXE_EXT))
	@echo "::endgroup::"

_build/bindist/stage2/ghc.tar.gz: _build/bindist/stage2
	@tar czf $@ \
		--directory=_build/bindist/stage2 \
		$(foreach exe,$(BINDIST2_EXECTUABLES),bin/$(exe)) \
		lib/ghc-usage.txt \
		lib/ghci-usage.txt \
		lib/package.conf.d \
		lib/settings \
		lib/template-hsc.h \
		lib/$(HOST_PLATFORM)

.PHONY: clean-stage2
clean-stage2:
	@echo "::group::Cleaning stage2 build artifacts..."
	rm -rf _build/stage2
	@echo "::endgroup::"
