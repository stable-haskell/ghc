ifndef MK_TOOLCHAIN
include mk/stable-haskell/toolchain.mk
endif

ifndef MK_COPY_HEADERS
include mk/stable-haskell/headers.mk
endif

ifndef MK_PKG_CONF
include mk/stable-haskell/pkg-conf.mk
endif

# $1 = triplet
define copycrosslib
	@cp -rfp _build/stage3/lib/targets/$1 _build/bindist/stage3/lib/targets/
	@ffi_incdir=`$(CURDIR)/_build/bindist/stage3/bin/$1-ghc-pkg$(EXE_EXT) field libffi-clib include-dirs | grep '/libffi-clib/src/' | sed 's|.*$(CURDIR)/||' || echo "none"` ; cd _build/bindist/stage3/lib/targets/$1/lib/package.conf.d ; \
		for pkg in *.conf ; do \
		  pkgname=`echo $${pkg} | $(SED) 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
		  pkgnamever=`echo $${pkg} | $(SED) 's/\.conf//'` ; \
		  mkdir -p $(CURDIR)/_build/bindist/stage3/lib/targets/$1/lib/$1/$${pkg%.conf} && \
	      cp -rfp $(CURDIR)/_build/stage3/$1/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/_build/bindist/stage3/lib/targets/$1/lib/$1/$${pkg%.conf}/ && \
	      if [ $${pkgname} = "libffi-clib" ] ; then \
		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
	      else \
		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
	      fi ; \
		done ; \
		if [ $${ffi_incdir} != "none" ] ; then $(call copy_headers,ffitarget.h,$(CURDIR)/$${ffi_incdir},libffi-clib,$(CURDIR)/_build/bindist/stage3/bin/$1-ghc-pkg$(EXE_EXT)) ; fi
endef

BINDIST3_EXECTUABLES := \
	ghc$(EXE_EXT) \
	ghc-iserv$(EXE_EXT) \
	ghc-pkg$(EXE_EXT) \
	hp2ps$(EXE_EXT) \
	hpc$(EXE_EXT) \
	hsc2hs$(EXE_EXT) \
	runghc$(EXE_EXT) \
	unlit$(EXE_EXT) \
	haddock$(EXE_EXT)

STAGE3_LIBS := \
    rts:nonthreaded-nodebug \
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
	file-io \
	filepath \
	ghc-bignum \
	ghci \
	hpc \
	integer-gmp \
	mtl \
	os-string \
	parsec \
	pretty \
	process \
	stm \
	template-haskell \
	text \
	time \
	transformers \
	xhtml


# --- Stage 3 generic ---

_build/stage2/lib/targets/% _build/stage3/lib/targets/%:
	@mkdir -p _build/stage3/lib/targets/$(@F)
	@rm -f _build/stage2/lib/targets/$(@F)
	@mkdir -p _build/stage2/lib/targets/
	@ln -sf ../../../stage3/lib/targets/$(@F) _build/stage2/lib/targets/$(@F)

_build/stage3/bin/%-ghc-pkg$(EXE_EXT): _build/stage2/bin/ghc-pkg$(EXE_EXT)
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/ghc-pkg$(EXE_EXT) $@

_build/stage3/bin/%-ghc$(EXE_EXT): _build/stage2/bin/ghc$(EXE_EXT)
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/ghc$(EXE_EXT) $@

_build/stage3/bin/%-hsc2hs$(EXE_EXT): _build/stage2/bin/hsc2hs$(EXE_EXT)
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/hsc2hs$(EXE_EXT) $@

_build/stage3/lib/targets/%/lib/package.conf.d: _build/stage3/lib/targets/%
	@mkdir -p $@

# ghc-toolchain borks unlit
_build/stage3/lib/targets/%/bin/unlit$(EXE_EXT): _build/stage2/bin/unlit$(EXE_EXT)
	@mkdir -p $(@D)
	cp -rfp $< $@

_build/stage3/lib/targets/%/lib/dyld.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/dyld.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/%/lib/post-link.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/post-link.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/%/lib/prelude.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/prelude.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/%/lib/ghc-interp.js:
	@mkdir -p $(@D)
	@cp -f ghc-interp.js $@

# $1 = TIPLET
define build_cross
	GHC=$(GHC) HADRIAN_SETTINGS='$(call HADRIAN_SETTINGS)' \
		PATH=$(PWD)/_build/stage2/bin:$(PWD)/_build/stage3/bin:$(PATH) \
		$(CABAL_BUILD) -W $(GHC2) --happy-options="--template=$(abspath _build/stage2/src/happy-lib-2.1.5/data/)" --with-hsc2hs=$1-hsc2hs --hsc2hs-options='-x' --configure-option='--host=$1' \
		$(foreach lib,$(CROSS_EXTRA_LIB_DIRS),--extra-lib-dirs=$(lib)) \
		$(foreach include,$(CROSS_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$(include)) \
		$(STAGE3_LIBS)
endef

# --- Stage 3 javascript build ---

.PHONY: stage3-javascript-unknown-ghcjs
stage3-javascript-unknown-ghcjs: _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings javascript-unknown-ghcjs-libs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/dyld.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/post-link.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/prelude.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/ghc-interp.js

_build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings: _build/stage2/lib/targets/javascript-unknown-ghcjs _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT)
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin$(EXE_EXT) $(GHC_TOOLCHAIN_ARGS) --triple javascript-unknown-ghcjs --output-settings -o $@ --cc $(EMCC) --cxx $(EMCXX) --ar $(EMAR) --ranlib $(EMRANLIB)

_build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache: _build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg$(EXE_EXT) _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings javascript-unknown-ghcjs-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/javascript-unknown-ghcjs/packagedb/host/*/* $(@D)
	_build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg$(EXE_EXT) recache

.PHONY: javascript-unknown-ghcjs-libs
javascript-unknown-ghcjs-libs: private GHC=$(abspath _build/stage3/bin/javascript-unknown-ghcjs-ghc$(EXE_EXT))
javascript-unknown-ghcjs-libs: private GHC2=$(abspath _build/stage2/bin/ghc$(EXE_EXT))
javascript-unknown-ghcjs-libs: private STAGE=stage3
javascript-unknown-ghcjs-libs: private CC=emcc
javascript-unknown-ghcjs-libs: private CROSS_EXTRA_LIB_DIRS=$(JS_EXTRA_LIB_DIRS)
javascript-unknown-ghcjs-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(JS_EXTRA_INCLUDE_DIRS)
javascript-unknown-ghcjs-libs: cabal.project.stage3 _build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg$(EXE_EXT) _build/stage3/bin/javascript-unknown-ghcjs-ghc$(EXE_EXT) _build/stage3/bin/javascript-unknown-ghcjs-hsc2hs$(EXE_EXT) _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings _build/stage3/lib/targets/javascript-unknown-ghcjs/bin/unlit$(EXE_EXT) _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d
	$(call build_cross,javascript-unknown-ghcjs)

# --- Stage 3 musl build ---

.PHONY: stage3-x86_64-musl-linux
stage3-x86_64-musl-linux: x86_64-musl-linux-libs _build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d/package.cache

_build/stage3/lib/targets/x86_64-musl-linux/lib/settings: _build/stage2/lib/targets/x86_64-musl-linux _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT)
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin$(EXE_EXT) $(GHC_TOOLCHAIN_ARGS) --triple x86_64-musl-linux --output-settings -o $@ --cc x86_64-unknown-linux-musl-cc --cxx x86_64-unknown-linux-musl-c++ --ar x86_64-unknown-linux-musl-ar --ranlib x86_64-unknown-linux-musl-ranlib --ld x86_64-unknown-linux-musl-ld

_build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d/package.cache: _build/stage3/bin/x86_64-musl-linux-ghc-pkg$(EXE_EXT) _build/stage3/lib/targets/x86_64-musl-linux/lib/settings x86_64-musl-linux-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/x86_64-musl-linux/packagedb/host/*/* $(@D)
	_build/stage3/bin/x86_64-musl-linux-ghc-pkg$(EXE_EXT) recache

.PHONY: x86_64-musl-linux-libs
x86_64-musl-linux-libs: private GHC=$(abspath _build/stage3/bin/x86_64-musl-linux-ghc$(EXE_EXT))
x86_64-musl-linux-libs: private GHC2=$(abspath _build/stage2/bin/ghc$(EXE_EXT))
x86_64-musl-linux-libs: private STAGE=stage3
x86_64-musl-linux-libs: private CC=x86_64-unknown-linux-musl-cc
x86_64-musl-linux-libs: private CROSS_EXTRA_LIB_DIRS=$(MUSL_EXTRA_LIB_DIRS)
x86_64-musl-linux-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(MUSL_EXTRA_INCLUDE_DIRS)
x86_64-musl-linux-libs: _build/stage3/bin/x86_64-musl-linux-ghc-pkg$(EXE_EXT) _build/stage3/bin/x86_64-musl-linux-ghc$(EXE_EXT) _build/stage3/bin/x86_64-musl-linux-hsc2hs$(EXE_EXT) _build/stage3/lib/targets/x86_64-musl-linux/lib/settings _build/stage3/lib/targets/x86_64-musl-linux/bin/unlit$(EXE_EXT) _build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d
	$(call build_cross,x86_64-musl-linux)

# --- Stage 3 wasm build ---

.PHONY: stage3-wasm32-unknown-wasi
stage3-wasm32-unknown-wasi: wasm32-unknown-wasi-libs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache _build/stage3/lib/targets/wasm32-unknown-wasi/lib/dyld.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/post-link.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/prelude.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/ghc-interp.js

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings: _build/stage2/lib/targets/wasm32-unknown-wasi _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT)
	@mkdir -p $(@D)
	PATH=/home/hasufell/.ghc-wasm/wasi-sdk/bin:$(PATH) _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT) $(GHC_TOOLCHAIN_ARGS) --triple wasm32-unknown-wasi --output-settings -o $@ --cc wasm32-wasi-clang --cxx wasm32-wasi-clang++ --ar ar --ranlib ranlib --ld wasm-ld --merge-objs wasm-ld --merge-objs-opt="-r" --disable-ld-override --disable-tables-next-to-code $(foreach opt,$(WASM_CC_OPTS),--cc-opt=$(opt)) $(foreach opt,$(WASM_CXX_OPTS),--cxx-opt=$(opt))

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache: _build/stage3/bin/wasm32-unknown-wasi-ghc-pkg$(EXE_EXT) _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings wasm32-unknown-wasi-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/wasm32-unknown-wasi/packagedb/host/*/* $(@D)
	_build/stage3/bin/wasm32-unknown-wasi-ghc-pkg$(EXE_EXT) recache

.PHONY: wasm32-unknown-wasi-libs
wasm32-unknown-wasi-libs: private GHC=$(abspath _build/stage3/bin/wasm32-unknown-wasi-ghc$(EXE_EXT))
wasm32-unknown-wasi-libs: private GHC2=$(abspath _build/stage2/bin/ghc$(EXE_EXT))
wasm32-unknown-wasi-libs: private STAGE=stage3
wasm32-unknown-wasi-libs: private CC=wasm32-wasi-clang
wasm32-unknown-wasi-libs: private CROSS_EXTRA_LIB_DIRS=$(WASM_EXTRA_LIB_DIRS)
wasm32-unknown-wasi-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(WASM_EXTRA_INCLUDE_DIRS)
wasm32-unknown-wasi-libs: cabal.project.stage3 _build/stage3/bin/wasm32-unknown-wasi-ghc-pkg$(EXE_EXT) _build/stage3/bin/wasm32-unknown-wasi-ghc$(EXE_EXT) _build/stage3/bin/wasm32-unknown-wasi-hsc2hs$(EXE_EXT) _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings _build/stage3/lib/targets/wasm32-unknown-wasi/bin/unlit$(EXE_EXT) _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d
	$(call build_cross,wasm32-unknown-wasi)


_build/bindist/stage3/lib/targets/%: _build/bindist/stage2 driver/ghc-usage.txt driver/ghci-usage.txt stage3-%
	@echo "::group::Creating binary distribution in $@"
	@mkdir -p _build/bindist/stage2/bin
	@mkdir -p _build/bindist/stage3/bin
	@mkdir -p _build/bindist/stage3/lib/targets
	# Symlinks
	@cd _build/bindist/stage2/bin ; for binary in * ; do \
		test -L $$binary || { cp -rfp $$binary $(CURDIR)/_build/bindist/stage3/bin/$$binary && ln -sf $$binary $(CURDIR)/_build/bindist/stage3/bin/$(@F)-$$binary ; } \
		; done
	# Copy libraries and settings
	@if [ -e $(CURDIR)/_build/bindist/stage2/lib/targets/$(@F)/lib/$(@F) ] ; then find $(CURDIR)/_build/bindist/stage3/lib/targets/$(@F)/lib/$(@F)/ -mindepth 1 -type f -name "*.so" -execdir mv '{}' $(CURDIR)/_build/bindist/stage3/lib/targets/$(@F)/lib/$(@F)/'{}' \; ; fi
	$(call copycrosslib,$(@F))
	# --help
	@cp -rfp driver/ghc-usage.txt _build/bindist/stage3/lib/targets/$(@F)/lib/
	@cp -rfp driver/ghci-usage.txt _build/bindist/stage3/lib/targets/$(@F)/lib/
	# Recache
	@_build/bindist/stage3/bin/$(@F)-ghc-pkg$(EXE_EXT) recache
	# Copy headers
	@$(call copy_all_stage3_h,_build/bindist/stage3/bin/$(@F)-ghc-pkg$(EXE_EXT),$(@F))
	# remove temporary binaries (the tarball should just contain symlinks to binaries...
	# the binaries are provided by stage2)
	@cd _build/bindist/stage2/bin ; for binary in * ; do \
		test -L $$binary || rm $(CURDIR)/_build/bindist/stage3/bin/$$binary \
		; done
	@echo "::endgroup::"

_build/bindist/stage3/ghc-%.tar.gz: _build/bindist/stage3/lib/targets/% _build/bindist/stage2/ghc.tar.gz
	@triple=`basename $<` ; \
		tar czf $@ \
		--directory=_build/bindist/stage3 \
		$(foreach exe,$(BINDIST3_EXECTUABLES),bin/$${triple}-$(exe)) \
		lib/targets/$${triple}

_build/bindist/tests.tar.gz:
	@tar czf $@ \
		testsuite

.PHONY: clean-stage3
clean-stage3:
	@echo "::group::Cleaning stage3 build artifacts..."
	rm -rf _build/stage3
	rm -rf _build/stage2/lib/targets
	@echo "::endgroup::"
