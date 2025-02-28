SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

CABAL0 ?= cabal
CABAL  ?= _stage0/bin/cabal
GHC    ?= ghc

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

CABAL_INSTALL_FLAGS += --builddir $(OUT)/build
CABAL_INSTALL_FLAGS += --with-compiler $(GHC)
CABAL_INSTALL_FLAGS += --installdir $(OUT)/bin
CABAL_INSTALL_FLAGS += --overwrite-policy=always
# If we copy the executables then ghc will recognise _stage1 as topdir (rather than a path in the store)
CABAL_INSTALL_FLAGS += --install-method=copy

CABAL_INSTALL = $(CABAL) $(CABAL_FLAGS) install $(CABAL_INSTALL_FLAGS)

cabal: _stage0/bin/cabal

_stage0/bin/cabal: OUT ?= $(abspath _stage0)
_stage0/bin/cabal: CABAL=$(CABAL0)
_stage0/bin/cabal:
	@$(LIB)
	log mkdir -p $(@D)
	log $(CABAL_INSTALL) --project-dir libraries/Cabal --project-file cabal.release.project cabal-install:exe:cabal


STAGE1_EXE = ghc ghc-toolchain-bin deriveConstants genprimopcode genapply
STAGE1_BIN := $(addprefix _stage1/bin/,$(STAGE1_EXE))

$(STAGE1_BIN) &: OUT ?= $(abspath _stage1)
$(STAGE1_BIN) &: $(CABAL)
	@$(LIB)
	log mkdir -p $(@D)
	log $(CABAL_INSTALL) --project-file cabal.project.stage1 $(addprefix exe:,$(STAGE1_EXE))

_stage1/lib/settings:
	@$(LIB)

	# Write here only the settings **actually read** by GHC.
	# ghc --info will complete the output with other details
	# known internally (like the project version)

	mkdir -p $(@D)
	cat > $@ <<-'EOF'
	[ ("C compiler command","cc")
	, ("C compiler flags","")
	, ("C++ compiler command","c++")
	, ("C++ compiler flags","")
	, ("C compiler link flags","")
	, ("C compiler supports -no-pie","YES")
	, ("CPP command","cpp")
	, ("CPP flags","-E")
	, ("Haskell CPP command","cpp")
	, ("Haskell CPP flags","-undef -traditional")
	, ("JavaScript CPP command","cpp")
	, ("JavaScript CPP flags","-CC -Wno-unicode -nostdinc")
	, ("C-- CPP command","cpp")
	, ("C-- CPP flags","")
	, ("C-- CPP supports -g0","YES")
	, ("ld supports compact unwind","NO")
	, ("ld supports filelist","NO")
	, ("ld supports single module","NO")
	, ("ld is GNU ld","YES")
	, ("Merge objects command","ld.gold")
	, ("Merge objects flags","-r")
	, ("Merge objects supports response files","YES")
	, ("ar command","ar")
	, ("ar flags","q")
	, ("ar supports at file","YES")
	, ("ar supports -L","NO")
	, ("ranlib command","ranlib")
	, ("otool command","")
	, ("install_name_tool command","")
	, ("windres command","/bin/false")
	, ("unlit command","$$topdir/../bin/unlit")
	, ("cross compiling","NO")
	, ("target platform string","x86_64-unknown-linux")
	, ("target os","OSLinux")
	, ("target arch","ArchX86_64")
	, ("target word size","8")
	, ("target word big endian","NO")
	, ("target has GNU nonexec stack","YES")
	, ("target has .ident directive","YES")
	, ("target has subsections via symbols","NO")
	, ("target has libm","NO")
	, ("Unregisterised","NO")
	, ("LLVM target","x86_64-unknown-linux-gnu")
	, ("LLVM llc command","llc")
	, ("LLVM opt command","opt")
	, ("LLVM llvm-as command","llvm-as")
	, ("Use inplace MinGW toolchain","NO")
	, ("target RTS linker only supports shared libraries","NO")
	, ("Use interpreter","YES")
	, ("Support SMP","YES")
	, ("RTS ways","")
	, ("Tables next to code","YES")
	, ("Leading underscore","NO")
	, ("Use LibFFI","NO")
	, ("RTS expects libdw","NO")
	, ("Relative Global Package DB","")
	, ("base unit-id","")
	]
	EOF

stage1: _stage1/bin/ghc _stage1/lib/settings

stage1-rts: CABAL = _stage0/bin/cabal
stage1-rts: OUT ?= $(abspath _stage1-rts)
stage1-rts: _stage0/bin/cabal stage1 rts/configure libraries/ghc-internal/configure
	@$(LIB)

	flags=(                                     \
	  --with-compiler $(abspath _stage1/bin/ghc) \
	  --prefix "$(OUT)"                         \
	  --package-db "$(OUT)/lib/package.conf.d"  \
	)
	mkdir -p $(OUT)/lib/package.conf.d

	pushd rts-fs || exit
	log ../$(CABAL) act-as-setup --build-type=Simple -- configure --builddir "$(OUT)/dist/rts-fs" "$${flags[@]}"
	log ../$(CABAL) act-as-setup --build-type=Simple -- build --builddir "$(OUT)/dist/rts-fs"
	log ../$(CABAL) act-as-setup --build-type=Simple -- install --builddir "$(OUT)/dist/rts-fs"
	popd

	pushd rts-headers || exit
	log ../$(CABAL) act-as-setup --build-type=Simple -- configure --builddir "$(OUT)/dist/rts-headers" "$${flags[@]}"
	log ../$(CABAL) act-as-setup --build-type=Simple -- build --builddir "$(OUT)/dist/rts-headers"
	log ../$(CABAL) act-as-setup --build-type=Simple -- install --builddir "$(OUT)/dist/rts-headers"
	popd

	export DERIVE_CONSTANTS=$(abspath _stage1/bin/deriveConstants)
	export GENAPPLY=$(abspath _stage1/bin/genapply)
	pushd rts || exit
	log ../$(CABAL) act-as-setup --build-type=Configure -- configure --builddir "$(OUT)/dist/rts" -v "$${flags[@]}"
	log ../$(CABAL) act-as-setup --build-type=Configure -- build --builddir "$(OUT)/dist/rts" -v
	log ../$(CABAL) act-as-setup --build-type=Configure -- install --builddir "$(OUT)/dist/rts" -v
	popd

