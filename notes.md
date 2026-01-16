
https://github.com/stable-haskell/ghc/commit/18e5e95056dcdc0f5f2f75950385ce815e061484#diff-76ed074a9305c04054cdebb9e9aad2d818052b07091de1f20cad0bbac34ffb52

ifeq ($(OS),Windows_NT)
CC := x86_64-w64-mingw32-clang.exe
CXX := x86_64-w64-mingw32-clang++.exe

# https://gitlab.haskell.org/ghc/ghc/-/issues/7289#note_646155
# @Phyx the current patch uses some non-portable macro that has currently been changed upstream in an ABI incompatible manner:
# https://github.com/mingw-w64/mingw-w64/commit/9c27617e016f966803a16e84d878f71565e12074
# https://github.com/mingw-w64/mingw-w64/commit/5c5973cf5f021db8fd75e9667e63881ccd169320
# What this means practically is: if you built GHC against an older crt and then link the rts and a new crt into a binary... you'll get a segfault, because the old FE_PC53_ENV macro is "inlined" into the rts and we try call fesetenv of the new crt now with a wrong argument.
# I confirmed that I can just remove the fesetenv and instead add the following to "C compiler link flags"
# -Wl,CRT_fp8.o
# This might be more portable and the test still passes.
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

This is because cabal list-bin will output a Windows path (sometimes with \ sometimes with /)
so we normalise with CYGPATH

	cp -rfp $(shell cabal list-bin -v0 $(BUILD_ARGS) cabal-install:exe:cabal | $(CYGPATH)) $@


same thing in

	echo "extra-prog-path: $(shell echo '$(GHC_LIBDIR)' | $(CYGPATH_MIXED))/../mingw/bin" > $@

why not --with-gcc? try something like 

	if os(windows)
		with-gcc: 

$(EXE_EXT)? we need them at least with pre-requisites (otherwise they are not actual files)

make packagedb entries relocatable

	$(SED) -i \
		-e "s|haddock-interfaces:.*|haddock-interfaces: \"\$${pkgroot}/$3/html/libraries/$5/$1.haddock\"|" \
		-e "s|haddock-html:.*|haddock-html: \"\$${pkgroot}/$3/html/libraries/$5\"|" \
        -e "s|import-dirs:.*|import-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|library-dirs:.*|library-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|library-dirs-static:.*|library-dirs-static: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|dynamic-library-dirs:.*|dynamic-library-dirs: \"\$${pkgroot}/../lib/$4\"|" \
		-e "s|data-dir:.*|data-dir: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|include-dirs:.*|include-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}/include\"|" \
		-e "s|^    /.*||" \
		-e "s|^    [A-Z]:.*||" \

last line is important for windows C:\ABC -> ABC

	if os(windows)
		package rts
			ghc-options: "-optc-DHostArch=\"x86_64\""
			ghc-options: "-optc-DHostOS=\"mingw32\""
			ghc-options: "-optc-DHostPlatform=\"x86_64-unknown-mingw32\""
			ghc-options: "-optc-DHostVendor=\"unknown\""

why not

	if os(windows)
		package rts
			ghc-options: "-optc-DHostOS=\"mingw32\""

	if arch(x86_64)
		package rts
			ghc-options: "-optc-DHostArch=\"x86_64\""

add haddock to stage2

	packages:
		utils/haddock
		utils/haddock/haddock-api
		utils/haddock/haddock-library

	package haddock-api
		flags: +in-ghc-tree

	if !os(windows)
		packages:
			libraries/terminfo

	if !os(windows)
		package *
			library-for-ghci: True

	-- remove the threaded flag, so threaded is on
	package ghc-bin
		flags: +internal-interpreter


some cleanup in BaseDir.hs
some changes in ghc-bin.cabal

autoconf changes in ghc-internal

stage2

  DIST_DIR/bin
  DIST_DIR/lib/settings
  DIST_DIR/lib/package.conf.d
  DIST_DIR/lib/$(HOST_PLATFORM)/... # packages

stage3

  DIST_DIR/bin                                # links ghc -> $(TARGET_PLATFORM)-ghc
  DIST_DIR/lib/targets/$(TARGET_PLATFORM)/bin # copy unlit because bullshit
  DIST_DIR/lib/targets/$(TARGET_PLATFORM)/lib
  DIST_DIR/lib/targets/$(TARGET_PLATFORM)/lib/settings
  DIST_DIR/lib/targets/$(TARGET_PLATFORM)/lib/package.conf.d
  DIST_DIR/lib/targets/$(TARGET_PLATFORM)/lib/$(TARGET_PLATFORM)/... # packages

## Ideally

/bin
/lib
/lib/package.conf.d <- KILL
/lib/x86_64-unknown-linux
/lib/x86_64-unknown-linux/settings
/lib/x86_64-unknown-linux/package.conf.d
/lib/x86_64-unknown-linux/libHSbase-A.B.C.D-ghcX.Y.Z.so
/lib/x86_64-unknown-linux/base-A.B.C.D/libHSbase-A.B.C.D.a
/lib/javascript-unknown-ghcjs/
/lib/javascript-unknown-ghcjs/settings
/lib/javascript-unknown-ghcjs/package.conf.d
/lib/javascript-unknown-ghcjs/libHSbase-A.B.C.D-ghcX.Y.Z.so
/lib/javascript-unknown-ghcjs/base-A.B.C.D/libHSbase-A.B.C.D.a
