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

ifndef TOOLCHAIN_MK
include mk/stable-haskell/toolchain.mk
endif

include mk/stable-haskell/boot.mk
include mk/stable-haskell/stage1.mk

# stage2 and 3 are somewhat intertwined
include mk/stable-haskell/stage2.mk
include mk/stable-haskell/stage3.mk

include mk/stable-haskell/test.mk

# --- Main Targets ---
#
all: _build/bindist/stage2


# --- Clean Targets ---
#
clean: clean-stage1 clean-stage2 clean-stage3
	rm -f libraries/ghc-boot-th-next/ghc-boot-th-next.cabal
	rm -f libraries/ghc-boot-th-next/ghc-boot-th-next.cabal.in
	rm -f libraries/ghc-boot-th-next/.synth-stamp

distclean: clean
	@echo "::group::Cleaning all generated files (distclean)..."
	rm -rf autom4te.cache
	rm -f config.status config.log config.h aclocal.m4
	rm -f $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES)
	rm -rf libraries/ghc-boot-th-next
	@echo "::endgroup::"

.PHONY: clean distclean all

