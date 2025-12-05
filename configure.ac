# configure.ac
AC_PREREQ([2.69])
AC_INIT([ghc-builder], [0.1.0], [your-email@example.com])
AC_CONFIG_SRCDIR([.]) # A representative .in file
AC_CONFIG_MACRO_DIR([m4])      # For any custom m4 macros

# --- Define GHC Build Options ---
# Usage: ./configure ProjectVersion=X.Y ...
AC_ARG_WITH([project-version], [AS_HELP_STRING([--with-project-version=VER], [GHC version (default: 9.14)])], [ProjectVersion="$withval"], [ProjectVersion="9.14"])
AC_ARG_WITH([project-version-int], [AS_HELP_STRING([--with-project-version-int=VER], [GHC version as int (default: 914)])], [ProjectVersionInt="$withval"], [ProjectVersionInt="914"])
AC_ARG_WITH([project-version-munged], [AS_HELP_STRING([--with-project-version-munged=VER], [GHC version "munged" (default: 9.14)])], [ProjectVersionMunged="$withval"], [ProjectVersionMunged="9.14"])
AC_ARG_WITH([project-version-for-lib], [AS_HELP_STRING([--with-project-version-for-lib=VER], [GHC version for libraries (default: 9.1400)])], [ProjectVersionForLib="$withval"], [ProjectVersionForLib="9.1400"])
AC_ARG_WITH([project-patch-level], [AS_HELP_STRING([--with-project-patch-level=VER], [GHC patchlevel version (default: 0)])], [ProjectPatchLevel="$withval"], [ProjectPatchLevel="0"])
AC_ARG_WITH([project-patch-level1], [AS_HELP_STRING([--with-project-patch-level1=VER], [GHC patchlevel1 version (default: 0)])], [ProjectPatchLevel1="$withval"], [ProjectPatchLevel1="0"])
AC_ARG_WITH([project-patch-level2], [AS_HELP_STRING([--with-project-patch-level2=VER], [GHC patchlevel2 version (default: 0)])], [ProjectPatchLevel2="$withval"], [ProjectPatchLevel2="0"])

# Git commit ID (captured at configure time)
AC_ARG_WITH([project-git-commit-id], [AS_HELP_STRING([--with-project-git-commit-id=ID], [Git commit hash (default: auto-detect)])],
    [ProjectGitCommitId="$withval"], [ProjectGitCommitId="$(git rev-parse HEAD 2>/dev/null || echo unknown)"])

# --- Platform Detection ---
# Auto-detect build machine's architecture and OS using uname, with override options
# These are used to generate GHC.Platform.Host module

# Detect default architecture from uname -m
default_arch=$(uname -m 2>/dev/null || echo unknown)
# Normalize common variants
case "$default_arch" in
    arm64) default_arch="aarch64" ;;
    amd64) default_arch="x86_64" ;;
esac

# Detect default OS from uname -s
default_os=$(uname -s 2>/dev/null | tr '[[A-Z]]' '[[a-z]]' || echo unknown)
# Normalize common variants
case "$default_os" in
    darwin*) default_os="darwin" ;;
    linux*) default_os="linux" ;;
    mingw*|msys*|cygwin*) default_os="mingw32" ;;
    freebsd*) default_os="freebsd" ;;
esac

AC_ARG_WITH([target-arch], [AS_HELP_STRING([--with-target-arch=ARCH], [Target architecture (default: auto-detect from uname)])],
    [TargetArch="$withval"], [TargetArch="$default_arch"])
AC_ARG_WITH([target-os], [AS_HELP_STRING([--with-target-os=OS], [Target OS (default: auto-detect from uname)])],
    [TargetOS="$withval"], [TargetOS="$default_os"])

# Shell functions to convert platform strings to Haskell types
# (Inline the relevant parts from m4/fptools_set_haskell_platform_vars.m4)
checkArch() {
    case $[]1 in
    i386)
        test -z "$[]2" || eval "$[]2=ArchX86"
        ;;
    x86_64|amd64)
        test -z "$[]2" || eval "$[]2=ArchX86_64"
        ;;
    powerpc)
        test -z "$[]2" || eval "$[]2=ArchPPC"
        ;;
    powerpc64*)
        test -z "$[]2" || eval "$[]2=\"ArchPPC_64 ELF_V2\""
        ;;
    s390x)
        test -z "$[]2" || eval "$[]2=ArchS390X"
        ;;
    arm)
        test -z "$[]2" || eval "$[]2=\"ArchARM ARMv7 VFPv3D16 HARD\""
        ;;
    aarch64)
        test -z "$[]2" || eval "$[]2=ArchAArch64"
        ;;
    riscv64)
        test -z "$[]2" || eval "$[]2=ArchRISCV64"
        ;;
    wasm32)
        test -z "$[]2" || eval "$[]2=ArchWasm32"
        ;;
    loongarch64)
        test -z "$[]2" || eval "$[]2=ArchLoongArch64"
        ;;
    javascript)
        test -z "$[]2" || eval "$[]2=ArchJavaScript"
        ;;
    *)
        test -z "$[]2" || eval "$[]2=ArchUnknown"
        ;;
    esac
}

checkOS() {
    case $[]1 in
    linux|linux-android)
        test -z "$[]2" || eval "$[]2=OSLinux"
        ;;
    darwin|ios|watchos|tvos)
        test -z "$[]2" || eval "$[]2=OSDarwin"
        ;;
    solaris2)
        test -z "$[]2" || eval "$[]2=OSSolaris2"
        ;;
    mingw32|mingw64|windows)
        test -z "$[]2" || eval "$[]2=OSMinGW32"
        ;;
    freebsd)
        test -z "$[]2" || eval "$[]2=OSFreeBSD"
        ;;
    dragonfly)
        test -z "$[]2" || eval "$[]2=OSDragonFly"
        ;;
    openbsd)
        test -z "$[]2" || eval "$[]2=OSOpenBSD"
        ;;
    netbsd)
        test -z "$[]2" || eval "$[]2=OSNetBSD"
        ;;
    wasi)
        test -z "$[]2" || eval "$[]2=OSWasi"
        ;;
    aix)
        test -z "$[]2" || eval "$[]2=OSAIX"
        ;;
    gnu)
        test -z "$[]2" || eval "$[]2=OSHurd"
        ;;
    ghcjs|js)
        test -z "$[]2" || eval "$[]2=OSGhcjs"
        ;;
    *)
        test -z "$[]2" || eval "$[]2=OSUnknown"
        ;;
    esac
}

# Convert target platform strings to Haskell types
checkArch "$TargetArch" "HaskellTargetArch"
checkOS "$TargetOS" "HaskellTargetOs"

AC_MSG_NOTICE([Target architecture: $TargetArch -> $HaskellTargetArch])
AC_MSG_NOTICE([Target OS: $TargetOS -> $HaskellTargetOs])

# Export these variables for substitution by AC_SUBST
AC_SUBST([ProjectVersion])
AC_SUBST([ProjectVersionInt])
AC_SUBST([ProjectVersionMunged])
AC_SUBST([ProjectVersionForLib])
AC_SUBST([ProjectPatchLevel])
AC_SUBST([ProjectPatchLevel1])
AC_SUBST([ProjectPatchLevel2])
AC_SUBST([ProjectGitCommitId])
AC_SUBST([TargetArch])
AC_SUBST([TargetOS])
AC_SUBST([HaskellTargetArch])
AC_SUBST([HaskellTargetOs])

# For ghc-boot-th.cabal.in
AC_SUBST([Suffix],[""])
AC_SUBST([SourceRoot],["."])

# --- Feature toggle (dynamic only) for imported project settings ---
AC_ARG_ENABLE([dynamic],
        [AS_HELP_STRING([--enable-dynamic],[Build shared libraries and enable dynamic RTS])],
        [enable_dynamic=$enableval],[enable_dynamic=no])

# Initialize accumulation variables
ALL_PACKAGES=""
CONSTRAINTS=""

# Provide defaults first (static)
APPEND_PKG_FIELD([shared: False])
APPEND_PKG_FIELD([executable-dynamic: False])

AS_IF([test "x$enable_dynamic" = "xyes"], [
        # Override (we rebuild list to avoid mixing static+dynamic lines)
        ALL_PACKAGES=""
        APPEND_PKG_FIELD([shared: True])
        APPEND_PKG_FIELD([executable-dynamic: True])
        APPEND_CONSTRAINT([rts +dynamic])
])

# Indent with two spaces for substitution blocks (uniform handling)
ALL_PACKAGES=`printf '%b' "$ALL_PACKAGES"`
CONSTRAINTS=`printf '%b' "$CONSTRAINTS"`
ALL_PACKAGES_INDENTED=`printf '%s' "$ALL_PACKAGES" | sed 's/^/  /'`
CONSTRAINTS_INDENTED=`printf '%s' "$CONSTRAINTS" | sed 's/^/  /'`
ALL_PACKAGES="$ALL_PACKAGES_INDENTED"
AS_IF([test "x$CONSTRAINTS" = "x"], [CONSTRAINTS="  -- (none)"], [CONSTRAINTS="$CONSTRAINTS_INDENTED"])

AC_SUBST([ALL_PACKAGES])
AC_SUBST([CONSTRAINTS])

# --- Define Programs ---
# We don't need to check for CC, MAKE_SET, and others for now, we only want substitution.
# AC_PROG_CC
# AC_PROG_MAKE_SET # To ensure 'set' works in Makefiles for undefined variables
AC_CHECK_PROGS([PYTHON], [python3 python python2])
AS_IF([test "x$PYTHON" = x], [AC_MSG_ERROR([Python interpreter not found. Please install Python or set PYTHON environment variable.])])
AC_SUBST([PYTHON])

# FIXME: For now we keep this check, but just widen it aggressively.
#        At some point we should just outright remove this.
LlvmMinVersion=10 # inclusive
LlvmMaxVersion=100 # not inclusive
AC_SUBST([LlvmMinVersion])
AC_SUBST([LlvmMaxVersion])

# --- Files to generate ---
# config.status will create these files by substituting @VAR@ placeholders.
AC_CONFIG_FILES([
    ghc/ghc-bin.cabal
    compiler/ghc.cabal
    compiler/GHC/CmmToLlvm/Version/Bounds.hs
    libraries/ghc-boot/ghc-boot.cabal
    libraries/ghc-boot/GHC/Version.hs
    libraries/ghc-boot/GHC/Platform/Host.hs
    libraries/ghc-boot-th/ghc-boot-th.cabal
    libraries/ghc-boot-th-next/ghc-boot-th-next.cabal
    libraries/ghc-heap/ghc-heap.cabal
    libraries/template-haskell/template-haskell.cabal
    libraries/ghci/ghci.cabal
    utils/ghc-pkg/ghc-pkg.cabal
    utils/ghc-iserv/ghc-iserv.cabal
    utils/runghc/runghc.cabal
    libraries/ghc-internal/ghc-internal.cabal
    libraries/ghc-experimental/ghc-experimental.cabal
    libraries/base/base.cabal
    rts/include/ghcversion.h
    cabal.project.stage2.settings
])

AC_OUTPUT

# After running ./configure, the following command can be used to see configured values:
# ./config.status --config
