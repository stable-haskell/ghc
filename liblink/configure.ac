AC_INIT([GHC runtime linker package], [0.1.0.0], [libraries@haskell.org], [liblink])

AC_CONFIG_SRCDIR([src/ll_version.c])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([../m4])
AC_PREREQ([2.69])

AH_TOP([#ifndef LIBLINK_LL_AUTOCONF_H
#define LIBLINK_LL_AUTOCONF_H])
AH_BOTTOM([#endif /* LIBLINK_LL_AUTOCONF_H */])

AC_CONFIG_HEADERS([include/ll_autoconf.h])
AC_CONFIG_FILES([include/ll_platform.h liblink.buildinfo])

AC_PROG_CC([cc gcc clang])
AC_USE_SYSTEM_EXTENSIONS
AC_SYS_LARGEFILE
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
GHC_CONVERT_PLATFORM_PARTS([host], [Host])
FPTOOLS_SET_PLATFORM_VARS([host], [Host])

AC_CHECK_HEADERS([dlfcn.h fcntl.h nlist.h signal.h sys/mman.h sys/stat.h sys/types.h])
AC_CHECK_SIZEOF([void *])
AC_SEARCH_LIBS([dlopen], [dl])
AC_CHECK_FUNCS([dlinfo])
FP_CHECK_PTHREAD_LIB

dnl The underscore probe keys off TargetPlatform even in this native package.
TargetPlatform=$HostPlatform
FP_LEADING_UNDERSCORE
if test "$fptools_cv_leading_underscore" = yes; then
  AC_DEFINE([LEADING_UNDERSCORE], [1],
    [Define to 1 if C symbols have a leading underscore added by the compiler.])
fi

FP_RTS_LINKER_USE_MMAP

AC_SUBST([EXTRA_LIBS], [` printf " %s " "$LIBS" | sed -E 's/ -l([[^ ]]*)/\1 /g' `])
AC_OUTPUT
