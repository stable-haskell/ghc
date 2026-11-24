/* Private backend inclusion policy. Native format identity and OS services
 * remain host properties; enabling extra file parsers does not change either.
 * These flags must never select fields in the installed shared layouts.
 */
#pragma once

#include "ll_platform.h"

#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) \
 || defined(linux_android_HOST_OS) || defined(freebsd_HOST_OS) \
 || defined(kfreebsdgnu_HOST_OS) || defined(dragonfly_HOST_OS) \
 || defined(netbsd_HOST_OS) || defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)
#define LL_NATIVE_ELF 1
#else
#define LL_NATIVE_ELF 0
#endif

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
#define LL_NATIVE_MACHO 1
#else
#define LL_NATIVE_MACHO 0
#endif

#if defined(mingw32_HOST_OS)
#define LL_NATIVE_COFF 1
#else
#define LL_NATIVE_COFF 0
#endif

#if defined(LL_ALL_BACKENDS)
#define LL_BUILD_ELF 1
#define LL_BUILD_MACHO 1
#define LL_BUILD_COFF 1
#else
#define LL_BUILD_ELF LL_NATIVE_ELF
#define LL_BUILD_MACHO LL_NATIVE_MACHO
#define LL_BUILD_COFF LL_NATIVE_COFF
#endif
