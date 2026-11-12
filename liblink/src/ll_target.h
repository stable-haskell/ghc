/* Object-code architecture selection. Host services use ll_platform.h instead.
 * A backend translation unit may select LL_TARGET_ARCH before including this
 * header; native builds default to the configured host architecture. */
#pragma once

#include "ll_platform.h"
#include "liblink/symbol_extras.h"

#define LL_ARCH_NATIVE_OTHER 0
#define LL_ARCH_I386 1
#define LL_ARCH_X86_64 2
#define LL_ARCH_ARM 3
#define LL_ARCH_AARCH64 4
#define LL_ARCH_RISCV64 5
#define LL_ARCH_POWERPC 6
#define LL_ARCH_SPARC 7

#ifndef LL_TARGET_ARCH
#if defined(i386_HOST_ARCH)
#define LL_TARGET_ARCH LL_ARCH_I386
#elif defined(x86_64_HOST_ARCH)
#define LL_TARGET_ARCH LL_ARCH_X86_64
#elif defined(arm_HOST_ARCH)
#define LL_TARGET_ARCH LL_ARCH_ARM
#elif defined(aarch64_HOST_ARCH) || defined(arm64_HOST_ARCH)
#define LL_TARGET_ARCH LL_ARCH_AARCH64
#elif defined(riscv64_HOST_ARCH)
#define LL_TARGET_ARCH LL_ARCH_RISCV64
#elif defined(powerpc_HOST_ARCH)
#define LL_TARGET_ARCH LL_ARCH_POWERPC
#elif defined(sparc_HOST_ARCH)
#define LL_TARGET_ARCH LL_ARCH_SPARC
#else
/* Retain native builds for architectures without a relocation implementation. */
#define LL_TARGET_ARCH LL_ARCH_NATIVE_OTHER
#endif
#endif

#if LL_TARGET_ARCH < LL_ARCH_NATIVE_OTHER || LL_TARGET_ARCH > LL_ARCH_SPARC
#error "unsupported LL_TARGET_ARCH"
#endif

#define LL_TARGET_I386 (LL_TARGET_ARCH == LL_ARCH_I386)
#define LL_TARGET_X86_64 (LL_TARGET_ARCH == LL_ARCH_X86_64)
#define LL_TARGET_ARM (LL_TARGET_ARCH == LL_ARCH_ARM)
#define LL_TARGET_AARCH64 (LL_TARGET_ARCH == LL_ARCH_AARCH64)
#define LL_TARGET_RISCV64 (LL_TARGET_ARCH == LL_ARCH_RISCV64)
#define LL_TARGET_POWERPC (LL_TARGET_ARCH == LL_ARCH_POWERPC)
#define LL_TARGET_SPARC (LL_TARGET_ARCH == LL_ARCH_SPARC)

#if LL_TARGET_I386 || LL_TARGET_ARM || LL_TARGET_POWERPC || LL_TARGET_SPARC
#define LL_TARGET_WORD_SIZE 4
#elif LL_TARGET_ARCH != LL_ARCH_NATIVE_OTHER
#define LL_TARGET_WORD_SIZE 8
#elif defined(__LP64__) || defined(_LP64)
#define LL_TARGET_WORD_SIZE 8
#else
#define LL_TARGET_WORD_SIZE 4
#endif

/* Format capabilities are target properties; they do not change shared RTS
 * layouts or choose the OS allocator, dynamic loader, or cache service. */
#define LL_ELF_NEED_SYMBOL_EXTRAS \
    (LL_TARGET_POWERPC || LL_TARGET_X86_64 || LL_TARGET_ARM || \
     LL_TARGET_AARCH64 || LL_TARGET_RISCV64)
#define LL_ELF_NEED_PLT \
    (LL_TARGET_ARM || LL_TARGET_AARCH64 || LL_TARGET_RISCV64)
#define LL_ELF_NEED_GOT LL_ELF_NEED_PLT
#define LL_MACHO_NEED_SYMBOL_EXTRAS (LL_TARGET_X86_64 || LL_TARGET_AARCH64)
#define LL_MACHO_NEED_PLT LL_TARGET_AARCH64

/* No new empty C struct is needed for targets without a symbol payload. The
 * allocator's legacy native representation remains separate during the split. */
#if LL_TARGET_X86_64
#define LL_TARGET_SYMBOL_EXTRA_SIZE sizeof(ll_symbol_extra_x86_64)
#elif LL_TARGET_ARM
#define LL_TARGET_SYMBOL_EXTRA_SIZE sizeof(ll_symbol_extra_arm)
#elif LL_TARGET_RISCV64
#define LL_TARGET_SYMBOL_EXTRA_SIZE sizeof(ll_symbol_extra_riscv64)
#elif LL_TARGET_POWERPC
#define LL_TARGET_SYMBOL_EXTRA_SIZE sizeof(ll_symbol_extra_powerpc)
#else
#define LL_TARGET_SYMBOL_EXTRA_SIZE 0
#endif
