/* Native SDK feature policy, separate from portable ELF file declarations.
 * Installed with the private friend headers: object layouts and sentinel
 * storage must use the same optional-feature decisions as the backend.
 */
#pragma once
#include "ll_platform.h"

#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) \
 || defined(linux_android_HOST_OS) || defined(freebsd_HOST_OS) \
 || defined(kfreebsdgnu_HOST_OS) || defined(dragonfly_HOST_OS) \
 || defined(netbsd_HOST_OS) || defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)
/* Retain the native SDK selection, including Solaris architecture hints. */
#if defined(i386_HOST_ARCH)
#define ELF_TARGET_386
#elif defined(x86_64_HOST_ARCH)
#define ELF_TARGET_X64_64
#define ELF_TARGET_AMD64
#endif
#if defined(openbsd_HOST_OS)
#include <elf_abi.h>
#else
#include <elf.h>
#endif
#if defined(EM_AARCH64)
#define LL_ELF_HAS_EM_AARCH64 1
#else
#define LL_ELF_HAS_EM_AARCH64 0
#endif
#if defined(EM_AMD64)
#define LL_ELF_HAS_EM_AMD64 1
#else
#define LL_ELF_HAS_EM_AMD64 0
#endif
#if defined(EM_ARM)
#define LL_ELF_HAS_EM_ARM 1
#else
#define LL_ELF_HAS_EM_ARM 0
#endif
#if defined(EM_IA_64)
#define LL_ELF_HAS_EM_IA_64 1
#else
#define LL_ELF_HAS_EM_IA_64 0
#endif
#if defined(EM_LOONGARCH)
#define LL_ELF_HAS_EM_LOONGARCH 1
#else
#define LL_ELF_HAS_EM_LOONGARCH 0
#endif
#if defined(EM_PPC64)
#define LL_ELF_HAS_EM_PPC64 1
#else
#define LL_ELF_HAS_EM_PPC64 0
#endif
#if defined(EM_RISCV)
#define LL_ELF_HAS_EM_RISCV 1
#else
#define LL_ELF_HAS_EM_RISCV 0
#endif
#if defined(EM_S390)
#define LL_ELF_HAS_EM_S390 1
#else
#define LL_ELF_HAS_EM_S390 0
#endif
#if defined(EM_SPARC32PLUS)
#define LL_ELF_HAS_EM_SPARC32PLUS 1
#else
#define LL_ELF_HAS_EM_SPARC32PLUS 0
#endif
#if defined(EM_X86_64)
#define LL_ELF_HAS_EM_X86_64 1
#else
#define LL_ELF_HAS_EM_X86_64 0
#endif
#if defined(SHN_XINDEX)
#define LL_ELF_HAS_SHN_XINDEX 1
#else
#define LL_ELF_HAS_SHN_XINDEX 0
#endif
#if defined(SHT_FINI_ARRAY)
#define LL_ELF_HAS_SHT_FINI_ARRAY 1
#else
#define LL_ELF_HAS_SHT_FINI_ARRAY 0
#endif
#if defined(SHT_INIT_ARRAY)
#define LL_ELF_HAS_SHT_INIT_ARRAY 1
#else
#define LL_ELF_HAS_SHT_INIT_ARRAY 0
#endif
#if defined(SHT_SYMTAB_SHNDX)
#define LL_ELF_HAS_SHT_SYMTAB_SHNDX 1
#else
#define LL_ELF_HAS_SHT_SYMTAB_SHNDX 0
#endif
#else
/* A foreign-format consumer has no native ELF SDK. Portable declarations
 * provide these features; native ELF hosts above retain their SDK policy. */
#define LL_ELF_HAS_EM_AARCH64 1
#define LL_ELF_HAS_EM_AMD64 1
#define LL_ELF_HAS_EM_ARM 1
#define LL_ELF_HAS_EM_IA_64 1
#define LL_ELF_HAS_EM_LOONGARCH 1
#define LL_ELF_HAS_EM_PPC64 1
#define LL_ELF_HAS_EM_RISCV 1
#define LL_ELF_HAS_EM_S390 1
#define LL_ELF_HAS_EM_SPARC32PLUS 1
#define LL_ELF_HAS_EM_X86_64 1
#define LL_ELF_HAS_SHN_XINDEX 1
#define LL_ELF_HAS_SHT_FINI_ARRAY 1
#define LL_ELF_HAS_SHT_INIT_ARRAY 1
#define LL_ELF_HAS_SHT_SYMTAB_SHNDX 1
#endif
