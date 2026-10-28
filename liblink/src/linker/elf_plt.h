#pragma once

#include <LinkerInternals.h>
#include "ll_backend.h"

#include "elf_plt_arm.h"
#include "elf_plt_aarch64.h"
#include "elf_plt_riscv64.h"

#if LL_TARGET_ARM || LL_TARGET_AARCH64 || LL_TARGET_RISCV64

#if LL_BUILD_ELF

#if LL_TARGET_AARCH64
#define __suffix__ Aarch64
#elif LL_TARGET_ARM
#define __suffix__ Arm
#elif LL_TARGET_RISCV64
#define __suffix__ RISCV64
#else
#error "unknown ELF PLT target"
#endif

#define PASTE(x,y) x ## y
#define EVAL(x,y) PASTE(x,y)
#define ADD_SUFFIX(x) EVAL(PASTE(x,),__suffix__)

unsigned  elf_numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex);

#define STUB_SIZE          ADD_SUFFIX(elf_stubSize)

bool elf_findStub(Section * section, void* * addr, uint8_t flags);
bool elf_makeStub(ll_linker *L, Section * section, void* * addr, void* got_addr, uint8_t flags);

#if LL_TARGET_ARM
/* The legacy stub API writes a host pointer, even for a 32-bit target. */
static inline bool
elf_arm_veneer(ll_linker *L, Section *section, ll_elf_target_addr *target,
               uint8_t flags)
{
    void *addr = (void *)(uintptr_t)*target;
    if (elf_findStub(section, &addr, flags) &&
        elf_makeStub(L, section, &addr, NULL, flags)) return true;
    *target = (ll_elf_target_addr)(uintptr_t)addr;
    return false;
}
#endif

/* Like the legacy helpers, zero means success. Target/flags retain the same
 * identity; output is written only on success. Storage stays section-owned.
 * The caller supplies non-NULL output and a section whose work and execution
 * extents correspond, including its reserved stub capacity. */
bool elf_findStubSite(Section *section, void *target, uint8_t flags,
                      ll_site *site);
bool elf_makeStubSite(ll_linker *L, Section *section, void *target,
                      void *got_addr, uint8_t flags, ll_site *site);

void elf_freeStubs(ll_linker *L, Section * section);

#endif // OBJECTFORMAT_ELF

#endif // arm/aarch64_HOST_ARCH/riscv64_HOST_ARCH
