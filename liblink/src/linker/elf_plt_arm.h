#pragma once

#include "LinkerInternals.h"
#include "linker/ElfTypes.h"

#if LL_BUILD_ELF

/* A Thumb-2 branch carries a signed 25-bit byte displacement. */
static inline bool elf_arm_thumb_branch_overflow(uint32_t offset)
{
    return offset + UINT32_C(0x01000000) > UINT32_C(0x01ffffff);
}

extern const size_t elf_stubSizeArm;
bool elf_needStubForRelArm(ll_elf_target_rel * rel);
bool elf_needStubForRelaArm(ll_elf_target_rela * rel);
bool elf_makeStubArm(ElfStub * s);

#endif
