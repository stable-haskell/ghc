#pragma once

#include "LinkerInternals.h"
#include "linker/ElfTypes.h"

#if LL_BUILD_ELF

extern const size_t elf_stubSizeArm;
bool elf_needStubForRelArm(ll_elf_target_rel * rel);
bool elf_needStubForRelaArm(ll_elf_target_rela * rel);
bool elf_makeStubArm(ElfStub * s);

#endif
