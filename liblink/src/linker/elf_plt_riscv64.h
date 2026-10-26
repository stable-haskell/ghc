#pragma once

#include "LinkerInternals.h"
#include "linker/ElfTypes.h"

#if LL_BUILD_ELF

extern const size_t elf_stubSizeRISCV64;
bool elf_needStubForRelRISCV64(ll_elf_target_rel * rel);
bool elf_needStubForRelaRISCV64(ll_elf_target_rela * rel);
bool elf_makeStubRISCV64(ll_linker *L, ElfStub * s);

#endif
