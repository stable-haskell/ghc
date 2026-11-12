#pragma once

#include "LinkerInternals.h"
#include "linker/ElfTypes.h"

#if LL_BUILD_ELF

extern const size_t elf_stubSizeAarch64;
bool elf_needStubForRelAarch64(ll_elf_target_rel * rel);
bool elf_needStubForRelaAarch64(ll_elf_target_rela * rel);
bool elf_makeStubAarch64(ElfStub * s);

#endif
