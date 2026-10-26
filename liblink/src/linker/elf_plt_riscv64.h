#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

extern const size_t stubSizeRISCV64;
bool needStubForRelRISCV64(ll_elf_target_rel * rel);
bool needStubForRelaRISCV64(ll_elf_target_rela * rel);
bool makeStubRISCV64(ll_linker *L, ElfStub * s);

#endif
