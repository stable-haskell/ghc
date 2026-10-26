#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

extern const size_t stubSizeAarch64;
bool needStubForRelAarch64(ll_elf_target_rel * rel);
bool needStubForRelaAarch64(ll_elf_target_rela * rel);
bool makeStubAarch64(ElfStub * s);

#endif
