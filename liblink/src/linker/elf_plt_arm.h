#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

extern const size_t stubSizeArm;
bool needStubForRelArm(ll_elf_target_rel * rel);
bool needStubForRelaArm(ll_elf_target_rela * rel);
bool makeStubArm(ElfStub * s);

#endif
