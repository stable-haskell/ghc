#pragma once

struct ll_reloc_ops;
extern const struct ll_reloc_ops ll_elf_riscv64_reloc_ops;

#include "LinkerInternals.h"

#if LL_BUILD_ELF

bool
relocateObjectCodeRISCV64(ObjectCode * oc);

void flushInstructionCacheRISCV64(ObjectCode *oc);
#endif /* OBJETFORMAT_ELF */
