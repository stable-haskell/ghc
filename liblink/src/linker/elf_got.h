#pragma once

#include "LinkerInternals.h"

#include <stdbool.h>
#include <linker/ElfTypes.h>

#if LL_BUILD_ELF
bool needGotSlot(ll_elf_target_sym * symbol);
bool makeGot(ObjectCode * oc);
bool fillGot(ObjectCode * oc);
bool verifyGot(ObjectCode * oc);
void freeGot(ObjectCode * oc);
#endif
