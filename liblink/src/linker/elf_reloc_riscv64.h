#pragma once

#include "LinkerInternals.h"

#if LL_BUILD_ELF

bool
relocateObjectCodeRISCV64(ObjectCode * oc);

void flushInstructionCacheRISCV64(ObjectCode *oc);
#endif /* OBJETFORMAT_ELF */
