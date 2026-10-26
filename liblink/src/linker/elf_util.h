#pragma once

#include "LinkerInternals.h"
#include "linker/ElfTypes.h"

#if LL_BUILD_ELF

ElfSymbolTable * findSymbolTable(ObjectCode * oc,
                                 unsigned symbolTableIndex);

ElfSymbol * findSymbol(ObjectCode * oc,
                       unsigned symbolTableIndex,
                       unsigned long symbolIndex);

#endif
