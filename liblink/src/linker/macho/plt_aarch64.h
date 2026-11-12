#pragma once

#include <LinkerInternals.h>

#if LL_BUILD_MACHO


#include "../MachOTypes.h"

extern const size_t macho_stubSizeAarch64;
bool macho_needStubForRelAarch64(MachORelocationInfo * rel);
bool macho_makeStubAarch64(MachOStub * s);

#endif
