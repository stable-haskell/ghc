#pragma once

#include <LinkerInternals.h>

#if defined(OBJFORMAT_MACHO)


#include "../MachOTypes.h"

extern const size_t stubSizeAarch64;
bool needStubForRelAarch64(MachORelocationInfo * rel);
bool makeStubAarch64(MachOStub * s);

#endif
