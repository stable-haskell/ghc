#pragma once

#include <LinkerInternals.h>

#if defined(OBJFORMAT_MACHO)

#if LL_TARGET_X86_64
#  include <mach-o/x86_64/reloc.h>
#endif

#if LL_TARGET_AARCH64
#  include <mach-o/arm64/reloc.h>
#endif

#include "../MachOTypes.h"


extern const size_t stubSizeAarch64;
bool needStubForRelAarch64(MachORelocationInfo * rel);
bool makeStubAarch64(MachOStub * s);

#endif

