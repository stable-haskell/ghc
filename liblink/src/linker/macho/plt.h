#pragma once

#include <LinkerInternals.h>

#include "plt_aarch64.h"

#if LL_TARGET_AARCH64

#if LL_BUILD_MACHO

#if LL_TARGET_AARCH64
#define __suffix__ Aarch64
#else
#error "unknown Mach-O PLT target"
#endif

#define PASTE(x,y) x ## y
#define EVAL(x,y) PASTE(x,y)
#define ADD_SUFFIX(x) EVAL(PASTE(x,),__suffix__)

unsigned macho_numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex);

#define STUB_SIZE          ADD_SUFFIX(macho_stubSize)

bool macho_findStub(Section * section, void* * addr, uint8_t flags);
bool macho_makeStub(ll_linker *L, Section * section, void* * addr, uint8_t flags);

void macho_freeStubs(ll_linker *L, Section * section);

#endif // OBJECTFORMAT_MACHO

#endif // aarch64_HOST_ARCH

