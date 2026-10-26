#pragma once

#include <LinkerInternals.h>

#include "plt_aarch64.h"

#if LL_TARGET_AARCH64

#if defined(OBJFORMAT_MACHO)

#if LL_TARGET_AARCH64
#define __suffix__ Aarch64
#else
#error "unknown Mach-O PLT target"
#endif

#define PASTE(x,y) x ## y
#define EVAL(x,y) PASTE(x,y)
#define ADD_SUFFIX(x) EVAL(PASTE(x,),__suffix__)

unsigned numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex);

#define STUB_SIZE          ADD_SUFFIX(stubSize)

bool findStub(Section * section, void* * addr, uint8_t flags);
bool makeStub(ll_linker *L, Section * section, void* * addr, uint8_t flags);

void freeStubs(ll_linker *L, Section * section);

#endif // OBJECTFORMAT_MACHO

#endif // aarch64_HOST_ARCH

