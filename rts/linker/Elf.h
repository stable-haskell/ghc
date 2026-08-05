#pragma once

#include "Rts.h"
#include "LinkerInternals.h"
#include "linker/ElfTypes.h"

#include "BeginPrivate.h"

void ocInit_ELF          ( ObjectCode* oc );
void ocDeinit_ELF        ( ObjectCode* oc );
int ocVerifyImage_ELF    ( ObjectCode* oc );
int ocGetNames_ELF       ( ObjectCode* oc );
int ocResolve_ELF        ( ObjectCode* oc );
int ocRunInit_ELF        ( ObjectCode* oc );
int ocRunFini_ELF        ( ObjectCode* oc );
int ocAllocateExtras_ELF ( ObjectCode *oc );
void *loadNativeObjFromLinkerScript_ELF( char **errmsg );

#if defined(aarch64_HOST_ARCH) && defined(OBJFORMAT_ELF) && RTS_LINKER_USE_MMAP
#define USE_LINKER_POOL 1
void linkerPoolProtect(void);
void * linkerPoolAlloc(SectionKind kind, StgWord align, StgWord size);
void * linkerPoolAllocGot(StgWord size);
#endif

#include "EndPrivate.h"
