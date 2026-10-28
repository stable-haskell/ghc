#pragma once

#include "LinkerInternals.h"
#include "linker/ElfTypes.h"

#include "ll_begin_private.h"

void ocInit_ELF          ( ObjectCode* oc );
void ocDeinit_ELF        ( ObjectCode* oc );
int ocVerifyImage_ELF    ( ObjectCode* oc );
int ocValidateImage_ELF  ( ObjectCode* oc );
int ocGetNames_ELF       ( ObjectCode* oc );
int ocResolve_ELF        ( ObjectCode* oc );
int ocRunInit_ELF        ( ObjectCode* oc );
int ocRunFini_ELF        ( ObjectCode* oc );
int ocAllocateExtras_ELF ( ObjectCode *oc );
void *loadNativeObjFromLinkerScript_ELF( ll_linker *L, char **errmsg );

#include "ll_end_private.h"
