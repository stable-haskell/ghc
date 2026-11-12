#pragma once

#include <stdio.h>

#include "MachOTypes.h"

struct ll_linker;

#include "ll_begin_private.h"

int    ocInit_MachO                 ( ObjectCode* oc );
void   ocDeinit_MachO               ( ObjectCode* oc );
int    ocVerifyImage_MachO          ( ObjectCode* oc );
int    ocBuildSegments_MachO        ( ObjectCode* oc );
int    ocGetNames_MachO             ( ObjectCode* oc );
int    ocResolve_MachO              ( ObjectCode* oc );
int    ocRunInit_MachO              ( ObjectCode* oc );
int    ocRunFini_MachO              ( ObjectCode* oc );
/* Returns padding in [0, 15], or -1 after recording a read/format/seek error. */
int    machoGetMisalignment         ( struct ll_linker *L, FILE * );
int    ocAllocateExtras_MachO       ( ObjectCode* oc );

SectionKind getSectionKind_MachO    ( MachOSection *macho );

#include "ll_end_private.h"
