#pragma once

#include "LinkerInternals.h"

#include <stdio.h>
#include "ll_path.h"
#include "PEi386Types.h"
#if defined(mingw32_HOST_OS)
#include "PEi386Host.h"
#endif
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>

#include "ll_begin_private.h"

#if LL_TARGET_X86_64
#define PEi386_IMAGE_OFFSET 4
#else
#define PEi386_IMAGE_OFFSET 0
#endif

#define PEi386_STRTAB_OFFSET 4

/********************************************
 * COFF/PE prototypes
 ********************************************/

void freePreloadObjectFile_PEi386( ObjectCode *oc );

bool checkAndLoadImportLibrary( ll_linker *L, pathchar* arch_name, char* member_name, FILE* f);

bool ocResolve_PEi386     ( ObjectCode* oc );
bool ocRunInit_PEi386     ( ObjectCode *oc );
bool ocRunFini_PEi386     ( ObjectCode *oc );
bool ocGetNames_PEi386    ( ObjectCode* oc );
bool ocVerifyImage_PEi386 ( ObjectCode* oc );
ll_symbol_addr *lookupSymbol_PEi386(ll_linker *L, ll_symbol_name *lbl,
                                    ObjectCode *dependent, ll_sym_type *type);
/* We use myindex to calculate array addresses, rather than
   simply doing the normal subscript thing.  That's because
   some of the above structs have sizes which are not
   a whole number of words.  GCC rounds their sizes up to a
   whole number of words, which means that the address calcs
   arising from using normal C indexing or pointer arithmetic
   are just plain wrong.  Sigh.
*/
LL_INLINE_HEADER unsigned char *
myindex ( int scale, void* base, int index )
{
    return
        ((unsigned char*)base) + scale * index;
}
char *get_name_string(
    ll_linker *L,
    unsigned char* name,
    ObjectCode* oc);

char* get_sym_name(
    ll_linker *L,
    uint8_t* name,
    ObjectCode* oc);

/* Util symbol handling functions.  */
COFF_OBJ_TYPE getObjectType ( ll_linker *L, char* image, pathchar* fileName );
COFF_HEADER_INFO* getHeaderInfo ( ObjectCode* oc );
size_t getSymbolSize ( ll_linker *L, COFF_HEADER_INFO *info );
uint32_t getSymSectionNumber ( ll_linker *L, COFF_HEADER_INFO *info, COFF_symbol* sym );
uint32_t getSymValue ( ll_linker *L, COFF_HEADER_INFO *info, COFF_symbol* sym );
uint8_t getSymStorageClass ( ll_linker *L, COFF_HEADER_INFO *info, COFF_symbol* sym );
uint8_t getSymNumberOfAuxSymbols ( ll_linker *L, COFF_HEADER_INFO *info, COFF_symbol* sym );
uint16_t getSymType ( ll_linker *L, COFF_HEADER_INFO *info, COFF_symbol* sym );
uint8_t* getSymShortName ( ll_linker *L, COFF_HEADER_INFO *info, COFF_symbol* sym );

#include "ll_end_private.h"
