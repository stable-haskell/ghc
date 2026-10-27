#pragma once

#include "liblink/coff.h"

typedef enum _COFF_OBJ_TYPE {
    COFF_IMAGE,
    COFF_ANON_OBJ,
    COFF_IMPORT_LIB,
    COFF_ANON_BIG_OBJ,
    COFF_UNKNOWN
} COFF_OBJ_TYPE;

typedef struct _COFF_HEADER_INFO {
   COFF_OBJ_TYPE type;
   uint16_t sizeOfOptionalHeader;
   uint16_t sizeOfHeader;
   uint32_t pointerToSymbolTable;
   uint32_t numberOfSymbols;
   uint32_t numberOfSections;
} COFF_HEADER_INFO;

/* Section 7.1 PE Specification */
typedef ll_coff_import_header COFF_import_header;
#define sizeof_COFF_import_Header sizeof(COFF_import_header)

typedef ll_coff_section COFF_section;
#define sizeof_COFF_section sizeof(COFF_section)

typedef ll_coff_symbol COFF_symbol_og;
#define sizeof_COFF_symbol_og sizeof(COFF_symbol_og)

typedef ll_coff_symbol_ex COFF_symbol_ex;
#define sizeof_COFF_symbol_ex sizeof(COFF_symbol_ex)

typedef ll_coff_relocation COFF_reloc;
#define sizeof_COFF_reloc sizeof(COFF_reloc)

/* Combine union of possible symbol types.  */
typedef
union _COFF_symbol {
    COFF_symbol_og og;
    COFF_symbol_ex ex;
} COFF_symbol;

typedef
struct {
    uint32_t TagIndex;
    uint32_t Characteristics;
} COFF_symbol_aux_weak_external;

/* Some alignment information.  */
typedef
struct _Alignments {
    uint32_t mask;
    uint32_t value;
} Alignments;

struct ll_coff_section_info {
    char* name;
    size_t alignment;
    COFF_reloc* relocs;
    uint32_t noRelocs;
    uint32_t props;
    uint64_t virtualSize;
    uint64_t virtualAddr;
 };

struct ll_coff_extra_allocation {
    void *allocation;
    void *work;
    uintptr_t exec;
    struct ll_coff_extra_allocation *next;
};

struct ll_coff_object_info {
    struct ll_coff_extra_allocation *noexec_extras;
    struct InitFiniList* init; // Freed by ocRunInit_PEi386
    struct InitFiniList* fini; // Freed by ocRunFini_PEi386
    struct _Section* pdata;
    struct _Section* xdata;
    COFF_HEADER_INFO* ch_info; // Freed by ocResolve_PEi386
    COFF_symbol* symbols;      // Freed by ocResolve_PEi386
    char* str_tab;
 };
