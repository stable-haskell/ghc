#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"

#if LL_BUILD_MACHO

/* File records remain available to other backends; only this implementation
 * requires a supported Mach-O target. */
#if !(LL_TARGET_X86_64 || LL_TARGET_AARCH64)
#error Unknown Darwin architecture
#endif

/* for roundUpToPage */

#include "LinkerInternals.h"
#include "ll_failure.h"
#include "linker/MachO.h"
#include "ll_format.h"
#include "ll_backend.h"
#include "ll_noexec.h"

#include "linker/CacheFlush.h"
#include "linker/SymbolExtras.h"
#include "linker/MMap.h"
#include "linker/BackendMemory.h"

#include <string.h>
#include <errno.h>
#include <stdlib.h>

/*
  Support for MachO linking on Darwin/MacOS X
  by Wolfgang Thaller (wolfgang.thaller@gmx.net)

  I hereby formally apologize for the hackish nature of this code.
  Things that need to be done:
  *) add still more sanity checks.
*/
#if LL_TARGET_AARCH64
#  include "macho/plt.h"

/* aarch64 linker by moritz angermann <moritz@lichtzwerge.de> */

/* often times we need to extend some value of certain number of bits
 * int an int64_t for e.g. relative offsets.
 */
static int64_t signExtend(uint64_t val, uint8_t bits);
/* Helper functions to check some instruction properties */
static bool isVectorOp(uint32_t *p);
static bool isLoadStore(uint32_t *p);

/* aarch64 relocations may contain an addend already in the position
 * where we want to write the address offset to. Thus decoding as well
 * as encoding is needed.
 */
static bool fitsBits(ll_linker *L, size_t bits, int64_t value);
static bool decodeAddend(ObjectCode * oc, ll_site site,
                         MachORelocationInfo * ri, int64_t *addend);
static bool encodeAddend(ObjectCode * oc, ll_site site,
                  MachORelocationInfo * ri, int64_t addend, MachOSymbol * symbol);

/* Global Offset Table logic */
static bool isGotLoad(MachORelocationInfo * ri);
static bool needGotSlot(MachOSymbol * symbol);
static bool makeGot(ObjectCode * oc);
static void freeGot(ObjectCode * oc);
static void findInternalGotRefs(ObjectCode * oc);
#endif /* aarch64_HOST_ARCH */

/*
 * Initialize some common data in the object code so we don't have to
 * continuously look up the addresses.
 */
int
ocInit_MachO(ObjectCode * oc)
{
    ocDeinit_MachO(oc);

    oc->macho_info = (struct ll_macho_object_info*)LL_CALLOC_FOR(oc->L,
                1, sizeof *oc->macho_info,
                "ocInit_MachO(ObjectCodeFormatInfo)");
    if (oc->macho_info == NULL) {
        ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O object metadata");
        return 0;
    }
    oc->macho_info->header  = (MachOHeader *) oc->image;
    oc->macho_info->symCmd  = NULL;
    oc->macho_info->segCmd  = NULL;
    oc->macho_info->dsymCmd = NULL;

    MachOLoadCommand *lc = (MachOLoadCommand*)(oc->image + sizeof(MachOHeader));
    for(size_t i = 0; i < oc->macho_info->header->ncmds; i++) {
        if (lc->cmd == LL_MACHO_LC_SEGMENT || lc->cmd == LL_MACHO_LC_SEGMENT_64) {
            oc->macho_info->segCmd = (MachOSegmentCommand*) lc;
        }
        else if (lc->cmd == LL_MACHO_LC_SYMTAB) {
            oc->macho_info->symCmd = (MachOSymtabCommand*) lc;
        }
        else if (lc->cmd == LL_MACHO_LC_DYSYMTAB) {
            oc->macho_info->dsymCmd = (MachODsymtabCommand*) lc;
        }
        lc = (MachOLoadCommand *) ( ((char*)lc) + lc->cmdsize );
    }
    if (NULL == oc->macho_info->segCmd) {
        ll_error_set(oc->L, LL_E_FORMAT, "ocInit_MachO: no segment load command");
        LL_LOG_ERROR_FOR(oc->L, "ocInit_MachO: no segment load command");
        return 0;
    }

    oc->macho_info->macho_sections = (MachOSection*) (oc->macho_info->segCmd+1);
    oc->n_sections = oc->macho_info->segCmd->nsects;

    if (oc->macho_info->symCmd != NULL && oc->macho_info->symCmd->nsyms != 0) {
        const char *table = oc->image + oc->macho_info->symCmd->symoff;
        if ((uintptr_t)table % _Alignof(MachONList) == 0) {
            oc->macho_info->nlist = (MachONList *)table;
        } else {
            /* Merged objects can pack tables without native C alignment. */
            size_t bytes = (size_t)oc->macho_info->symCmd->nsyms * sizeof(MachONList);
            oc->macho_info->owned_nlist = LL_MALLOC_FOR(oc->L, bytes,
                                                 "ocInit_MachO(nlist)");
            if (oc->macho_info->owned_nlist == NULL) {
                ll_error_set(oc->L, LL_E_NOMEM,
                             "cannot allocate aligned Mach-O symbol table");
                return 0;
            }
            memcpy(oc->macho_info->owned_nlist, table, bytes);
            oc->macho_info->nlist = oc->macho_info->owned_nlist;
        }
    }
    oc->macho_info->names = oc->macho_info->symCmd == NULL
              ? NULL
              : (oc->image + oc->macho_info->symCmd->stroff);

    /* If we have symbols, allocate and fill the macho_symbols
     * This will make relocation easier.
     */
    oc->macho_info->n_macho_symbols = 0;
    oc->macho_info->macho_symbols = NULL;

    if(NULL != oc->macho_info->nlist && oc->macho_info->symCmd->nsyms != 0) {
        oc->macho_info->n_macho_symbols = oc->macho_info->symCmd->nsyms;
        oc->macho_info->macho_symbols = (MachOSymbol*)LL_CALLOC_FOR(oc->L,
                                    oc->macho_info->symCmd->nsyms,
                                    sizeof(MachOSymbol),
                                    "ocInit_MachO(MachOSymbol)");
        if (oc->macho_info->macho_symbols == NULL) {
            ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O symbol metadata");
            return 0;
        }
        for(uint32_t i = 0; i < oc->macho_info->symCmd->nsyms; i++) {
            oc->macho_info->macho_symbols[i].name  = oc->macho_info->names
                                             + oc->macho_info->nlist[i].n_un.n_strx;
            oc->macho_info->macho_symbols[i].nlist = &oc->macho_info->nlist[i];
             /* We don't have an address for this symbol yet; this
              * will be populated during ocGetNames_MachO. Hence init
              * with NULL
              */
            oc->macho_info->macho_symbols[i].addr  = NULL;
            oc->macho_info->macho_symbols[i].got_addr = NULL;
        }
    }
    return 1;
}

void
ocDeinit_MachO(ObjectCode * oc) {
    if (oc->macho_info != NULL) {
        if (oc->macho_info->common_allocation != NULL)
            free(oc->macho_info->common_allocation);
        else
            LL_FREE_FOR(oc->L, oc->macho_info->common_mem);
        if(oc->macho_info->n_macho_symbols > 0) {
            LL_FREE_FOR(oc->L, oc->macho_info->macho_symbols);
        }
#if LL_TARGET_AARCH64
        freeGot(oc);
#endif
        if(oc->sections != NULL) {
            for(int i = 0; i < oc->n_sections; i++) {
                /* Empty sections and partially built loads have no metadata. */
                if (oc->sections[i].macho_info != NULL) {
#if LL_TARGET_AARCH64
                    macho_freeStubs(oc->L, &oc->sections[i]);
#endif
                    LL_FREE_FOR(oc->L, oc->sections[i].macho_info->owned_relocations);
                    oc->sections[i].macho_info->owned_relocations = NULL;
                }
            }
        }
        LL_FREE_FOR(oc->L, oc->macho_info->owned_nlist);
        LL_FREE_FOR(oc->L, oc->macho_info);
        oc->macho_info = NULL;
    }
}

static int
resolveImports(
    ObjectCode* oc,
    MachOSection *sect,
    Section *target,
    const uint32_t *indirectSyms,
    uint32_t nIndirectSyms);

#if LL_MACHO_NEED_SYMBOL_EXTRAS
#if LL_TARGET_X86_64 || LL_TARGET_AARCH64

int
ocAllocateExtras_MachO(ObjectCode* oc)
{
    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocAllocateExtras_MachO: start\n"));

    if (NULL != oc->macho_info->symCmd) {
        LL_IF_DEBUG_FOR(oc->L, linker,
            LL_DEBUG_FOR(oc->L, "ocAllocateExtras_MachO: allocate %d symbols\n",
                oc->macho_info->symCmd->nsyms));
        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocAllocateExtras_MachO: done\n"));
        return ocAllocateExtras(oc, oc->macho_info->symCmd->nsyms, 0, 0,
                                LL_TARGET_SYMBOL_EXTRA_SIZE, ocInit_MachO);
    }

    LL_IF_DEBUG_FOR(oc->L, linker,
        LL_DEBUG_FOR(oc->L, "ocAllocateExtras_MachO: allocated no symbols\n"));
    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocAllocateExtras_MachO: done\n"));
    return ocAllocateExtras(oc, 0, 0, 0, LL_TARGET_SYMBOL_EXTRA_SIZE,
                            ocInit_MachO);
}

#else
#error Unknown MachO architecture
#endif /* HOST_ARCH */
#endif /* NEED_SYMBOL_EXTRAS */

static int
resolveImports(
    ObjectCode* oc,
    MachOSection *sect,
    Section *target,
    const uint32_t *indirectSyms,
    uint32_t nIndirectSyms)
{
    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "resolveImports: start\n"));

    if (target == NULL || target->size != sect->size ||
        (sect->size != 0 && target->start == NULL) ||
        sect->size % sizeof(void *) != 0 ||
        sect->reserved1 > nIndirectSyms ||
        sect->size / sizeof(void *) > nIndirectSyms - sect->reserved1) {
        ll_error_set(oc->L, LL_E_FORMAT, "Mach-O indirect pointer range is invalid");
        LL_LOG_ERROR_FOR(oc->L, "Mach-O indirect pointer range is invalid");
        return 0;
    }

    size_t slots = sect->size / sizeof(void *);
    /* Validate all entries before looking up symbols or writing any slot. */
    for (size_t i = 0; i < slots; i++)
    {
        uint32_t indirectSymbolIndex = indirectSyms[sect->reserved1+i];
        /* LOCAL/ABS entries need local or absolute resolution, which this
         * legacy importer does not implement. They are not symbol indexes.
         */
        if (indirectSymbolIndex & (LL_MACHO_INDIRECT_SYMBOL_LOCAL | LL_MACHO_INDIRECT_SYMBOL_ABS)) {
            ll_error_set(oc->L, LL_E_UNSUPPORTED,
                         "Mach-O local indirect pointer entries are unsupported");
            LL_LOG_ERROR_FOR(oc->L,
                         "Mach-O local indirect pointer entries are unsupported");
            return 0;
        }
        if (indirectSymbolIndex >= oc->macho_info->n_macho_symbols) {
            ll_error_set(oc->L, LL_E_FORMAT, "Mach-O indirect symbol index is invalid");
            LL_LOG_ERROR_FOR(oc->L, "Mach-O indirect symbol index is invalid");
            return 0;
        }
    }
    for (size_t i = 0; i < slots; i++)
    {
        uint32_t indirectSymbolIndex = indirectSyms[sect->reserved1+i];
        MachOSymbol *symbol = &oc->macho_info->macho_symbols[indirectSymbolIndex];
        ll_symbol_addr* addr = NULL;

        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "resolveImports: resolving %s\n", symbol->name));

        if ((symbol->nlist->n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_UNDF
            && (symbol->nlist->n_type & LL_MACHO_N_EXT) && (symbol->nlist->n_value != 0)) {
            addr = (ll_symbol_addr*) (symbol->nlist->n_value);
            LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "resolveImports: undefined external %s has value %p\n", symbol->name, addr));
        } else {
            addr = lookupDependentSymbol(oc->L, symbol->name, oc, NULL);
            LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "resolveImports: looking up %s, %p\n", symbol->name, addr));
        }

        if (addr == NULL)
        {
            ll_error_set(oc->L, LL_E_UNRESOLVED,
                         "%s: unknown symbol `%s'", oc->fileName, symbol->name);
            LL_LOG_ERROR_FOR(oc->L, "\nlookupSymbol failed in resolveImports\n"
                       "%s: unknown symbol `%s'", oc->fileName, symbol->name);
            return 0;
        }

        checkProddableBlock(oc->L, &oc->proddables,
                            (char *)target->start + i * sizeof(void *),
                            sizeof(void *));
        /* A file's section alignment need not align a C pointer lvalue. */
        memcpy((char *)target->start + i * sizeof(void *), &addr, sizeof(addr));
    }

    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "resolveImports: done\n"));
    return 1;
}

#if LL_TARGET_AARCH64
/* aarch64 linker by moritz angermann <moritz@lichtzwerge.de> */

static int64_t
signExtend(uint64_t val, uint8_t bits) {
    return (int64_t)(val << (64-bits)) >> (64-bits);
}

static bool
isVectorOp(uint32_t *p) {
    return (*p & 0x04800000) == 0x04800000;
}

static bool
isLoadStore(uint32_t *p) {
    return (*p & 0x3B000000) == 0x39000000;
}

static bool
decodeAddend(ObjectCode * oc, ll_site site, MachORelocationInfo * ri,
             int64_t *addend) {

    /* the instruction. It is 32bit wide */
    uint32_t * p = (uint32_t*)site.work;

    checkProddableBlock(oc->L, &oc->proddables, (void*)p, 1 << ri->r_length);

    switch(ri->r_type) {
        case LL_MACHO_ARM64_RELOC_UNSIGNED: {
            switch (ri->r_length) {
                case 0: *addend = signExtend(*(uint8_t*)p,  8 << ri->r_length); return true;
                case 1: *addend = signExtend(*(uint16_t*)p, 8 << ri->r_length); return true;
                case 2: *addend = signExtend(*(uint32_t*)p, 8 << ri->r_length); return true;
                case 3: *addend = signExtend(*(uint64_t*)p, 8 << ri->r_length); return true;
                default:
                    LL_FATAL_FOR(oc->L, "Unsupported r_length (%d) for UNSIGNED relocation",
                         ri->r_length);
            }
        }
        case LL_MACHO_ARM64_RELOC_SUBTRACTOR: {
            switch (ri->r_length) {
                case 0: *addend = signExtend(*(uint8_t*)p,  8 << ri->r_length); return true;
                case 1: *addend = signExtend(*(uint16_t*)p, 8 << ri->r_length); return true;
                case 2: *addend = signExtend(*(uint32_t*)p, 8 << ri->r_length); return true;
                case 3: *addend = signExtend(*(uint64_t*)p, 8 << ri->r_length); return true;
                default:
                    LL_FATAL_FOR(oc->L, "Unsupported r_length (%d) for SUBTRACTOR relocation",
                         ri->r_length);
            }
        }
        case LL_MACHO_ARM64_RELOC_BRANCH26:
            /* take the lower 26 bits and shift them by 2. The last two are
             * implicitly 0 (as the instructions must be aligned!) and sign
             * extend to 64 bits.
             */
            *addend = signExtend( (*p & 0x03FFFFFF) << 2, 28 ); return true;
        case LL_MACHO_ARM64_RELOC_PAGE21:
        case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGE21:
            /* take the instruction bits masked with 0x6 (0110), and push them
             * down. into the last two bits, and mask in the
             *
             * the 21 bits are encoded as follows in the instruction
             *
             * -**- ---* **** **** **** **** ***-- ----
             *  ^^
             *  ''-- these are the low two bits.
             */
            *addend = signExtend(   (*p & 0x60000000) >> 29
                               | ((*p & 0x01FFFFE0) >> 3) << 12, 33); return true;
        case LL_MACHO_ARM64_RELOC_PAGEOFF12:
        case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGEOFF12: {
            /* the 12 bits for the page offset are encoded from bit 11 onwards
             *
             * ---- ---- --** **** **** **-- ---- ----
             */
            int64_t a = (*p & 0x003FFC00) >> 10;
            int shift = 0;
            if (isLoadStore(p)) {
                shift = (*p >> 30) & 0x3;
                if(0 == shift && isVectorOp(p)) {
                    shift = 4;
                }
            }
            *addend = a << shift; return true;
        }
    }
    ll_error_set(oc->L, LL_E_UNSUPPORTED, "unsupported relocation type: %d\n", ri->r_type);
    LL_LOG_ERROR_FOR(oc->L, "unsupported relocation type: %d\n", ri->r_type);
    return false;
}

inline static bool
fitsBits(ll_linker *L, size_t bits, int64_t value) {
    if(bits == 64) return true;
    if(bits > 64) LL_FATAL_FOR(L, "fits_bits with %zu bits and an 64bit integer!", bits);
    return  0 == (value >> bits)   // All bits off: 0
        || -1 == (value >> bits);  // All bits on: -1
}

static bool
encodeAddend(ObjectCode * oc, ll_site site,
             MachORelocationInfo * ri, int64_t addend, MachOSymbol * symbol) {
    uint32_t * p = (uint32_t*)site.work;

    checkProddableBlock(oc->L, &oc->proddables, (void*)p, 1 << ri->r_length);

    const char *symbol_name = symbol && symbol->name ? (char*)symbol->name : "<unknown>";
    const char *file_name = oc->fileName ? (char*)oc->fileName : "<unknown>";

    switch (ri->r_type) {
        case LL_MACHO_ARM64_RELOC_UNSIGNED: {
            if(!fitsBits(oc->L, 8 << ri->r_length, addend)) {
                const char *library_info = OC_INFORMATIVE_FILENAME(oc);
                ll_error_set(oc->L, LL_E_RELOC_RANGE, "Relocation out of range for UNSIGNED in %s: symbol '%s', addend 0x%llx, address 0x%llx, library: %s",
                     file_name, symbol_name, (long long)addend, (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                LL_LOG_ERROR_FOR(oc->L, "Relocation out of range for UNSIGNED in %s: symbol '%s', addend 0x%llx, address 0x%llx, library: %s",
                     file_name, symbol_name, (long long)addend, (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                return false;
            }
            switch (ri->r_length) {
                case 0: *(uint8_t*)p  = (uint8_t)addend; break;
                case 1: *(uint16_t*)p = (uint16_t)addend; break;
                case 2: *(uint32_t*)p = (uint32_t)addend; break;
                case 3: *(uint64_t*)p = (uint64_t)addend; break;
                default:
                    LL_FATAL_FOR(oc->L, "Unsupported r_length (%d) for UNSIGNED relocation",
                         ri->r_length);
            }
            return true;
        }
        case LL_MACHO_ARM64_RELOC_SUBTRACTOR: {
            if(!fitsBits(oc->L, 8 << ri->r_length, addend)) {
                const char *library_info = OC_INFORMATIVE_FILENAME(oc);
                ll_error_set(oc->L, LL_E_RELOC_RANGE, "Relocation out of range for SUBTRACTOR in %s: symbol '%s', addend 0x%llx, address 0x%llx, library: %s",
                     file_name, symbol_name, (long long)addend, (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                LL_LOG_ERROR_FOR(oc->L, "Relocation out of range for SUBTRACTOR in %s: symbol '%s', addend 0x%llx, address 0x%llx, library: %s",
                     file_name, symbol_name, (long long)addend, (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                return false;
            }
            switch (ri->r_length) {
                case 0: *(uint8_t*)p  = (uint8_t)addend; break;
                case 1: *(uint16_t*)p = (uint16_t)addend; break;
                case 2: *(uint32_t*)p = (uint32_t)addend; break;
                case 3: *(uint64_t*)p = (uint64_t)addend; break;
                default:
                    LL_FATAL_FOR(oc->L, "Unsupported r_length (%d) for SUBTRACTOR relocation",
                         ri->r_length);
            }
            return true;
        }
        case LL_MACHO_ARM64_RELOC_BRANCH26: {
            /* We can only store 26 bits in the instruction, due to alignment we
             * do not need the last two bits of the value. If the value >> 2
             * still exceeds 26bits, we won't be able to reach it.
             */
            if(!fitsBits(oc->L, 26, addend >> 2)) {
                const char *library_info = OC_INFORMATIVE_FILENAME(oc);
                ll_error_set(oc->L, LL_E_RELOC_RANGE, "Relocation target for BRANCH26 out of range in %s: symbol '%s', addend 0x%llx (0x%llx >> 2), address 0x%llx, library: %s",
                     file_name, symbol_name, (long long)addend, (long long)(addend >> 2), (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                LL_LOG_ERROR_FOR(oc->L, "Relocation target for BRANCH26 out of range in %s: symbol '%s', addend 0x%llx (0x%llx >> 2), address 0x%llx, library: %s",
                     file_name, symbol_name, (long long)addend, (long long)(addend >> 2), (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                return false;
            }
            *p = (*p & 0xFC000000) | ((uint32_t)(addend >> 2) & 0x03FFFFFF);
            return true;
        }
        case LL_MACHO_ARM64_RELOC_PAGE21:
        case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGE21: {
            /* We store 21bits, in bits 6 to 24, and bits 30 and 31.
             * The encoded value describes a multiple of 4k pages, and together
             * with the PAGEOFF12 relocation allows to address a relative range
             * of +-4GB.
             */
            if(!fitsBits(oc->L, 21, addend >> 12)) {
                const char *reloc_type = (ri->r_type == LL_MACHO_ARM64_RELOC_PAGE21) ? "PAGE21" : "GOT_LOAD_PAGE21";
                const char *library_info = OC_INFORMATIVE_FILENAME(oc);
                ll_error_set(oc->L, LL_E_RELOC_RANGE, "Relocation target for %s out of range in %s: symbol '%s', addend 0x%llx (0x%llx >> 12), address 0x%llx, library: %s",
                     reloc_type, file_name, symbol_name, (long long)addend, (long long)(addend >> 12), (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                LL_LOG_ERROR_FOR(oc->L, "Relocation target for %s out of range in %s: symbol '%s', addend 0x%llx (0x%llx >> 12), address 0x%llx, library: %s",
                     reloc_type, file_name, symbol_name, (long long)addend, (long long)(addend >> 12), (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                return false;
            }
            /* Extract fields as unsigned bits, including negative deltas. */
            const uint64_t addend_bits = (uint64_t)addend;
            *p = (*p & 0x9F00001F) | (uint32_t)((addend_bits << 17) & 0x60000000)
                                   | (uint32_t)((addend_bits >> 9)  & 0x00FFFFE0);
            return true;
        }
        case LL_MACHO_ARM64_RELOC_PAGEOFF12:
        case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGEOFF12: {
            /* Store an offset into a page (4k). Depending on the instruction
             * the bits are stored at slightly different positions.
             */
            if(!fitsBits(oc->L, 12, addend)) {
                const char *library_info = OC_INFORMATIVE_FILENAME(oc);
                ll_error_set(oc->L, LL_E_RELOC_RANGE, "Relocation target for PAGEOFF12 out of range in %s: symbol '%s', addend 0x%llx, address 0x%llx, library: %s",
                     file_name, symbol_name, (long long)addend, (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                LL_LOG_ERROR_FOR(oc->L, "Relocation target for PAGEOFF12 out of range in %s: symbol '%s', addend 0x%llx, address 0x%llx, library: %s",
                     file_name, symbol_name, (long long)addend, (long long)ri->r_address, library_info ? (char*)library_info : "<unknown>");
                return false;
            }

            int shift = 0;
            if(isLoadStore(p)) {
                shift = (*p >> 30) & 0x3;
                if(0 == shift && isVectorOp(p)) {
                    shift = 4;
                }
            }
            *p = (*p & 0xFFC003FF)
               | ((uint32_t)(addend >> shift << 10) & 0x003FFC00);
            return true;
        }
    }
    ll_error_set(oc->L, LL_E_UNSUPPORTED, "unsupported relocation type: %d\n", ri->r_type);
    LL_LOG_ERROR_FOR(oc->L, "unsupported relocation type: %d\n", ri->r_type);
    return false;
}

/* Note [Symbols in need of GOT entries]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * As GOT entries require memory, we ideally want to avoid reserving
 * them for symbols where they are unnecessary. Specifically, most internal
 * symbols will not be referenced by the GOT, even in position independent code
 * (since you can instead use direct PC-relative addressing).
 *
 * However, it is nevertheless possible for internal symbols to be referenced
 * via the GOT. Consequently, we use the following strategy to determine whether
 * a symbol needs a GOT slot:
 *
 *  a. all undefined external symbols are given GOT entries
 *  b. all external symbols with cross-section refrences are given GOT entries
 *  c. all internal symbols for which there are GOT relocations are given GOT
 *     entries.
 *
 * Failing to consider (c) lead to #25577. For this we explicitly traverse
 * the relocations in findInternalGotRefs() looking for GOT relocations
 * referencing internal symbols, setting the MachOSymbol.needs_got flag for
 * each.
 */

// See Note [Symbols in need of GOT entries]
static void
findInternalGotRefs(ObjectCode * oc)
{
    for (int curSection = 0; curSection < oc->n_sections; curSection++) {
        Section * sect = &oc->sections[curSection];
        if (sect->macho_info == NULL)
            continue;
        MachOSection * msect = sect->macho_info->macho_section; // for access convenience
        MachORelocationInfo * relocs = sect->macho_info->relocation_info;
        for(uint32_t i = 0; i < msect->nreloc; i++) {
            MachORelocationInfo *ri = &relocs[i];
            if (isGotLoad(ri)) {
                MachOSymbol* symbol = &oc->macho_info->macho_symbols[ri->r_symbolnum];
                symbol->needs_got = true;
            }
        }
    }
}

static bool
isGotLoad(MachORelocationInfo * ri) {
    return ri->r_type == LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGE21
    ||  ri->r_type == LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGEOFF12;
}

/*
 * Check if we need a global offset table slot for a
 * given symbol. See Note [Symbols in need of GOT entries].
 */
static bool
needGotSlot(MachOSymbol * symbol) {
    // Does it have any internal references?
    if (symbol->needs_got) {
        return true;
    }

    return (symbol->nlist->n_type & LL_MACHO_N_EXT)             /* is an external symbol    */
        && (LL_MACHO_N_UNDF == (symbol->nlist->n_type & LL_MACHO_N_TYPE) /* and is undefined         */
            || LL_MACHO_NO_SECT != symbol->nlist->n_sect);      /*     or is defined in a
                                                        *        different section */
}

static bool
makeGot(ObjectCode * oc) {
    size_t got_slots = 0;

    for(size_t i=0; i < oc->macho_info->n_macho_symbols; i++)
        if(needGotSlot(&oc->macho_info->macho_symbols[i]))
            got_slots += 1;

    if(got_slots > 0) {
        oc->macho_info->got_size =  got_slots * sizeof(void*);
        if (ll_noexec_enabled(oc->L)) {
            ll_noexec_block block;
            if (!ll_noexec_allocate(oc->L, oc->macho_info->got_size, sizeof(void *), &block))
                return EXIT_FAILURE;
            oc->macho_info->got_start = block.site.work;
            oc->macho_info->got_exec = block.site.exec;
            oc->macho_info->got_allocation = block.allocation;
        } else {
            oc->macho_info->got_start = ll_mmap_anon_for_linker(oc->L, oc->macho_info->got_size);
            oc->macho_info->got_exec = (uintptr_t)oc->macho_info->got_start;
        }
        if( oc->macho_info->got_start == NULL ) {
            return EXIT_FAILURE;
        }
        /* update got_addr */
        size_t slot = 0;
        for(size_t i=0; i < oc->macho_info->n_macho_symbols; i++)
            if(needGotSlot(&oc->macho_info->macho_symbols[i])) {
                oc->macho_info->macho_symbols[i].got_addr
                    = ((uint8_t*)oc->macho_info->got_start)
                    + (slot * sizeof(void *));
                oc->macho_info->macho_symbols[i].got_exec =
                    oc->macho_info->got_exec + slot++ * sizeof(void *);
            }
    }
    return EXIT_SUCCESS;
}

static void
freeGot(ObjectCode * oc) {
    /* sanity check */
    if(NULL != oc->macho_info->got_start && oc->macho_info->got_size > 0) {
        if (oc->macho_info->got_allocation != NULL)
            free(oc->macho_info->got_allocation);
        else
            ll_munmap_for_linker(oc->L, oc->macho_info->got_start, oc->macho_info->got_size, "freeGot");
    }
    oc->macho_info->got_start = NULL;
    oc->macho_info->got_size = 0;
}

// Retrieve symbol value
static bool symbol_value(ObjectCode* oc, MachOSymbol* symbol, uint64_t *result) {
  uint64_t value = 0;
  if(symbol->nlist->n_type & LL_MACHO_N_EXT) {
      /* external symbols should be able to be
       * looked up via the lookupDependentSymbol function.
       * Either through the global symbol hashmap
       * or asking the system, if not found
       * in the symbol hashmap
       */
      value = (uint64_t)lookupDependentSymbol(oc->L, (char*)symbol->name, oc, NULL);
      if(!value) {
          ll_error_set(oc->L, LL_E_UNRESOLVED, "Could not lookup symbol: %s!", symbol->name);
          LL_LOG_ERROR_FOR(oc->L, "Could not lookup symbol: %s!", symbol->name);
          return false;
      }
  } else {
      value = (uint64_t)symbol->addr;    // address of the symbol.
  }
  *result = value;
  return true;
}

static int
relocateSectionAarch64(ObjectCode * oc, Section * section)
{
    if(section->size == 0)
        return 1;
    /* at this point, we have:
     *
     * - loaded the sections (potentially into non-contiguous memory),
     *   (in ocGetNames_MachO)
     * - registered exported symbols
     *   (in ocGetNames_MachO)
     * - and fixed the nlist[i].n_value for common storage symbols (N_UNDF,
     *   N_EXT and n_value != 0) so that they point into the common storage.
     *   (in ocGetNames_MachO)
     * - All oc->symbols however should now point at the right place.
     */

    /* we need to care about the explicit addend */
    int64_t explicit_addend = 0;
    size_t  nreloc = section->macho_info->macho_section->nreloc;

    for(size_t i = 0; i < nreloc; i++) {
        MachORelocationInfo * ri = &section->macho_info->relocation_info[i];
        const ll_site site = {.work = (uint8_t *)section->start + ri->r_address,
                              .exec = section->exec_start + ri->r_address};
        switch (ri->r_type) {
            case LL_MACHO_ARM64_RELOC_UNSIGNED: {
                MachOSymbol* symbol = &oc->macho_info->macho_symbols[ri->r_symbolnum];
                int64_t addend;
                if (!decodeAddend(oc, site, ri, &addend)) return 0;
                uint64_t value;
                if (!symbol_value(oc, symbol, &value)) return 0;
                if (!encodeAddend(oc, site, ri, value + addend, symbol)) return 0;
                break;
            }
            case LL_MACHO_ARM64_RELOC_SUBTRACTOR:
            {
                // subtractor and unsigned are called in tandem:
                // first  pc <- pc - symbol address (SUBTRACTOR)
                // second pc <- pc + symbol address (UNSIGNED)
                // to achieve pc <- pc + target - base.

                // check that the following relocation exists and has the
                // expected ARM64_RELOC_UNSIGNED type
                if(!(i+1 < nreloc)
                   || !(section->macho_info->relocation_info[i+1].r_type
                          == LL_MACHO_ARM64_RELOC_UNSIGNED)) {
                    ll_error_set(oc->L, LL_E_FORMAT, "SUBTRACTOR relocation *must* be followed by UNSIGNED relocation.");
                    LL_LOG_ERROR_FOR(oc->L, "SUBTRACTOR relocation *must* be followed by UNSIGNED relocation.");
                    return 0;
                }

                // we *know* that the next relocation is ARM64_RELOC_UNSIGNED
                // (see above). So let's process both relocations and write the
                // combined result in the target location. This prevents
                // overflow. (Compared to trying to store the intermediate
                // result which may not fit in the target bits).

                // sub part (ARM64_RELOC_SUBTRACTOR)
                MachOSymbol* symbol1 = &oc->macho_info->macho_symbols[ri->r_symbolnum];
                uint64_t sub_value;
                if (!symbol_value(oc, symbol1, &sub_value)) return 0;

                // add part (ARM64_RELOC_UNSIGNED)
                MachORelocationInfo * ri2 = &section->macho_info->relocation_info[i+1];
                MachOSymbol* symbol2 = &oc->macho_info->macho_symbols[ri2->r_symbolnum];
                uint64_t add_value;
                if (!symbol_value(oc, symbol2, &add_value)) return 0;

                // combine with addend and store
                int64_t addend;
                if (!decodeAddend(oc, site, ri, &addend)) return 0;
                if (!encodeAddend(oc, site, ri, addend - sub_value + add_value, symbol1)) return 0;

                // skip next relocation: we've already handled it
                i += 1;
                break;
            }
            case LL_MACHO_ARM64_RELOC_BRANCH26: {
                MachOSymbol* symbol = &oc->macho_info->macho_symbols[ri->r_symbolnum];

                // pre-existing addend
                int64_t addend;
                if (!decodeAddend(oc, site, ri, &addend)) return 0;
                // address of the branch (b/bl) instruction.
                uint64_t pc = site.exec;
                uint64_t value = 0;
                if(symbol->nlist->n_type & LL_MACHO_N_EXT) {
                    value = (uint64_t)lookupDependentSymbol(oc->L, (char*)symbol->name, oc, NULL);
                    if(!value) {
                        ll_error_set(oc->L, LL_E_UNRESOLVED, "Could not lookup symbol: %s!", symbol->name);
                        LL_LOG_ERROR_FOR(oc->L, "Could not lookup symbol: %s!", symbol->name);
                        return 0;
                    }
                } else {
                    value = (uint64_t)symbol->addr;    // address of the symbol.
                }
                // We've got:
                // + 2  bits, for alignment
                // + 26 bits for for the relocation value
                // - 1  bit for signage.
                //
                // Thus we can encode 26 bits for relocation, including the sign
                // bit. However as branches need to be 4-byte aligned, we only
                // need 26 bits to address a 28 bit range. Thus discarding the
                // sign bit, we can encode a range of +/- 27bits.
                //
                if((value - pc + addend) >> (2 + 26 - 1)) {
                    ll_site stub;
                    /* we need a stub */
                    /* check if we already have that stub */
                    if(macho_findStubSite(section, (void *)(uintptr_t)value, 0, &stub)) {
                        /* did not find it. Crete a new stub. */
                        if(macho_makeStubSite(oc->L, section, (void *)(uintptr_t)value, 0, &stub)) {
                            if (oc->L->error_status == LL_OK)
                                ll_error_set(oc->L, LL_E_RELOC_RANGE, "could not find or make stub");
                            LL_LOG_ERROR_FOR(oc->L, "could not find or make stub");
                            return 0;
                        }
                    }
                    value = stub.exec;
                }
                if (!encodeAddend(oc, site, ri, value - pc + addend, symbol)) return 0;
                break;
            }
            case LL_MACHO_ARM64_RELOC_PAGE21:
            case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGE21: {
                MachOSymbol* symbol = &oc->macho_info->macho_symbols[ri->r_symbolnum];
                int64_t addend;
                if (!decodeAddend(oc, site, ri, &addend)) return 0;
                if(!(explicit_addend == 0 || addend == 0)) {
                    ll_error_set(oc->L, LL_E_FORMAT, "explicit_addend and addend can't be set at the same time.");
                    LL_LOG_ERROR_FOR(oc->L, "explicit_addend and addend can't be set at the same time.");
                    return 0;
                }
                uint64_t pc = site.exec;
                uint64_t value = isGotLoad(ri) ? symbol->got_exec : (uintptr_t)symbol->addr;
                LL_ASSERT_FOR(oc->L, !isGotLoad(ri) || (symbol->got_addr != 0));
                if (!encodeAddend(oc, site, ri, ((value + addend + explicit_addend) & (-4096)) - (pc & (-4096)), symbol)) return 0;

                // reset, just in case.
                explicit_addend = 0;
                break;
            }
            case LL_MACHO_ARM64_RELOC_PAGEOFF12:
            case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGEOFF12: {
                MachOSymbol* symbol = &oc->macho_info->macho_symbols[ri->r_symbolnum];
                int64_t addend;
                if (!decodeAddend(oc, site, ri, &addend)) return 0;
                if(!(explicit_addend == 0 || addend == 0)) {
                    ll_error_set(oc->L, LL_E_FORMAT, "explicit_addend and addend can't be set at the same time.");
                    LL_LOG_ERROR_FOR(oc->L, "explicit_addend and addend can't be set at the same time.");
                    return 0;
                }
                uint64_t value = isGotLoad(ri) ? symbol->got_exec : (uintptr_t)symbol->addr;
                LL_ASSERT_FOR(oc->L, !isGotLoad(ri) || (symbol->got_addr != 0));
                if (!encodeAddend(oc, site, ri, 0xFFF & (value + addend + explicit_addend), symbol)) return 0;

                // reset, just in case.
                explicit_addend = 0;
                break;
            }
            case LL_MACHO_ARM64_RELOC_ADDEND: {
                explicit_addend = signExtend(ri->r_symbolnum, 24);
                if(!(i+1 < nreloc)
                   || !(section->macho_info->relocation_info[i+1].r_type == LL_MACHO_ARM64_RELOC_PAGE21
                        || section->macho_info->relocation_info[i+1].r_type == LL_MACHO_ARM64_RELOC_PAGEOFF12)) {
                    ll_error_set(oc->L, LL_E_FORMAT, "ADDEND relocation *must* be followed by PAGE or PAGEOFF relocation");
                    LL_LOG_ERROR_FOR(oc->L, "ADDEND relocation *must* be followed by PAGE or PAGEOFF relocation");
                    return 0;
                }
                break;
            }
            default: {
                ll_error_set(oc->L, LL_E_UNSUPPORTED, "Relocation of type: %d not (yet) supported!\n", ri->r_type);
                LL_LOG_ERROR_FOR(oc->L, "Relocation of type: %d not (yet) supported!\n", ri->r_type);
                return 0;
            }
        }
    }
    return 1;
}
#endif /* aarch64_HOST_ARCH */

#if LL_TARGET_X86_64
static int
checkMachORelocationSpan(ObjectCode *oc, void *address, size_t size)
{
    if (!containsSpan(&oc->proddables, (uintptr_t)address,
                       (uintptr_t)address + size)) {
        ll_error_set(oc->L, LL_E_FORMAT,
                     "checkProddableBlock: invalid fixup in runtime linker: %p", address);
        LL_LOG_ERROR_FOR(oc->L,
                     "checkProddableBlock: invalid fixup in runtime linker: %p", address);
        return 0;
    }
    return 1;
}

static int
relocateSection(ObjectCode* oc, int curSection)
{
    Section * sect = &oc->sections[curSection];

    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "relocateSection %d, info: %p\n", curSection, (void*)sect->macho_info));

    // empty sections (without segments), won't have their info filled.
    // there is no relocation to be done for them.
    if(sect->macho_info == NULL)
        return 1;

    MachOSection * msect = sect->macho_info->macho_section; // for access convenience
    MachORelocationInfo * relocs = sect->macho_info->relocation_info;
    MachOSymbol * symbols = oc->macho_info->macho_symbols;

    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "relocateSection %d (%.16s, %.16s): start\n",
                                curSection, msect->segname, msect->sectname));

    if(!strncmp(msect->sectname,"__la_symbol_ptr",16))
        return 1;
    else if(!strncmp(msect->sectname,"__nl_symbol_ptr",16))
        return 1;
    else if(!strncmp(msect->sectname,"__la_sym_ptr2",16))
        return 1;
    else if(!strncmp(msect->sectname,"__la_sym_ptr3",16))
        return 1;

    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "relocateSection: number of relocations: %d\n", msect->nreloc));

    for(uint32_t i = 0; i < msect->nreloc; i++)
    {
        MachORelocationInfo *reloc = &relocs[i];

        const ll_site site = {
            .work = (uint8_t *)sect->start + reloc->r_address,
            .exec = sect->exec_start + reloc->r_address
        };
        char *thingPtr = (char *)site.work;
        uint64_t thing;
        /* We shouldn't need to initialise this, but gcc on OS X 64 bit
           complains that it may be used uninitialized if we don't */
        uint64_t value = 0;
        uint64_t baseValue;
        Section *target_section = NULL;
        int type = reloc->r_type;
        int relocLenBytes;
        int nextInstrAdj = 0;

        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "relocateSection: relocation %d\n", i));
        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : type      = %d\n", reloc->r_type));
        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : address   = %d\n", reloc->r_address));
        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : symbolnum = %u\n", reloc->r_symbolnum));
        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : pcrel     = %d\n", reloc->r_pcrel));
        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : length    = %d\n", reloc->r_length));
        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : extern    = %d\n", reloc->r_extern));
        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : type      = %d\n", reloc->r_type));

        if (!checkMachORelocationSpan(oc, thingPtr, (size_t)1 << reloc->r_length))
            return 0;

        switch(reloc->r_length)
        {
            case 0:
                thing = *(uint8_t*)thingPtr;
                relocLenBytes = 1;
                break;
            case 1: {
                uint16_t operand;
                memcpy(&operand, thingPtr, sizeof(operand));
                thing = operand;
                relocLenBytes = 2;
                break;
            }
            case 2: {
                uint32_t operand;
                memcpy(&operand, thingPtr, sizeof(operand));
                thing = operand;
                relocLenBytes = 4;
                break;
            }
            case 3:
                memcpy(&thing, thingPtr, sizeof(thing));
                relocLenBytes = 8;
                break;
            default:
                LL_FATAL_FOR(oc->L, "Unknown size.");
        }

        /*
         * With SIGNED_N the relocation is not at the end of the
         * instruction and baseValue needs to be adjusted accordingly.
         */
        switch (type) {
            case LL_MACHO_X86_64_RELOC_SIGNED_1:
                nextInstrAdj = 1;
                break;
            case LL_MACHO_X86_64_RELOC_SIGNED_2:
                nextInstrAdj = 2;
                break;
            case LL_MACHO_X86_64_RELOC_SIGNED_4:
                nextInstrAdj = 4;
                break;
        }
        baseValue = site.exec + relocLenBytes + nextInstrAdj;


        LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                 LL_DEBUG_FOR(oc->L, "relocateSection: length = %d, thing = %" PRId64 ", baseValue = %p\n",
                            reloc->r_length, thing, (char *)baseValue));

        if (type == LL_MACHO_X86_64_RELOC_GOT
         || type == LL_MACHO_X86_64_RELOC_GOT_LOAD)
        {
            MachOSymbol *symbol = &symbols[reloc->r_symbolnum];
            ll_symbol_name* nm = symbol->name;
            ll_symbol_addr* addr = NULL;

            LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "relocateSection: making jump island for %s, extern = %d, X86_64_RELOC_GOT\n",
                                        nm, reloc->r_extern));

            if (reloc->r_extern == 0) {
                    LL_LOG_ERROR_FOR(oc->L, "\nrelocateSection: global offset table relocation for symbol with r_extern == 0\n");
            }

            if (symbol->nlist->n_type & LL_MACHO_N_EXT) {
                    // The external bit is set, meaning the symbol is exported,
                    // and therefore can be looked up in this object module's
                    // symtab, or it is undefined, meaning dlsym must be used
                    // to resolve it.

                    addr = lookupDependentSymbol(oc->L, nm, oc, NULL);
                    LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                             LL_DEBUG_FOR(oc->L, "relocateSection: looked up %s, "
                                        "external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n"
                                        "               : addr = %p\n", nm, addr));

                    if (addr == NULL) {
                            ll_error_set(oc->L, LL_E_UNRESOLVED,
                                       "\nlookupSymbol failed in relocateSection (RELOC_GOT)\n"
                                       "%s: unknown symbol `%s'", oc->fileName, nm);
                            LL_LOG_ERROR_FOR(oc->L, "\nlookupSymbol failed in relocateSection (RELOC_GOT)\n"
                                       "%s: unknown symbol `%s'", oc->fileName, nm);
                            return 0;
                    }
            } else {
                    LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "relocateSection: %s is not an exported symbol\n", nm));

                    // The symbol is not exported, or defined in another
                    // module, so it must be in the current object module,
                    // at the location given by the section index and
                    // symbol address (symbol->n_value)

                    if ((symbol->nlist->n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_SECT) {
                        if (symbol->addr == NULL) {
                            ll_error_set(oc->L, LL_E_UNRESOLVED,
                                         "relocateSection: address of internal symbol %s was not resolved\n", nm);
                            LL_LOG_ERROR_FOR(oc->L, "relocateSection: address of internal symbol %s was not resolved\n", nm);
                            return 0;
                        }

                        addr = symbol->addr;

                        LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                                 LL_DEBUG_FOR(oc->L, "relocateSection: calculated relocation of "
                                            "non-external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n"));
                        LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                                 LL_DEBUG_FOR(oc->L, "               : addr = %p\n", addr));
                    } else {
                        ll_error_set(oc->L, LL_E_FORMAT,
                                   "\nrelocateSection: %s is not exported,"
                                   " and should be defined in a section, but isn't!\n", nm);
                        LL_LOG_ERROR_FOR(oc->L, "\nrelocateSection: %s is not exported,"
                                   " and should be defined in a section, but isn't!\n", nm);
                        return 0;
                    }
            }

            // creates a jump island for every relocation entry for a symbol
            // TODO (AP): use got_addr to store the loc. of a jump island to reuse later
            value = ll_make_symbol_extra_x86_64_site(oc, reloc->r_symbolnum,
                                                      (uint64_t)addr).exec;

            type = LL_MACHO_X86_64_RELOC_SIGNED;
        }
        else if (reloc->r_extern)
        {
            MachOSymbol *symbol = &symbols[reloc->r_symbolnum];
            ll_symbol_name* nm = symbol->name;
            ll_symbol_addr* addr = NULL;

            LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "relocateSection: looking up external symbol %s\n", nm));
            LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : type  = %d\n", symbol->nlist->n_type));
            LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : sect  = %d\n", symbol->nlist->n_sect));
            LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : desc  = %d\n", symbol->nlist->n_desc));
            LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "               : value = %p\n", (void *)symbol->nlist->n_value));

            if ((symbol->nlist->n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_SECT) {
                if (symbol->addr == NULL) {
                    ll_error_set(oc->L, LL_E_UNRESOLVED,
                                 "relocateSection: address of external symbol %s was not resolved\n", nm);
                    LL_LOG_ERROR_FOR(oc->L,
                                 "relocateSection: address of external symbol %s was not resolved\n", nm);
                    return 0;
                }
                value = (uint64_t) symbol->addr;
                LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                         LL_DEBUG_FOR(oc->L, "relocateSection, defined external symbol %s, relocated address %p\n",
                                    nm, (void *)value));
            }
            else {
                addr = lookupDependentSymbol(oc->L, nm, oc, NULL);
                if (addr == NULL)
                {
                     ll_error_set(oc->L, LL_E_UNRESOLVED,
                                "\nlookupSymbol failed in relocateSection (relocate external)\n"
                                "%s: unknown symbol `%s'", oc->fileName, nm);
                     LL_LOG_ERROR_FOR(oc->L, "\nlookupSymbol failed in relocateSection (relocate external)\n"
                                "%s: unknown symbol `%s'", oc->fileName, nm);
                     return 0;
                }

                value = (uint64_t) addr;
                LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                         LL_DEBUG_FOR(oc->L, "relocateSection: external symbol %s, address %p\n",
                                    nm, (void *)value));
            }
        }
        else
        {
            /* Since the relocation is internal, r_symbolnum contains a section
             * number relative to which the relocation is.  Depending on whether
             * the relocation is unsigned or signed, the given displacement is
             * relative to the image or the section respectively.
             *
             * For instance, in a signed case:
             * thing = <displ. to to section r_symbolnum *in the image*> (1)
             *       + <offset within r_symbolnum section>
             * (1) needs to be updated due to different section placement in memory.
             */

            if (!(reloc->r_symbolnum > 0)) {
                ll_error_set(oc->L, LL_E_FORMAT,
                   "relocateSection: unsupported r_symbolnum = %" PRIu32 " < 1 for internal relocation",
                   reloc->r_symbolnum);
                LL_LOG_ERROR_FOR(oc->L,
                   "relocateSection: unsupported r_symbolnum = %" PRIu32 " < 1 for internal relocation",
                   reloc->r_symbolnum);
                return 0;
            }

            int targetSecNum = reloc->r_symbolnum - 1; // sec numbers start with 1
            Section * targetSec = &oc->sections[targetSecNum];
            target_section = targetSec;
            MachOSection * targetMacho = targetSec->macho_info->macho_section;

            LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                     LL_DEBUG_FOR(oc->L, "relocateSection: internal relocation relative to section %d (%.16s, %.16s)\n",
                                targetSecNum, targetMacho->segname, targetMacho->sectname));

            switch (type) {
            case LL_MACHO_X86_64_RELOC_UNSIGNED: {
                if (!(thing >= targetMacho->addr)) {
                    ll_error_set(oc->L, LL_E_FORMAT,
                       "relocateSection: unsigned displacement %" PRIx64 "before target section start address %" PRIx64 "\n",
                       thing, (uint64_t) targetMacho->addr);
                    LL_LOG_ERROR_FOR(oc->L,
                       "relocateSection: unsigned displacement %" PRIx64 "before target section start address %" PRIx64 "\n",
                       thing, (uint64_t) targetMacho->addr);
                    return 0;
                }

                uint64_t thingRelativeOffset = thing - targetMacho->addr;
                LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                         LL_DEBUG_FOR(oc->L, "                 "
                                    "unsigned displacement %" PRIx64 " with section relative offset %" PRIx64 "\n",
                                            thing, thingRelativeOffset));

                thing = targetSec->exec_start + thingRelativeOffset;
                LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                         LL_DEBUG_FOR(oc->L, "                 "
                                    "relocated address is %p\n", (void *) thing));

                /* Compared to external relocation we don't need to adjust value
                 * any further since thing already has absolute address.
                 */
                value = 0;
                break;
            }
            case LL_MACHO_X86_64_RELOC_SIGNED:
            case LL_MACHO_X86_64_RELOC_SIGNED_1:
            case LL_MACHO_X86_64_RELOC_SIGNED_2:
            case LL_MACHO_X86_64_RELOC_SIGNED_4: {
                uint32_t baseValueOffset = reloc->r_address + relocLenBytes + nextInstrAdj;
                uint64_t imThingLoc = msect->addr + baseValueOffset + (int64_t) thing;

                if (!(imThingLoc >= targetMacho->addr)) {
                    ll_error_set(oc->L, LL_E_FORMAT,
                       "relocateSection: target location %p in image before target section start address %p\n",
                       (void *) imThingLoc, (void *) targetMacho->addr);
                    LL_LOG_ERROR_FOR(oc->L,
                       "relocateSection: target location %p in image before target section start address %p\n",
                       (void *) imThingLoc, (void *) targetMacho->addr);
                    return 0;
                }

                int64_t thingRelativeOffset = imThingLoc - targetMacho->addr;
                LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                     LL_DEBUG_FOR(oc->L, "                 "
                                "original displacement %" PRId64 " to %p with section relative offset %" PRIu64 "\n",
                                thing, (void *) imThingLoc, thingRelativeOffset));

                thing = (int64_t) (targetSec->exec_start + thingRelativeOffset)
                                - (sect->exec_start + baseValueOffset);
                value = baseValue; // so that it further cancels out with baseValue
                LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                         LL_DEBUG_FOR(oc->L, "                 "
                                    "relocated displacement %" PRId64 " to %p\n",
                                    (int64_t) thing, (void *) (baseValue + thing)));
                break;
            }
            default:
                ll_error_set(oc->L, LL_E_UNSUPPORTED,
                             "relocateSection: unexpected internal relocation type %d\n", type);
                LL_LOG_ERROR_FOR(oc->L, "relocateSection: unexpected internal relocation type %d\n", type);
                return 0;
            }
        }

        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "relocateSection: value = %p\n", (void *) value));

        if (type == LL_MACHO_X86_64_RELOC_BRANCH)
        {
            if((int32_t)(value - baseValue) != (int64_t)(value - baseValue))
            {
                LL_CHECK_FOR(oc->L, reloc->r_extern);
                value = ll_make_symbol_extra_x86_64_site(oc, reloc->r_symbolnum,
                                                          value).exec
                      + offsetof(ll_symbol_extra_x86_64, jumpIsland);
            }
            if (!((int32_t)(value - baseValue) == (int64_t)(value - baseValue))) {
                ll_error_set(oc->L, LL_E_RELOC_RANGE, "Mach-O branch island is out of range");
                LL_LOG_ERROR_FOR(oc->L, "Mach-O branch island is out of range");
                return 0;
            }
            type = LL_MACHO_X86_64_RELOC_SIGNED;
        }

        switch(type)
        {
            case LL_MACHO_X86_64_RELOC_UNSIGNED:
                if (reloc->r_pcrel) {
                    ll_error_set(oc->L, LL_E_FORMAT, "Mach-O UNSIGNED relocation is PC-relative");
                    LL_LOG_ERROR_FOR(oc->L, "Mach-O UNSIGNED relocation is PC-relative");
                    return 0;
                }
                thing += value;
                break;
            case LL_MACHO_X86_64_RELOC_SIGNED:
            case LL_MACHO_X86_64_RELOC_SIGNED_1:
            case LL_MACHO_X86_64_RELOC_SIGNED_2:
            case LL_MACHO_X86_64_RELOC_SIGNED_4:
                if (!reloc->r_pcrel) {
                    ll_error_set(oc->L, LL_E_FORMAT, "Mach-O SIGNED relocation is not PC-relative");
                    LL_LOG_ERROR_FOR(oc->L, "Mach-O SIGNED relocation is not PC-relative");
                    return 0;
                }
                thing += value - baseValue;
                break;
            case LL_MACHO_X86_64_RELOC_SUBTRACTOR:
                if (reloc->r_pcrel) {
                    ll_error_set(oc->L, LL_E_FORMAT, "Mach-O SUBTRACTOR relocation is PC-relative");
                    LL_LOG_ERROR_FOR(oc->L, "Mach-O SUBTRACTOR relocation is PC-relative");
                    return 0;
                }
                thing -= value;
                break;
            default:
                ll_error_set(oc->L, LL_E_UNSUPPORTED, "unknown relocation");
                LL_LOG_ERROR_FOR(oc->L, "unknown relocation");
                return 0;
        }

        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "relocateSection: thing = %p\n", (void *) thing));

        /* Thing points to memory within one of the relocated sections. We can
         * probe the first byte to sanity check internal relocations.
         */
        if (0 == reloc->r_extern) {
            uintptr_t target = reloc->r_pcrel ? thing + baseValue : thing;
            if (target_section->exec_start != (uintptr_t)target_section->start) {
                target = (uintptr_t)target_section->start +
                         (target - target_section->exec_start);
            }
            if (reloc->r_pcrel) {
                if (!checkMachORelocationSpan(oc, (void *)target, 1))
                    return 0;
            } else {
                if (!checkMachORelocationSpan(oc, (void *)target, 1))
                    return 0;
            }
        }

        switch(reloc->r_length)
        {
            case 0:
                *(uint8_t*)thingPtr = thing;
                break;
            case 1: {
                uint16_t operand = (uint16_t)thing;
                memcpy(thingPtr, &operand, sizeof(operand));
                break;
            }
            case 2: {
                uint32_t operand = (uint32_t)thing;
                memcpy(thingPtr, &operand, sizeof(operand));
                break;
            }
            case 3:
                memcpy(thingPtr, &thing, sizeof(thing));
                break;
        }
    }

    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "relocateSection: done\n"));
    return 1;
}
#endif /* x86_64_HOST_ARCH */

SectionKind
getSectionKind_MachO(MachOSection *section)
{
    uint8_t s_type = section->flags & LL_MACHO_SECTION_TYPE;
    if (s_type == LL_MACHO_S_MOD_INIT_FUNC_POINTERS) {
        return SECTIONKIND_INIT_ARRAY;
    } else if (s_type == LL_MACHO_S_MOD_TERM_FUNC_POINTERS) {
        return SECTIONKIND_FINI_ARRAY;
    } else if (0==strncmp(section->segname,"__TEXT",16)) {
        return SECTIONKIND_CODE_OR_RODATA;
    } else if (0==strncmp(section->segname,"__DATA",16)) {
        return SECTIONKIND_RWDATA;
    } else {
        return SECTIONKIND_OTHER;
    }
}

/* Calculate the # of active segments and their sizes based on section
 * sizes and alignments. This is done in 2 passes over sections:
 * 1. Calculate how many sections is going to be in each segment and
 * the total segment size.
 * 2. Fill in segment's sections_idx arrays.
 *
 * gbZerofillSegment is there because of this comment in mach-o/loader.h:
 * The gigabyte zero fill sections, those with the section type
 * S_GB_ZEROFILL, can only be in a segment with sections of this
 * type. These segments are then placed after all other segments.
 */
int
ocBuildSegments_MachO(ObjectCode *oc)
{
    int n_rxSections = 0;
    size_t size_rxSegment = 0;
    Segment *rxSegment = NULL;

    int n_rwSections = 0;
    size_t size_rwSegment = 0;
    Segment *rwSegment = NULL;

    int n_gbZerofills = 0;
    size_t size_gbZerofillSegment = 0;
    Segment *gbZerofillSegment = NULL;

    size_t max_alignment[3] = {1, 1, 1};

    int n_activeSegments = 0;
    int curSegment = 0;
    size_t size_compound;

    Segment *segments = NULL;
    void *mem = NULL, *curMem = NULL;

    for (int i = 0; i < oc->n_sections; i++) {
        MachOSection *macho = &oc->macho_info->macho_sections[i];
        if (0 == macho->size) {
            LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocBuildSegments_MachO: found a zero length section, skipping\n"));
            continue;
        }

        size_t alignment = 1 << macho->align;

        if (LL_MACHO_S_GB_ZEROFILL == (macho->flags & LL_MACHO_SECTION_TYPE)) {
            size_gbZerofillSegment = ll_round_up_to_align(size_gbZerofillSegment, alignment);
            size_gbZerofillSegment += macho->size;
            if (alignment > max_alignment[2]) max_alignment[2] = alignment;
            n_gbZerofills++;
        } else if (getSectionKind_MachO(macho) == SECTIONKIND_CODE_OR_RODATA) {
            size_rxSegment = ll_round_up_to_align(size_rxSegment, alignment);
            size_rxSegment += macho->size;
            if (alignment > max_alignment[0]) max_alignment[0] = alignment;
            n_rxSections++;
        } else {
            size_rwSegment = ll_round_up_to_align(size_rwSegment, alignment);
            size_rwSegment += macho->size;
            if (alignment > max_alignment[1]) max_alignment[1] = alignment;
            n_rwSections++;
        }
    }

    /* Offsets were sized from zero, but mmap guarantees only page alignment.
     * Reserve the worst-case initial phase before any larger alignment. */
    size_t page = ll_page_size_for(oc->L);
    size_t *sizes[3] = {&size_rxSegment, &size_rwSegment, &size_gbZerofillSegment};
    for (size_t i = 0; i < 3; ++i) {
        size_t padding = max_alignment[i] > page ? max_alignment[i] - page : 0;
        if (*sizes[i] > SIZE_MAX - padding) {
            ll_error_set(oc->L, LL_E_FORMAT, "Mach-O segment alignment padding overflow");
            return 0;
        }
        *sizes[i] += padding;
    }

    size_compound = ll_round_up_to_page_for(oc->L, size_rxSegment) +
        ll_round_up_to_page_for(oc->L, size_rwSegment) +
        ll_round_up_to_page_for(oc->L, size_gbZerofillSegment);

    if (n_rxSections > 0) {
        n_activeSegments++;
    }
    if (n_rwSections > 0) {
        n_activeSegments++;
    }
    if (n_gbZerofills > 0) {
        n_activeSegments++;
    }

    // N.B. it's possible that there is nothing mappable in an object. In this
    // case we avoid the mmap call and segment allocation/building since it will
    // fail either here or further down the road, e.g. on size > 0 assert in
    // addProddableBlock. See #16701.
    if (0 == size_compound) {
        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocBuildSegments_MachO: all segments are empty, skipping\n"));
        return 1;
    }

    mem = ll_mmap_anon_for_linker(oc->L, size_compound);
    if (NULL == mem) {
        ll_error_set(oc->L, LL_E_NOMEM, "cannot map Mach-O segments");
        return 0;
    }

    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocBuildSegments: allocating %d segments\n", n_activeSegments));
    segments = (Segment*)LL_CALLOC_FOR(oc->L, n_activeSegments, sizeof(Segment),
                                        "ocBuildSegments_MachO(segments)");
    if (segments == NULL) goto allocation_failed;
    curMem = mem;

    /* Allocate space for RX segment */
    if (n_rxSections > 0) {
        rxSegment = &segments[curSegment];
        initSegment(oc->L, rxSegment,
                    curMem,
                    ll_round_up_to_page_for(oc->L, size_rxSegment),
                    SEGMENT_PROT_RX,
                    n_rxSections);
        if (rxSegment->sections_idx == NULL) goto allocation_failed;
        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocBuildSegments_MachO: init segment %d (RX) at %p size %zu\n",
                                    curSegment, rxSegment->start, rxSegment->size));
        curMem = (char *)curMem + rxSegment->size;
        curSegment++;
    }

    /* Allocate space for RW segment */
    if (n_rwSections > 0) {
        rwSegment = &segments[curSegment];
        initSegment(oc->L, rwSegment,
                    curMem,
                    ll_round_up_to_page_for(oc->L, size_rwSegment),
                    SEGMENT_PROT_RWO,
                    n_rwSections);
        if (rwSegment->sections_idx == NULL) goto allocation_failed;
        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocBuildSegments_MachO: init segment %d (RWO) at %p size %zu\n",
                                    curSegment, rwSegment->start, rwSegment->size));
        curMem = (char *)curMem + rwSegment->size;
        curSegment++;
    }

    /* Allocate space for GB_ZEROFILL segment */
    if (n_gbZerofills > 0) {
        gbZerofillSegment = &segments[curSegment];
        initSegment(oc->L, gbZerofillSegment,
                    curMem,
                    ll_round_up_to_page_for(oc->L, size_gbZerofillSegment),
                    SEGMENT_PROT_RWO,
                    n_gbZerofills);
        if (gbZerofillSegment->sections_idx == NULL) goto allocation_failed;
        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocBuildSegments_MachO: init segment %d (GB_ZEROFILL) at %p size %zu\n",
                                    curSegment, gbZerofillSegment->start, gbZerofillSegment->size));
        curMem = (char *)curMem + gbZerofillSegment->size;
        curSegment++;
    }

    /* Second pass over sections to fill in sections_idx arrays */
    for (int i = 0, rx = 0, rw = 0, gb = 0;
         i < oc->n_sections;
         i++)
    {
        MachOSection *macho = &oc->macho_info->macho_sections[i];
        // Skip zero size sections here as well since there was no place
        // allocated for them in Segment's sections_idx array
        if (0 == macho->size) {
            continue;
        }

        if (LL_MACHO_S_GB_ZEROFILL == (macho->flags & LL_MACHO_SECTION_TYPE)) {
            gbZerofillSegment->sections_idx[gb++] = i;
        } else if (getSectionKind_MachO(macho) == SECTIONKIND_CODE_OR_RODATA) {
            rxSegment->sections_idx[rx++] = i;
        } else {
            rwSegment->sections_idx[rw++] = i;
        }
    }

    oc->segments = segments;
    oc->n_segments = n_activeSegments;

    return 1;

allocation_failed:
    /* No segment owns the compound mapping until all descriptors are ready. */
    if (segments != NULL) {
        for (int i = 0; i < n_activeSegments; ++i)
            LL_FREE_FOR(oc->L, segments[i].sections_idx);
        LL_FREE_FOR(oc->L, segments);
    }
    ll_munmap_for_linker(oc->L, mem, size_compound, "ocBuildSegments_MachO");
    ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O segment metadata");
    return 0;
}

/* Heap-owned sections carry a separate synthetic execution address. */
static int
noexecSections_MachO(ObjectCode *oc)
{
    for (int i = 0; i < oc->n_sections; ++i) {
        MachOSection *header = &oc->macho_info->macho_sections[i];
        if (header->size == 0) continue;
        const size_t alignment = (size_t)1 << header->align;
        const unsigned type = header->flags & LL_MACHO_SECTION_TYPE;
        const bool zerofill = type == LL_MACHO_S_ZEROFILL ||
                              type == LL_MACHO_S_GB_ZEROFILL;
        size_t tail = 0, tail_offset = header->size;
#if LL_MACHO_NEED_PLT
        if (!zerofill) {
            const size_t count = macho_numberOfStubsForSection(oc, i);
            if (count > SIZE_MAX / STUB_SIZE) {
                ll_error_set(oc->L, LL_E_NOMEM, "NO_EXEC Mach-O stub capacity is too large");
                return 0;
            }
            tail = count * STUB_SIZE;
        }
#endif
        ll_noexec_block block;
        if (tail != 0) {
            if (!ll_noexec_allocate_tail(oc->L, header->size, alignment,
                                         tail, 4, &block, &tail_offset)) return 0;
        } else if (!ll_noexec_allocate(oc->L, header->size, alignment, &block)) {
            return 0;
        }
        if (!zerofill) memcpy(block.site.work, oc->image + header->offset, header->size);
        Section *section = &oc->sections[i];
        addSection(oc->L, section, getSectionKind_MachO(header), SECTION_NOEXEC,
                   block.site.work, header->size, 0, block.allocation, 0,
                   sizeof(struct ll_macho_section_info));
        section->exec_start = block.site.exec;
        if (section->macho_info == NULL) {
            ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O section metadata");
            return 0;
        }
        section->macho_info->macho_section = header;
        if (tail != 0) {
            section->macho_info->stub_offset = block.site.work + tail_offset;
            section->macho_info->stub_size = tail;
        }
        if (!zerofill) addProddableBlock(oc->L, &oc->proddables, section->start, section->size);
        if (header->nreloc != 0) {
            const char *table = oc->image + header->reloff;
            if ((uintptr_t)table % _Alignof(MachORelocationInfo) == 0) {
                section->macho_info->relocation_info = (MachORelocationInfo *)table;
            } else {
                size_t bytes = (size_t)header->nreloc * sizeof(MachORelocationInfo);
                MachORelocationInfo *copy = LL_MALLOC_FOR(oc->L, bytes,
                                                "ocGetNames_MachO(relocations)");
                if (copy == NULL) {
                    ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate aligned Mach-O relocations");
                    return 0;
                }
                memcpy(copy, table, bytes);
                section->macho_info->owned_relocations = copy;
                section->macho_info->relocation_info = copy;
            }
        }
    }
    return 1;
}

int
ocGetNames_MachO(ObjectCode* oc)
{
    unsigned curSymbol = 0;

    unsigned long commonSize = 0;
    ll_symbol_addr* commonStorage = NULL;
    unsigned long commonCounter;

    LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: %s start\n",  OC_INFORMATIVE_FILENAME(oc)));

    Section *secArray;
    secArray = (Section*)LL_CALLOC_FOR(oc->L,
        oc->macho_info->segCmd->nsects,
        sizeof(Section),
        "ocGetNames_MachO(sections)");
    if (oc->macho_info->segCmd->nsects != 0 && secArray == NULL) {
        ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O sections");
        return 0;
    }

    oc->sections = secArray;

    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: will load %d sections\n",
                                oc->n_sections));

    if (ll_noexec_enabled(oc->L)) {
        if (!noexecSections_MachO(oc)) return 0;
    } else if (!ocBuildSegments_MachO(oc)) return 0;

    for (int seg_n = 0; seg_n < oc->n_segments; seg_n++) {
        Segment *segment = &oc->segments[seg_n];
        void *curMem = segment->start;

        LL_IF_DEBUG_FOR(oc->L, linker,
                 LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: loading segment %d "
                            "(address = %p, size = %zu) "
                            "with %d sections\n",
                            seg_n, segment->start, segment->size, segment->n_sections));

        for (int sec_n = 0; sec_n < segment->n_sections; sec_n++) {
            int sec_idx = segment->sections_idx[sec_n];
            MachOSection *section = &oc->macho_info->macho_sections[sec_idx];

            size_t alignment = 1 << section->align;
            SectionKind kind = getSectionKind_MachO(section);
            SectionAlloc alloc = SECTION_NOMEM;
            void *start = NULL, *mapped_start = NULL;
            ll_word mapped_size = 0, mapped_offset = 0;
            ll_word size = section->size;

            void *secMem = (void *)ll_round_up_to_align((size_t)curMem, alignment);

            start = secMem;

            LL_IF_DEBUG_FOR(oc->L, linker,
                     LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: loading section %d in segment %d "
                                "(#%d, %.16s %.16s)\n"
                                "                  skipped %zu bytes due to alignment of %zu\n",
                                sec_n, seg_n, sec_idx, section->segname, section->sectname,
                                (char *)secMem - (char *)curMem, alignment));

            switch (section->flags & LL_MACHO_SECTION_TYPE) {
            case LL_MACHO_S_ZEROFILL:
            case LL_MACHO_S_GB_ZEROFILL:
                LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: memset to 0 a ZEROFILL section\n"));
                memset(secMem, 0, section->size);
                addSection(oc->L, &secArray[sec_idx], kind, alloc, start, size,
                            mapped_offset, mapped_start, mapped_size, sizeof(struct ll_macho_section_info));
                if (secArray[sec_idx].macho_info == NULL) {
                    ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O section metadata");
                    return 0;
                }
                break;
            default:
                LL_IF_DEBUG_FOR(oc->L, linker,
                         LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: copying from %p to %p"
                                    " a block of %" PRIu64 " bytes\n",
                                    (void *) (oc->image + section->offset), secMem, section->size));
#if LL_MACHO_NEED_PLT
                unsigned nstubs = macho_numberOfStubsForSection(oc, sec_idx);
                unsigned stub_space = STUB_SIZE * nstubs;

                void * mem = ll_backend_map_anon_write(oc->L, section->size+stub_space);

                if( mem == NULL ) {
                    ll_error_set(oc->L, LL_E_NOMEM,
                                 "cannot map Mach-O section %d", sec_idx);
                    LL_LOG_SYSERROR_FOR(oc->L, "failed to mmap allocated memory to load section %d. "
                                  "errno = %d", sec_idx, errno);
                    return 0;
                }
                /* copy only the image part over; we don't want to copy data
                * into the stub part.
                */
                memcpy( mem, oc->image + section->offset, size );

                alloc = SECTION_MMAP;
                mapped_offset = 0;
                mapped_size = ll_round_up_to_page_for(oc->L, size+stub_space);
                start = mem;
                mapped_start = mem;
#else
                memcpy(secMem, oc->image + section->offset, section->size);
#endif
                addSection(oc->L, &secArray[sec_idx], kind, alloc, start, size,
                            mapped_offset, mapped_start, mapped_size, sizeof(struct ll_macho_section_info));
                if (secArray[sec_idx].macho_info == NULL) {
                    ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O section metadata");
                    return 0;
                }
            /* SECTION_NOMEM since memory is already allocated in segments */

#if LL_MACHO_NEED_PLT
                secArray[sec_idx].macho_info->nstubs = 0;
                secArray[sec_idx].macho_info->stub_offset = (uint8_t*)mem + size;
                secArray[sec_idx].macho_info->stub_size = stub_space;
                secArray[sec_idx].macho_info->stubs = NULL;
#else
                secArray[sec_idx].macho_info->nstubs = 0;
                secArray[sec_idx].macho_info->stub_offset = NULL;
                secArray[sec_idx].macho_info->stub_size = 0;
                secArray[sec_idx].macho_info->stubs = NULL;
#endif
                addProddableBlock(oc->L, &oc->proddables, start, section->size);
            }

            curMem = (char*) secMem + section->size;

            secArray[sec_idx].macho_info->macho_section = section;
            if (section->nreloc != 0) {
                const char *table = oc->image + section->reloff;
                if ((uintptr_t)table % _Alignof(MachORelocationInfo) == 0) {
                    secArray[sec_idx].macho_info->relocation_info
                        = (MachORelocationInfo *)table;
                } else {
                    size_t bytes = (size_t)section->nreloc
                                 * sizeof(MachORelocationInfo);
                    MachORelocationInfo *copy = LL_MALLOC_FOR(oc->L, bytes,
                                                "ocGetNames_MachO(relocations)");
                    if (copy == NULL) {
                        ll_error_set(oc->L, LL_E_NOMEM,
                                     "cannot allocate aligned Mach-O relocations");
                        return 0;
                    }
                    memcpy(copy, table, bytes);
                    secArray[sec_idx].macho_info->owned_relocations = copy;
                    secArray[sec_idx].macho_info->relocation_info = copy;
                }
            }

        }

    }

    /* now, as all sections have been loaded, we can resolve the absolute
     * address of symbols defined in those sections.
     */
    for(size_t i=0; i < oc->macho_info->n_macho_symbols; i++) {
        MachOSymbol * s = &oc->macho_info->macho_symbols[i];
        if( LL_MACHO_N_SECT == (s->nlist->n_type & LL_MACHO_N_TYPE) ) {
            if( LL_MACHO_NO_SECT == s->nlist->n_sect ) {
                ll_error_set(oc->L, LL_E_FORMAT, "Symbol with N_SECT type, but no section.");
                LL_LOG_ERROR_FOR(oc->L, "Symbol with N_SECT type, but no section.");
                return 0;
            }

            /* section is given, and n_sect is >0 */
            uint8_t n = s->nlist->n_sect - 1;
            if(0 == oc->macho_info->macho_sections[n].size) {
                continue;
            }

            /* addr <-   address in memory where the relocated section resides | (a)
             *         - section's address in the image | (b)
             *         + symbol's address in the image  | (c)
             * (c) - (b) gives symbol's offset relative to section start
             * (a) - (b) + (c) gives symbol's address for the relocated section
             *
             * (c) and (b) are not _real_ addresses and not equal
             * to file offsets in the image.
             * Rather they are (virtual) aligned addresses within
             * a single segment of MH_OBJECT object file.
             */
            s->addr = (void *)(oc->sections[n].exec_start
                              - oc->macho_info->macho_sections[n].addr
                              + s->nlist->n_value);
            if(NULL == s->addr) {
                ll_error_set(oc->L, LL_E_FORMAT, "Failed to compute address for symbol %s", s->name);
                LL_LOG_ERROR_FOR(oc->L, "Failed to compute address for symbol %s", s->name);
                return 0;
            }
        }
    }

    // count external symbols defined here
    oc->n_symbols = 0;
    if (oc->macho_info->symCmd) {
        for (size_t i = 0; i < oc->macho_info->n_macho_symbols; i++) {
            if (oc->macho_info->nlist[i].n_type & LL_MACHO_N_STAB) {
                ;
            }
            else if(oc->macho_info->nlist[i].n_type & LL_MACHO_N_EXT)
            {
                if((oc->macho_info->nlist[i].n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_UNDF
                    && (oc->macho_info->nlist[i].n_value != 0))
                {
                    oc->n_symbols++;
                    /* Only allocate space for COMMON symbols not already
                     * defined by a previously-loaded object. */
                    ll_symbol_name *nm_c = oc->macho_info->macho_symbols[i].name;
                    if (!lookupStrHashTable(oc->L->symhash, nm_c)) {
                        commonSize += oc->macho_info->nlist[i].n_value;
                    }
                }
                else if((oc->macho_info->nlist[i].n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_SECT)
                    oc->n_symbols++;
            }
        }
    }
    /* allocate space for the exported symbols
     * in the object code. This is used to track
     * which symbols will have to be removed when
     * this object code is unloaded
     */
    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: %d external symbols\n",
                                oc->n_symbols));
    oc->symbols = LL_CALLOC_FOR(oc->L, oc->n_symbols, sizeof(Symbol_t),
                                   "ocGetNames_MachO(oc->symbols)");
    if (oc->n_symbols != 0 && oc->symbols == NULL) {
        ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O exported symbols");
        return 0;
    }

    if (oc->macho_info->symCmd) {
        for (size_t i = 0; i < oc->macho_info->n_macho_symbols; i++) {
            ll_symbol_name* nm = oc->macho_info->macho_symbols[i].name;
            if (oc->macho_info->nlist[i].n_type & LL_MACHO_N_STAB)
            {
                LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: Skip STAB: %s\n", nm));
            }
            else if ((oc->macho_info->nlist[i].n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_SECT)
            {
                if (oc->macho_info->nlist[i].n_type & LL_MACHO_N_EXT)
                {
                    if (   (oc->macho_info->nlist[i].n_desc & LL_MACHO_N_WEAK_DEF)
                        && lookupDependentSymbol(oc->L, nm, oc, NULL)) {
                        // weak definition, and we already have a definition
                        LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "    weak: %s\n", nm));
                    }
                    else
                    {
                            LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: inserting %s\n", nm));
                            ll_symbol_addr* addr = oc->macho_info->macho_symbols[i].addr;
                            // TODO: Make figure out how to determine this from the object file
                            ll_sym_type sym_type = LL_SYM_CODE;
                            if (!ghciInsertSymbolTable( oc->L
                                                 , oc->fileName
                                                 , nm
                                                 , addr
                                                 , LL_HS_BOOL_FALSE
                                                 , sym_type
                                                 , 0
                                                 , oc)) {
                                return 0;
                            }

                            oc->symbols[curSymbol].name = nm;
                            oc->symbols[curSymbol].addr = addr;
                            oc->symbols[curSymbol].type = sym_type;
                            curSymbol++;
                    }
                }
                else
                {
                    LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: \t...not external, skipping %s\n", nm));
                }
            }
            else
            {
                LL_IF_DEBUG_FOR(oc->L, linker_verbose, LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: \t...not defined in this section, skipping %s\n", nm));
            }
        }
    }

    /* setup the common storage */
    if (commonSize > 0) {
        if (ll_noexec_enabled(oc->L)) {
            ll_noexec_block block;
            if (!ll_noexec_allocate(oc->L, commonSize, 8, &block)) return 0;
            commonStorage = block.site.work;
            oc->macho_info->common_allocation = block.allocation;
            commonCounter = block.site.exec;
        } else {
            commonStorage = LL_CALLOC_FOR(oc->L, 1, commonSize, "ocGetNames_MachO(common symbols)");
            commonCounter = (unsigned long)commonStorage;
        }
        if (commonStorage == NULL) {
            ll_error_set(oc->L, LL_E_NOMEM, "cannot allocate Mach-O COMMON storage");
            return 0;
        }
        oc->macho_info->common_mem = commonStorage;
    }

    if (oc->macho_info->symCmd) {
        for (size_t i = 0; i < oc->macho_info->n_macho_symbols; i++) {
            ll_symbol_name* nm = oc->macho_info->macho_symbols[i].name;
            MachONList *nlist = &oc->macho_info->nlist[i];
            if((nlist->n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_UNDF
             && (nlist->n_type & LL_MACHO_N_EXT)
             && (nlist->n_value != 0)) {
                unsigned long sz = nlist->n_value;
                /* TODO: Figure out how to determine this from object */
                ll_sym_type sym_type = LL_SYM_CODE;

                RtsSymbolInfo *existing = lookupStrHashTable(oc->L->symhash, nm);
                if (existing != NULL) {
                    if (!ll_failure_alias(oc, existing->owner)) return 0;
                    /* COMMON symbol already allocated by a previously-loaded
                     * object; reuse that address so relocations resolve to
                     * the same storage. */
                    if (sz > existing->size) {
                        ll_error_set(oc->L, LL_E_DUPSYM, "linker: trying to link COMMON symbols %s with"
                             " incompatible sizes: previous size %llu,"
                             " new size %lu\n",
                             nm,
                             (long long unsigned int) existing->size,
                             sz);
                        LL_LOG_ERROR_FOR(oc->L, "linker: trying to link COMMON symbols %s with"
                             " incompatible sizes: previous size %llu,"
                             " new size %lu\n",
                             nm,
                             (long long unsigned int) existing->size,
                             sz);
                        return 0;
                    }
                    nlist->n_value = (unsigned long)existing->value;
                    oc->macho_info->macho_symbols[i].addr = existing->value;
                    LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                             LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: COMMON symbol %s"
                                        " reusing address %p\n",
                                        nm, existing->value));
                    /* Don't add to oc->symbols: not the owner */
                } else {
                    nlist->n_value = commonCounter;
                    /* also set the final address to the macho_symbol */
                    oc->macho_info->macho_symbols[i].addr = (void*)commonCounter;

                    LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                             LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: inserting common symbol: %s\n", nm));
                    if (!ghciInsertSymbolTable(oc->L, oc->fileName, nm,
                                         (void*)commonCounter, LL_HS_BOOL_FALSE,
                                         sym_type,
                                         sz, oc)) {
                        return 0;
                    }
                    oc->symbols[curSymbol].name = nm;
                    oc->symbols[curSymbol].addr = oc->macho_info->macho_symbols[i].addr;
                    oc->symbols[curSymbol].type = sym_type;
                    curSymbol++;

                    commonCounter += sz;
                }
            }
        }
    }
#if LL_TARGET_AARCH64
    findInternalGotRefs(oc);

    /* Setup the global offset table
     * This is for symbols that are external, and not defined here.
     * So that we can load their address indirectly.
     *
     * We will get GOT request for any symbol that is
     * - EXT and UNDF
     * - EXT and not in the same section.
     *
     * As sections are not necessarily contiguous and can live
     * anywhere in the addressable space. This obviously makes
     * sense.  However it took me a while to figure this out.
     */
    if (makeGot(oc)) {
        ll_error_set(oc->L, LL_E_NOMEM,
                     "could not allocate the global offset table for %" PATH_FMT,
                     OC_INFORMATIVE_FILENAME(oc));
        return 0;
    }

    /* at this point, macho_symbols, should know the addresses for
     * all symbols defined by this object code.
     * - those that are defined in sections.
     * - those that are undefined, but have a value (common storage).
     */
#endif
    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocGetNames_MachO: done\n"));
    return 1;
}

static bool
ocMprotect_MachO( ObjectCode *oc )
{
    for(int i=0; i < oc->n_segments; i++) {
        Segment *segment = &oc->segments[i];
        if(segment->size == 0) continue;

        if(segment->prot == SEGMENT_PROT_RX) {
            if (!ll_mprotect_for_linker(oc->L, segment->start, segment->size,
                                        MEM_READ_EXECUTE)) {
                ll_failure_protection(oc);
                return false;
            }
        }
    }

    // Also mark mmaped, sections executable. Those are not part of the
    // segments anymore and have been mapped separately.
    for(int i=0; i < oc->n_sections; i++) {
        Section *section = &oc->sections[i];
        if(section->size == 0) continue;
        if(section->alloc != SECTION_MMAP) continue;
        // N.B. m32 handles protection of its allocations during
        // flushing.
        if(section->alloc == SECTION_M32) continue;
        switch (section->kind) {
        case SECTIONKIND_CODE_OR_RODATA: {
            if (!ll_mprotect_for_linker(oc->L, section->mapped_start,
                                        section->mapped_size, MEM_READ_EXECUTE)) {
                ll_failure_protection(oc);
                return false;
            }
            break;
        }
        default:
            break;
        }
    }

    return true;
}

int
ocResolve_MachO(ObjectCode* oc)
{
    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocResolve_MachO: %s start\n", OC_INFORMATIVE_FILENAME(oc)));

    if(NULL != oc->macho_info->dsymCmd)
    {
        uint32_t offset = oc->macho_info->dsymCmd->indirectsymoff;
        uint32_t count = oc->macho_info->dsymCmd->nindirectsyms;
        if (oc->fileSize < 0 || offset > (size_t)oc->fileSize ||
            count > ((size_t)oc->fileSize - offset) / sizeof(uint32_t) ||
            (count != 0 && (uintptr_t)(oc->image + offset) % _Alignof(uint32_t) != 0)) {
            ll_error_set(oc->L, LL_E_FORMAT, "Mach-O indirect symbol table is invalid");
            LL_LOG_ERROR_FOR(oc->L, "Mach-O indirect symbol table is invalid");
            return 0;
        }
        const uint32_t *indirectSyms
            = (const uint32_t*) (oc->image + offset);

        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocResolve_MachO: resolving dsymLC\n"));
        for (int i = 0; i < oc->n_sections; i++)
        {
            const char * sectionName = oc->macho_info->macho_sections[i].sectname;

            LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocResolve_MachO: section %d/%d: %.16s\n", i, oc->n_sections, sectionName));

            if(    !strncmp(sectionName,"__la_symbol_ptr",16)
                || !strncmp(sectionName,"__la_sym_ptr2",16)
                || !strncmp(sectionName,"__la_sym_ptr3",16))
            {
                if(!resolveImports(oc,&oc->macho_info->macho_sections[i],
                                   &oc->sections[i], indirectSyms,
                                   oc->macho_info->dsymCmd->nindirectsyms))
                    return 0;
            }
            else if(!strncmp(sectionName,"__nl_symbol_ptr",16)
                ||  !strncmp(sectionName,"__pointers",16))
            {
                if(!resolveImports(oc,&oc->macho_info->macho_sections[i],
                                   &oc->sections[i], indirectSyms,
                                   oc->macho_info->dsymCmd->nindirectsyms))
                    return 0;
            }
            else if(!strncmp(sectionName,"__jump_table",16))
            {
                /* This legacy alias has no established slot format. Standard
                 * S_SYMBOL_STUBS uses reserved2 byte strides, not pointers. */
                ll_error_set(oc->L, LL_E_UNSUPPORTED,
                             "Mach-O __jump_table format is unsupported");
                LL_LOG_ERROR_FOR(oc->L,
                             "Mach-O __jump_table format is unsupported");
                return 0;
            }
            else
            {
                LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocResolve_MachO: unknown section %d/%d\n", i, oc->n_sections));
            }
        }
    }
#if LL_TARGET_AARCH64
    /* fill the GOT table */
    for(size_t i = 0; i < oc->macho_info->n_macho_symbols; i++) {
        MachOSymbol * symbol = &oc->macho_info->macho_symbols[i];
        if(needGotSlot(symbol)) {
            if(LL_MACHO_N_UNDF == (symbol->nlist->n_type & LL_MACHO_N_TYPE)) {
                /* an undefined symbol. So we need to ensure we
                 * have the address.
                 */
                if(NULL == symbol->addr) {
                    symbol->addr = lookupDependentSymbol(oc->L, (char*)symbol->name, oc, NULL);
                    if(NULL == symbol->addr) {
                        LL_LOG_ERROR_FOR(oc->L, "Failed to lookup symbol: %s", symbol->name);
                        return 0;
                    }
                } else {
                    // we already have the address.
                }
            } /* else it was defined in the same object,
               * just a different section. We should have
               * the address as well already
               */
            if(NULL == symbol->addr) {
                LL_LOG_ERROR_FOR(oc->L, "Symbol %s has no address!\n", (char*)symbol->name);
                return 0;
            }
            if(NULL == symbol->got_addr) {
                LL_LOG_ERROR_FOR(oc->L, "Symbol %s has no Global Offset Table address!\n", (char*)symbol->name);
                return 0;
            }
            *(uint64_t*)symbol->got_addr = (uint64_t)symbol->addr;
        }
    }
#endif

    for(int i = 0; i < oc->n_sections; i++)
    {
        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocResolve_MachO: relocating section %d/%d\n", i, oc->n_sections));

#if LL_TARGET_AARCH64
        if (!relocateSectionAarch64(oc, &oc->sections[i]))
            return 0;
#else
        if (!relocateSection(oc, i))
            return 0;
#endif
    }
    if(!ll_noexec_enabled(oc->L) && !ocMprotect_MachO ( oc ))
        return 0;

    return 1;
}

int
ocRunInit_MachO ( ObjectCode *oc )
{
    if (NULL == oc->macho_info->segCmd) {
        LL_FATAL_FOR(oc->L, "ocRunInit_MachO: no segment load command");
    }

    int argc = 0, envc = 0;
    char **argv = NULL, **envv = NULL;

    if (oc->L->services.host.prog_args != NULL) {
        oc->L->services.host.prog_args(oc->L->services.host.ctx, &argc, &argv, &envc, &envv);
    }

    for (int i = 0; i < oc->n_sections; i++) {
        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocRunInit_MachO: checking section %d\n", i));

        if (oc->sections[i].kind == SECTIONKIND_INIT_ARRAY) {
            LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocRunInit_MachO:     running mod init functions\n"));

            void *init_startC = oc->sections[i].start;
            init_t *init = (init_t*)init_startC;
            init_t *init_end = (init_t*)((uint8_t*)init_startC
                             + oc->sections[i].macho_info->macho_section->size);

            for (int pn = 0; init < init_end; init++, pn++) {
                LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocRunInit_MachO:     function pointer %d at %p to %p\n",
                                            pn, (void *) init, (void *) *init));
                (*init)(argc, argv, envv);
            }
        }
    }

    if (oc->L->services.host.free_prog_env != NULL) {
        oc->L->services.host.free_prog_env(oc->L->services.host.ctx, envc, envv);
    }
    return 1;
}

int
ocRunFini_MachO ( ObjectCode *oc )
{
    if (NULL == oc->macho_info->segCmd) {
        LL_FATAL_FOR(oc->L, "ocRunInit_MachO: no segment load command");
    }

    for (int i = 0; i < oc->n_sections; i++) {
        LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocRunFini_MachO: checking section %d\n", i));

        if (oc->sections[i].kind == SECTIONKIND_FINI_ARRAY) {
            LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocRunFini_MachO:     running mod fini functions\n"));

            void *fini_startC = oc->sections[i].start;
            fini_t *fini = (fini_t*)fini_startC;
            fini_t *fini_end = (fini_t*)((uint8_t*)fini_startC
                             + oc->sections[i].macho_info->macho_section->size);

            for (int pn = 0; fini < fini_end; fini++, pn++) {
                LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "ocRunFini_MachO:     function pointer %d at %p to %p\n",
                                            pn, (void *) fini, (void *) *fini));
                (*fini)();
            }
        }
    }

    return 1;
}

/*
 * Figure out by how much to shift the entire Mach-O file in memory
 * when loading so that its single segment ends up 16-byte-aligned.
 * Return -1 with an owner-local error if the header cannot be read or rewound.
 */
int
machoGetMisalignment( ll_linker *L, FILE * f )
{
    MachOHeader header;
    int misalignment;

    {
        size_t n = fread(&header, sizeof(header), 1, f);
        if (n != 1) {
            ll_error_set(L, ferror(f) ? LL_E_IO : LL_E_FORMAT,
                         "machoGetMisalignment: can't read the Mach-O header");
            LL_LOG_ERROR_FOR(L, "machoGetMisalignment: can't read the Mach-O header");
            return -1;
        }
    }
    if (fseek(f, -(long)sizeof(header), SEEK_CUR) != 0) {
        ll_error_set(L, LL_E_IO, "machoGetMisalignment: can't rewind the Mach-O header");
        LL_LOG_ERROR_FOR(L, "machoGetMisalignment: can't rewind the Mach-O header");
        return -1;
    }

    if(header.magic != LL_MACHO_MH_MAGIC_64) {
        ll_error_set(L, LL_E_FORMAT, "Bad magic. Expected: %08x, got: %08x.",
             LL_MACHO_MH_MAGIC_64, header.magic);
        LL_LOG_ERROR_FOR(L, "Bad magic. Expected: %08x, got: %08x.",
             LL_MACHO_MH_MAGIC_64, header.magic);
        return -1;
    }

    misalignment = (header.sizeofcmds + sizeof(header))
                    & 0xF;

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "mach-o misalignment %d\n", misalignment));
    return misalignment ? (16 - misalignment) : 0;
}

static int ll_macho_init(ObjectCode *oc)
{
    if (!ocVerifyImage_MachO(oc)) return 0;
    return ocInit_MachO(oc);
}
static int ll_macho_extras(ObjectCode *oc)
{
#if LL_MACHO_NEED_SYMBOL_EXTRAS
    return ocAllocateExtras_MachO(oc);
#else
    (void)oc;
    return 1;
#endif
}
const struct ll_format_ops ll_format_macho = {
    .name = "Mach-O", .sniff = ll_format_sniff_macho,
    .verify = ocVerifyImage_MachO, .init = ll_macho_init,
    .alloc_extras = ll_macho_extras, .get_names = ocGetNames_MachO,
    .resolve = ocResolve_MachO, .run_init = ocRunInit_MachO,
    .run_fini = ocRunFini_MachO, .deinit = ocDeinit_MachO
};

#endif /* darwin_HOST_OS || ios_HOST_OS */
