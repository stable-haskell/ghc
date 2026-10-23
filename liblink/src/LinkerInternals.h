/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "liblink/rts_friend.h"
#include "ll_linker.h"

void printLoadedObjects(ll_linker *L);

/*
 * Note [Processing overflowed relocations]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * When processing relocations whose targets exceed the relocation's maximum
 * displacement, we can take advantage of knowledge of the symbol type to avoid
 * linker failures. In particular, if we know that a symbol is a code symbol
 * then we can handle the relocation by creating a "jump island", a small bit
 * of code which immediately jumps (with an instruction sequence capable of
 * larger displacement) to the target.
 *
 * This is not possible for data symbols (or, for that matter, Haskell symbols
 * when TNTC is in use). In these cases we have to rather fail and ask the user
 * to recompile their program as position-independent.
 */

#if defined(OBJFORMAT_ELF)
#  include "linker/ElfTypes.h"
#elif defined(OBJFORMAT_PEi386)
#  include "linker/PEi386Types.h"
#elif defined(OBJFORMAT_MACHO)
#  include "linker/MachOTypes.h"
#elif defined(OBJFORMAT_WASM32)
#  include "linker/Wasm32Types.h"
#else
#  error "Unknown OBJECT_FORMAT for HOST_OS"
#endif



#if defined(OBJFORMAT_ELF) && defined(SHN_XINDEX)
/* We cannot simply use NULL to signal uninitialised shndx_table because NULL
 * is valid return value of get_shndx_table. Thus SHNDX_TABLE_UNINIT is defined
 * as the address of the linker's shndx_table_uninit_label, which is definitely
 * unequal to any heap-allocated address. */
#define SHNDX_TABLE_UNINIT(L) (&(L)->shndx_table_uninit_label)
#endif

#define OC_INFORMATIVE_FILENAME(OC)             \
    ( (OC)->archiveMemberName ?                 \
      (OC)->archiveMemberName :                 \
      (OC)->fileName                            \
    )

#define ocDebugBelch(oc, s, ...) \
    LL_DEBUG_FOR((oc)->L, "%s(%" PATH_FMT ": " s, \
               __func__, \
               OC_INFORMATIVE_FILENAME(oc), \
               ##__VA_ARGS__)


/* Type of an initializer */
typedef void (*init_t) (int argc, char **argv, char **env);

/* Type of a finalizer */
typedef void (*fini_t) (void);

/* SymbolInfo tracks a symbol's address, the object code from which
   it originated, and whether or not it's weak.

   RtsSymbolInfo is used to track the state of the symbols currently
   loaded or to be loaded by the Linker.

   Where the information in the `ObjectCode` is used to track the
   original status of the symbol inside the `ObjectCode`.

   A weak symbol that has been used will still be marked as weak
   in the `ObjectCode` but in the `RtsSymbolInfo` it won't be.
*/
typedef struct _RtsSymbolInfo {
    ll_symbol_addr* value;
    ObjectCode *owner;
    ll_strength strength;
    ll_sym_type type;
    unsigned long size; // symbol size: only used for COMMON symbols
} RtsSymbolInfo;

#include "ll_begin_private.h"

ll_symbol_addr* loadSymbol(ll_linker *L, ll_symbol_name *lbl, RtsSymbolInfo *pinfo);

void addSection (ll_linker *L, Section *s, SectionKind kind, SectionAlloc alloc,
                 void* start, ll_word size, ll_word mapped_offset,
                 void* mapped_start, ll_word mapped_size);

ll_hs_bool ghciLookupSymbolInfo(ll_linker *L,
                            const ll_symbol_name* key, RtsSymbolInfo **result);

int ghciInsertSymbolTable(
    ll_linker *L,
    pathchar* obj_name,
    const ll_symbol_name* key,
    ll_symbol_addr* data,
    ll_strength weak,
    ll_sym_type type,
    unsigned long size,
    ObjectCode *owner);

/* Lock-free version of lookupSymbol. When 'dependent' is not NULL, adds it as a
 * dependent to the owner of the symbol. The type of the symbol is stored in 'type'. */
ll_symbol_addr* lookupDependentSymbol (ll_linker *L, ll_symbol_name* lbl,
                                       ObjectCode *dependent, ll_sym_type *type);

/* Perform TLSGD symbol lookup returning the address of the resulting GOT entry,
 * which in this case holds the module id and the symbol offset. */
ll_int64 lookupTlsgdSymbol(const char *, unsigned long, ObjectCode *);

/* defined in LoadArchive.c */
bool isArchive (pathchar *path);
ll_hs_int loadArchive_ (ll_linker *L, pathchar *path);

/*************************************************
 * Various bits of configuration
 *************************************************/

/* PowerPC and ARM have relative branch instructions with only 24 bit
 * displacements and therefore need jump islands contiguous with each object
 * code module.
 */
#if defined(powerpc_HOST_ARCH)
#define SHORT_REL_BRANCH 1
#endif
#if defined(arm_HOST_ARCH)
#define SHORT_REL_BRANCH 1
#endif

#if (RTS_LINKER_USE_MMAP && defined(SHORT_REL_BRANCH) && defined(linux_HOST_OS))
#define USE_CONTIGUOUS_MMAP 1
#else
#define USE_CONTIGUOUS_MMAP 0
#endif

/* Initialization and exit require exclusive access. Lookup and unload callers
 * hold the owner lock. */
void ll_init_linker(ll_linker *L, int retain_cafs);
void ll_exit_linker(ll_linker *L);
ll_symbol_addr *lookupSymbol_(ll_linker *L, ll_symbol_name *lbl);
ll_hs_int unloadNativeObj_(ll_linker *L, void *handle);

ll_hs_int isAlreadyLoaded( ll_linker *L, pathchar *path );
OStatus getObjectLoadStatus_ (ll_linker *L, pathchar *path);
ObjectCode *lookupObjectByPath(ll_linker *L, pathchar *path);
ll_hs_int loadOc( ObjectCode* oc );
/* Before registration: validate before format initialization, and unwind all
 * state owned by an object whose load did not complete. Caller holds L's lock. */
int ll_init_object(ObjectCode *oc);
void ll_discard_object(ObjectCode *oc);
ObjectCode* mkOc( ll_linker *L, ObjectType type, pathchar *path, char *image, int imageSize,
                  bool mapped, pathchar *archiveMemberName,
                  int misalignment
                  );

void initSegment(ll_linker *L, Segment *s, void *start, size_t size,
                 SegmentProt prot, int n_sections);
void freeSegments(ObjectCode *oc);

#include "ll_end_private.h"
