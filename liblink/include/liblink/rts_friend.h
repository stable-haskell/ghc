/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Temporary interface shared by the RTS and the in-memory linker.
 * Keep object layouts shared while unload checking remains in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ll_compat.h"
#include "ll_hash.h"
#include "ll_mutex.h"
#include "linker/M32Alloc.h"
#include "linker/ProddableBlocks.h"
#include "ll_visibility.h"
#include "liblink/symbol_extras.h"

#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif

/* Which object file format are we targeting? */
#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) \
|| defined(linux_android_HOST_OS) \
|| defined(freebsd_HOST_OS) || defined(kfreebsdgnu_HOST_OS) \
|| defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) \
|| defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)
#  define OBJFORMAT_ELF
#elif defined(mingw32_HOST_OS)
#  define OBJFORMAT_PEi386
#elif defined(darwin_HOST_OS) || defined(ios_HOST_OS)
#  define OBJFORMAT_MACHO
#elif defined(wasm32_HOST_ARCH)
#  define OBJFORMAT_WASM32
#endif

struct ll_format_ops;
typedef struct _ObjectCode ObjectCode;
typedef struct _Section Section;

/* Each format owns its metadata. The native names remain temporary aliases
 * for RTS clients and fixtures while this shared interface is retired. */
struct ll_elf_object_info;
struct ll_elf_section_info;
struct ll_macho_object_info;
struct ll_macho_section_info;
struct ll_coff_object_info;
struct ll_coff_section_info;

#if defined(OBJFORMAT_ELF)
#define ObjectCodeFormatInfo ll_elf_object_info
#define SectionFormatInfo ll_elf_section_info
#elif defined(OBJFORMAT_MACHO)
#define ObjectCodeFormatInfo ll_macho_object_info
#define SectionFormatInfo ll_macho_section_info
#elif defined(OBJFORMAT_PEi386)
#define ObjectCodeFormatInfo ll_coff_object_info
#define SectionFormatInfo ll_coff_section_info
#endif

#if defined(OBJFORMAT_ELF)
#include <elf.h>
/* Match the word type selected by the private ELF backend. */
#if defined(__LP64__) || defined(_LP64)
typedef Elf64_Word ll_elf_word;
#else
typedef Elf32_Word ll_elf_word;
#endif
#endif

/* Hold extended information about a symbol in case we need to resolve it at a
   late stage.  */
typedef struct _Symbol
{
    ll_symbol_name *name;
    ll_symbol_addr *addr;
    ll_sym_type type;
} Symbol_t;

typedef struct NativeCodeRange_ {
  void *start, *end;

  /* Allow a chain of these things */
  struct NativeCodeRange_ *next;
} NativeCodeRange;

/* Indication of section kinds for loaded objects.  Needed by
   the GC for deciding whether or not a pointer on the stack
   is a code pointer.
   See Note [BFD import library].
*/
typedef
   enum { /* Section is code or readonly. e.g. .text or .r(o)data.  */
          SECTIONKIND_CODE_OR_RODATA,
          /* Section contains read/write data. e.g. .data.  */
          SECTIONKIND_RWDATA,
          /* Static initializer section. e.g. .ctors.  */
          SECTIONKIND_INIT_ARRAY,
          /* Static finalizer section. e.g. .dtors.  */
          SECTIONKIND_FINI_ARRAY,
          /* We don't know what the section is and don't care.  */
          SECTIONKIND_OTHER,

          /*
           * Windows-specific section kinds
           */

          /* Section contains debug information. e.g. .debug$.  */
          SECTIONKIND_DEBUG,
          /* Section contains exception table. e.g. .pdata.  */
          SECTIONKIND_EXCEPTION_TABLE,
          /* Section contains unwind info. e.g. .xdata.  */
          SECTIONKIND_EXCEPTION_UNWIND,
          /* Section belongs to an import section group. e.g. .idata$.  */
          SECTIONKIND_IMPORT,
          /* Section defines the head section of a BFD-style import library, e.g. idata$7.  */
          SECTIONKIND_BFD_IMPORT_LIBRARY_HEAD,
          /* Section defines an import library entry, e.g. idata$7.  */
          SECTIONKIND_BFD_IMPORT_LIBRARY,
        }
   SectionKind;

typedef
   enum { SECTION_NOMEM,
          SECTION_M32,
          SECTION_MMAP,
          SECTION_MALLOC
        }
   SectionAlloc;

/* Indicates a desired memory protection for pages within a segment. Defined as
 * enum since it's more explicit and look nicer in a debugger.
 *
 * Can be used directly as a substitution for a combination of PROT_X flags on
 * POSIX systems.
 */
typedef enum {
#if RTS_LINKER_USE_MMAP
    SEGMENT_PROT_RO  = PROT_READ,
    SEGMENT_PROT_RX  = PROT_READ | PROT_EXEC,
    SEGMENT_PROT_RWO = PROT_READ | PROT_WRITE,
#else
    SEGMENT_PROT_RO,
    SEGMENT_PROT_RX,
    SEGMENT_PROT_RWO,
#endif
} SegmentProt;

/*
 * Note [No typedefs for customizable types]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Some pointer-to-struct types are defined opaquely
 * first, and customized later to architecture/ABI-specific
 * instantiations. Having the usual
 *   typedef struct _Foo {...} Foo;
 * wrappers is hard to get right with older versions of GCC,
 * so just have a
 *   struct Foo {...};
 * and always refer to it with the 'struct' qualifier.
 */

struct _Section {
  void*    start;              /* actual start of section in memory */
  ll_word  size;               /* actual size of section in memory */
  SectionKind kind;
  SectionAlloc alloc;

  /*
   * The following fields are relevant for SECTION_MMAP sections only
   */
  ll_word mapped_offset;      /* offset from the image of mapped_start */
  void* mapped_start;         /* start of mmap() block */
  ll_word mapped_size;        /* size of mmap() block */

  /* These views alias one allocation, owned by the selected format. All
   * members are pointers to structs, with identical representation/alignment. */
  union {
    struct SectionFormatInfo *info;
    struct ll_elf_section_info *elf_info;
    struct ll_macho_section_info *macho_info;
    struct ll_coff_section_info *coff_info;
  };
};

_Static_assert(sizeof(Section) - offsetof(Section, info) ==
               sizeof(struct SectionFormatInfo *),
               "section metadata views must occupy one native pointer");
_Static_assert(_Alignof(Section) == _Alignof(struct SectionFormatInfo *),
               "section metadata views must preserve native alignment");

typedef struct _Segment {
    void *start;                /* page aligned start address of a segment */
    size_t size;                /* page rounded size of a segment */
    SegmentProt prot;           /* mem protection to set after all symbols were
                                 * resolved */

    int *sections_idx;          /* an array of section indexes assigned to this segment */
    int n_sections;
} Segment;

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH) || defined(riscv64_HOST_ARCH)
#define NEED_SYMBOL_EXTRAS 1
#endif

/*
 * We use the m32 allocator for symbol extras on Windows and other mmap-using
 * platforms.
 */
#if RTS_LINKER_USE_MMAP || defined(mingw32_HOST_ARCH)
#define NEED_M32 1
#endif

/* Jump Islands are sniplets of machine code required for relative address
 * relocations on the PowerPC, x86_64 and ARM. On RISCV64 we use symbolextras
 * like a GOT for locals where SymbolExtra represents one entry.
 */
#if defined(powerpc_HOST_ARCH)
_Static_assert(sizeof(short) == 2 && sizeof(long) == 4,
               "native PowerPC symbol extras require the existing 32-bit ABI");
typedef ll_symbol_extra_powerpc SymbolExtra;
#elif defined(x86_64_HOST_ARCH)
typedef ll_symbol_extra_x86_64 SymbolExtra;
#elif defined(arm_HOST_ARCH)
typedef ll_symbol_extra_arm SymbolExtra;
#elif defined(riscv64_HOST_ARCH)
typedef ll_symbol_extra_riscv64 SymbolExtra;
#else
/* Preserve the legacy zero-sized native representation until storage/count
 * handling is separated. AArch64 uses section stubs rather than this payload. */
typedef struct { } SymbolExtra;
#endif

typedef enum {
    /* Objects that were loaded by this linker */
    STATIC_OBJECT,

    /* Objects that were loaded by dlopen */
    DYNAMIC_OBJECT,
} ObjectType;

typedef void (*cxa_finalize_fn)(void *);

/* Top-level structure for an object module.  One of these is allocated
 * for each object file in use.
 */
struct _ObjectCode {
    /* Linker that owns this object's allocations, caches, and services. */
    ll_linker *L;

    ll_compat_ostatus    status;
    ll_compat_pathchar  *fileName;
    int        fileSize;     /* also mapped image size when using mmap() */
    const char *formatName;   /* e.g. "ELF32", "DLL", "COFF", etc. */
    ObjectType type;         /* who loaded this object? */

    /* If this object is a member of an archive, archiveMemberName is
     * like "libarchive.a(object.o)". Otherwise it's NULL.
     */
    ll_compat_pathchar*      archiveMemberName;

    /* An array containing ptrs to all the symbol names copied from
       this object into the global symbol hash table.  This is so that
       we know which parts of the latter mapping to nuke when this
       object is removed from the system. */
    Symbol_t *symbols;
    int    n_symbols;

    /* ptr to mem containing the object file image */
    char*      image;

    /* One format-owned allocation, with typed views for each backend. */
    union {
        struct ObjectCodeFormatInfo *info;
        struct ll_elf_object_info *elf_info;
        struct ll_macho_object_info *macho_info;
        struct ll_coff_object_info *coff_info;
    };

    /* non-zero if the object file was mmap'd, otherwise malloc'd */
    int        imageMapped;

    /* record by how much image has been deliberately misaligned
       after allocation, so that we can use realloc */
    int        misalignment;

    /* Set to true after initializers (.init_array, .ctors, etc.) have been
     * executed. Used by freeObjectCode to decide whether finalizers should
     * run: only objects whose initializers ran should have their finalizers
     * executed. See Note [Object unloading and finalizers]. */
    bool initializersRan;

    /* The address of __cxa_finalize; set when at least one finalizer was
     * register and therefore we must call __cxa_finalize before unloading.
     * See Note [Resolving __dso_handle]. */
    cxa_finalize_fn cxa_finalize;

    /* The section-kind entries for this object module. An array. */
    int n_sections;
    Section* sections;

    int n_segments;
    Segment *segments;

    //
    // Garbage collection fields
    //

    // Next object in `objects` list
    struct _ObjectCode *next;

    // Previous object in `objects` list
    struct _ObjectCode *prev;

    // Next object in `loaded_objects` list
    struct _ObjectCode *next_loaded_object;

    // Mark bit
    // N.B. This is a full word as we CAS it.
    ll_word mark;

    // Can this object be safely unloaded? Not true for
    // dynamic objects when dlinfo is not available as
    // we cannot determine liveness.
    bool unloadable;

    // Set of dependencies (ObjectCode*) of the object file. Traverse
    // dependencies using `iterHashTable`.
    //
    // New entries are added as we resolve symbols in an object file, in
    // `lookupDependentSymbol`. When an object file uses multiple symbols from
    // another object file we add the dependent multiple times, so we use a
    // `HashTable` here rather than a list/array to avoid copies.
    //
    // Used when unloading object files. See Note [Object unloading] in
    // CheckUnload.c.
    HashSet *dependencies;

    //
    // End of garbage collection fields
    //

    /* SANITY CHECK ONLY: a list of the only memory regions which may
       safely be prodded during relocation.  Any attempt to prod
       outside one of these is an error in the linker. */
    ProddableBlockSet proddables;

#if defined(NEED_SYMBOL_EXTRAS)
    SymbolExtra    *symbol_extras;
    unsigned long   first_symbol_extra;
    unsigned long   n_symbol_extras;
#endif
    /* Additional memory that is preallocated and contiguous with image
       which can be used to relocate bss sections. */
    char* bssBegin;
    char* bssEnd;

    /* a list of all ForeignExportsLists owned by this object */
    struct ForeignExportsList *foreign_exports;

    /* Holds the list of symbols in the .o file which
       require extra information.*/
    StrHashTable *extraInfos;

#if defined(NEED_M32)
    /* The m32 allocators used for allocating small sections and symbol extras
     * during loading. We have two: one for (writeable) data and one for
     * (read-only/executable) code. */
    m32_allocator *rw_m32, *rx_m32;
#endif

#if defined(OBJFORMAT_ELF) && defined(SHN_XINDEX)
    /* Cached address of ELF's shndx table, or SHNDX_TABLE_UNINIT if not
     * initialized yet. It would be better to put it info ELF-specific
     * ObjectCodeFormatInfo, but unfortunately shndx table is needed in
     * ocVerifyImage_ELF which runs before ObjectCodeFormatInfo is
     * initialized by ocInit_ELF. */
    ll_elf_word *shndx_table;
#endif

    /*
     * The following are only valid if .type == DYNAMIC_OBJECT
     */

    /* handle returned from dlopen */
    void *dlopen_handle;

    /* virtual memory ranges of loaded code. NULL if no range information is
     * available (e.g. if dlinfo is unavailable on the current platform).
     */
    NativeCodeRange *nc_ranges;

    /* Terminal resolution, initialization, or protection requires unload/reload.
     * Keep the marker through GC disposal; a fresh object starts unmarked. */
    bool resolution_failed;

    /* Selected before format initialization; retained even if get_names
     * releases the original image (PE import objects). Native DLLs use NULL. */
    const struct ll_format_ops *format_ops;
};

_Static_assert(offsetof(ObjectCode, imageMapped) ==
               offsetof(ObjectCode, info) + sizeof(struct ObjectCodeFormatInfo *),
               "object metadata views must occupy one native pointer");

/* Borrowed RTS view into the default linker's state. The RTS binds this once
 * before adjustor initialization and accesses the fields directly during GC.
 * The owner mutex is referenced, never copied. */
typedef struct {
    /* Live objects form a doubly linked list through next/prev. Before an
     * unload check the RTS moves it to old_objects, then moves marked objects
     * back here and frees the remaining unloadable objects. */
    ObjectCode *objects;
    /* Objects not yet requested for unloading are GC roots. loadObj_ adds to
     * this list through next_loaded_object; unloadObj_ removes from it. */
    ObjectCode *loaded_objects;
    /* Mark bit for live objects, complemented before each unload check. */
    uint8_t object_code_mark_bit;
    /* Pending unload count: incremented by unloadObj_, decremented when the
     * RTS frees an object. */
    int n_unloaded_objects;
    ll_mutex *mutex;
} ll_gc_state;

LL_FRIEND_API ll_gc_state *ll_compat_default_gc_state(void);

LL_FRIEND_API void exitLinker(void);
LL_FRIEND_API void freeObjectCode(ObjectCode *oc);
LL_FRIEND_API ll_compat_pathchar *resolveSymbolAddr(ll_compat_pathchar *buffer, int size,
                                        ll_symbol_addr *symbol, uintptr_t *top);

LL_FRIEND_API void initLinkerMMap(void);

/** Access modes for mprotectForLinker */
typedef enum {
    MEM_NO_ACCESS,
    MEM_READ_ONLY,
    MEM_READ_WRITE,
    // Initially map pages as rw- and then switch to r-x later.
    MEM_READ_WRITE_THEN_READ_EXECUTE,
    MEM_READ_EXECUTE,
    MEM_READ_WRITE_EXECUTE,
} MemoryAccess;

// Map read/write anonymous memory anywhere in memory.
LL_FRIEND_API void *mmapAnon(size_t bytes);

// Change protection of previous mapping memory.
LL_FRIEND_API void mprotectForLinker(void *start, size_t len, MemoryAccess mode);

// Release a mapping.
LL_FRIEND_API void munmapForLinker (void *addr, size_t bytes, const char *caller);

