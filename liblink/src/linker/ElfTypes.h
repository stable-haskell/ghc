#if !defined(ElfTypes_h)
#define ElfTypes_h

#include "ll_target.h"
#include "liblink/elf.h"
#include "linker/ElfHost.h"

struct InitFiniList;

/* Selected file records follow the target, not the compiler word size. */
#if LL_TARGET_WORD_SIZE == 8
#define LL_ELF_CLASS LL_ELF_ELFCLASS64
typedef ll_elf64_addr ll_elf_target_addr;
typedef ll_elf64_word ll_elf_target_word;
typedef ll_elf64_sword ll_elf_target_sword;
typedef ll_elf64_half ll_elf_target_half;
typedef ll_elf64_ehdr ll_elf_target_ehdr;
typedef ll_elf64_phdr ll_elf_target_phdr;
typedef ll_elf64_shdr ll_elf_target_shdr;
typedef ll_elf64_sym ll_elf_target_sym;
typedef ll_elf64_rel ll_elf_target_rel;
typedef ll_elf64_rela ll_elf_target_rela;
#define LL_ELF_ST_VISIBILITY LL_ELF64_ST_VISIBILITY
#define LL_ELF_ST_TYPE LL_ELF64_ST_TYPE
#define LL_ELF_ST_BIND LL_ELF64_ST_BIND
#define LL_ELF_R_TYPE LL_ELF64_R_TYPE
#define LL_ELF_R_SYM LL_ELF64_R_SYM
#else
#define LL_ELF_CLASS LL_ELF_ELFCLASS32
typedef ll_elf32_addr ll_elf_target_addr;
typedef ll_elf32_word ll_elf_target_word;
typedef ll_elf32_sword ll_elf_target_sword;
typedef ll_elf32_half ll_elf_target_half;
typedef ll_elf32_ehdr ll_elf_target_ehdr;
typedef ll_elf32_phdr ll_elf_target_phdr;
typedef ll_elf32_shdr ll_elf_target_shdr;
typedef ll_elf32_sym ll_elf_target_sym;
typedef ll_elf32_rel ll_elf_target_rel;
typedef ll_elf32_rela ll_elf_target_rela;
#define LL_ELF_ST_VISIBILITY LL_ELF32_ST_VISIBILITY
#define LL_ELF_ST_TYPE LL_ELF32_ST_TYPE
#define LL_ELF_ST_BIND LL_ELF32_ST_BIND
#define LL_ELF_R_TYPE LL_ELF32_R_TYPE
#define LL_ELF_R_SYM LL_ELF32_R_SYM
#endif

typedef struct _ElfSymbol {
    ll_symbol_name * name;  /* the name of the symbol. */
    ll_symbol_addr * addr;  /* the final resting place of the symbol */
    void * got_addr;    /* address of the got slot for this symbol, if any */
    ll_elf_target_sym * elf_sym;  /* the elf symbol entry */
} ElfSymbol;

typedef struct _ElfSymbolTable {
    unsigned  index;               /* the index of the underlying symtab */
    ElfSymbol * symbols;
    size_t n_symbols;
    char * names;                  /* strings table for this symbol table */
    struct _ElfSymbolTable * next; /* there may be multiple symbol tables */
} ElfSymbolTable;

typedef struct _ElfRelocationTable {
    unsigned index;
    unsigned targetSectionIndex;
    ll_elf_target_shdr *sectionHeader;
    ll_elf_target_rel  *relocations;
    size_t n_relocations;
    struct _ElfRelocationTable *next;
} ElfRelocationTable;

typedef struct _ElfRelocationATable {
    unsigned index;
    unsigned targetSectionIndex;
    ll_elf_target_shdr *sectionHeader;
    ll_elf_target_rela  *relocations;
    size_t n_relocations;
    struct _ElfRelocationATable *next;
} ElfRelocationATable;

/*
 * Just a quick ELF recap:
 *
 * .-----------------.
 * | ELF Header      |
 * |-----------------|
 * | Program Header  |
 * |-----------------|   .
 * | Section 1       |   |
 * |-----------------|   | Segment 1
 * | Section 2       |   |
 * |-----------------|   :
 * | ...             |   |
 * |-----------------|   | Segment n
 * | Section n       |   '
 * |-----------------|
 * | Section Header  |
 * '-----------------'
 *
 *
 * The Program Header will inform us about the Segments.  Whereas the Section
 * Header provides Information about the sections.
 *
 */
struct ll_elf_object_info {
    ll_elf_target_ehdr             *elfHeader;
    ll_elf_target_phdr             *programHeader;
    ll_elf_target_shdr             *sectionHeader;
    char                 *sectionHeaderStrtab;

    ElfSymbolTable       *symbolTables;
    ElfRelocationTable   *relTable;
    ElfRelocationATable  *relaTable;

    /* Consumed by runInit/runFini, or freed by ocDeinit_ELF if still pending. */
    struct InitFiniList* init;
    struct InitFiniList* fini;

    /* pointer to the global offset table */
    void *                got_start;
    size_t                got_size;
    uintptr_t             got_exec;
    void *                got_allocation; /* non-NULL for separate NO_EXEC heap */

    /* Owned storage for this object's new COMMON definitions. */
    void *                common_mem;
    size_t                common_size;
    uintptr_t             common_exec;
    void *                common_allocation;
};

typedef
struct _ElfStub {
    void * addr;
    void * target;
    void* got_addr;
    /* flags can hold architecture specific information they are used during
     * lookup of stubs as well. Thus two stubs for the same target with
     * different flags are considered unequal.
    */
    uint8_t flags;
    struct _ElfStub * next;
} ElfStub;

/* Temporary native alias for existing RTS clients and fixtures. */
#if defined(OBJFORMAT_ELF)
typedef ElfStub Stub;
#endif

struct ll_elf_section_info {
    /*
     * The following fields are relevant for stubs next to sections only.
     */
    void * stub_offset;
    size_t stub_size;
    size_t nstubs;
    ElfStub * stubs;

    const char * name;

    ll_elf_target_shdr *sectionHeader;
};
#endif /* ElfTypes_h */
