#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"

#if LL_BUILD_ELF

// It is essential that this is included before any <elf.h> is included. <elf.h>
// defines R_XXX relocations, which would interfere with the COMPAT_R_XXX
// relocations we generate.  E.g. COMPAT_ ## R_ARM_ARM32 would end up as
// const unsigned COMPAT_3 = 0x03; instead of
// const unsigned COMPAT_R_ARM_ARM32 = 0x03;
#include "elf_compat.h"

#include "LinkerInternals.h"
#include "ll_failure.h"
#include "RtsSymbolInfo.h"
#include "linker/MMap.h"
#include "linker/BackendMemory.h"
#include "linker/Elf.h"
#include "linker/elf_reloc_x86_64.h"
#include "linker/elf_reloc_i386.h"
#include "linker/elf_reloc_arm.h"
#include "linker/InitFini.h"
#include "ll_format.h"
#include "ll_noexec.h"

#include "linker/CacheFlush.h"
#include "linker/M32Alloc.h"
#include "linker/SymbolExtras.h"
#include "linker/util.h"
#include "linker/elf_util.h"
#include "linker/LoadNativeObjPosix.h"

#include "ll_fs.h"
#if defined(freebsd_HOST_OS)
#include <link.h>
#endif
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#if LL_NATIVE_ELF
#include <regex.h>    // regex is already used by dlopen() so this is OK
                      // to use here without requiring an additional lib
#endif
#if defined(HAVE_DLFCN_H)
#include <dlfcn.h>
#endif
#if defined(HAVE_SYS_STAT_H)
#include <sys/stat.h>
#endif
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#if defined(HAVE_FCNTL_H)
#include <fcntl.h>
#endif
#if defined(dragonfly_HOST_OS)
#include <sys/tls.h>
#endif

/* On x86_64, relocating symbol references in code compiled without -fPIC
 * requires special handling. The small memory model assumes symbol references
 * fit in a 32-bit slot. When a reference to a symbol in a shared library
 * exceeds this range, we can use a hack (enabled by X86_64_ELF_NONPIC_HACK)
 * which consists in generating "jump islands" (trampolines) for code references
 * (this happens in makeSymbolExtra()).
 *
 * For FUNCTION symbols (STT_FUNC), jump islands work correctly: the call
 * bounces through the trampoline to reach the real function.
 *
 * For DATA symbols (STT_OBJECT, STT_NOTYPE), jump islands are INCORRECT:
 * the jump island address would be embedded as a data pointer (e.g., an info
 * table pointer in a closure), causing the GC to interpret jump island memory
 * as a valid info table — leading to "strange closure type" crashes.
 *
 * We detect data references using ELF_ST_TYPE(sym.st_info) from the symbol
 * table. When a non-function symbol overflows 32-bit range, we emit a clear
 * error message instead of silently creating a corrupt jump island.
 *
 * Workarounds for data reference overflow:
 *   - Use shared Haskell libraries (dynamically-linked GHCi)
 *   - Compile with -fPIC -fexternal-dynamic-refs (generates GOT-based
 *     relocations that can handle arbitrary distances)
 *   - Use +RTS -xp (maps loaded objects into low memory)
 *
 * See bug #781
 * See thread http://www.haskell.org/pipermail/cvs-ghc/2007-September/038458.html
 *
 * Naming Scheme for Symbol Macros
 *
 * SymI_*: symbol is internal to the RTS. It resides in an object
 *         file/library that is statically.
 * SymE_*: symbol is external to the RTS library. It might be linked
 *         dynamically.
 *
 * Sym*_HasProto  : the symbol prototype is imported in an include file
 *                  or defined explicitly
 * Sym*_NeedsProto: the symbol is undefined and we add a dummy
 *                  default proto extern void sym(void);
 */

/* Note [X86_64_ELF_NONPIC_HACK and data references]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * When a relocation overflows 32-bit range, X86_64_ELF_NONPIC_HACK creates
 * a "jump island" (trampoline) via makeSymbolExtra(). The SymbolExtra struct
 * (see rts/LinkerInternals.h) on x86_64 contains:
 *
 *   struct { uint64_t addr; uint8_t jumpIsland[8]; }
 *
 * where jumpIsland = { 0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF, 0x00, 0x00 }
 * is a `jmp *-14(%rip)` instruction that reads `addr` to reach the real symbol.
 *
 * For FUNCTION references (call/jmp targets), this works: the call bounces
 * through the trampoline. For DATA references (e.g., info table pointers like
 * _con_info), the jump island address gets embedded as a data pointer. When
 * the GC later follows this pointer, it finds the jump island bytes instead
 * of a valid info table, causing a "strange closure type" crash.
 *
 * We detect data references using ELF_ST_TYPE() on the symbol table entry.
 * Symbols with type STT_FUNC get the jump island treatment. All other types
 * (STT_OBJECT, STT_NOTYPE — which includes GHC's _con_info symbols) trigger
 * a clear error message directing the user to recompile with -fPIC.
 */

#include "elf_got.h"

#if LL_ELF_NEED_PLT
#  include "elf_plt.h"
#  include "elf_reloc.h"
#endif

/*
   Note [Many ELF Sections]
   ~~~~~~~~~~~~~~~~~~~~~~~~
   The normal section number fields in ELF are limited to 16 bits, which runs
   out of bits when you try to cram in more sections than that.

   To solve this, the fields e_shnum and e_shstrndx in the ELF header have an
   escape value (different for each case), and the actual section number is
   stashed into unused fields in the first section header.

   For symbols, there seems to have been no place in the actual symbol table
   for the extra bits, so the indexes have been moved into an auxiliary
   section instead.
   For symbols in sections beyond 0xff00, the symbol's st_shndx will be an
   escape value (SHN_XINDEX), and the actual 32-bit section number for symbol N
   is stored at index N in the SHT_SYMTAB_SHNDX table.

   These extensions seem to be undocumented in version 4.1 of the ABI and only
   appear in the drafts for the "next" version:
      https://refspecs.linuxfoundation.org/elf/gabi4+/contents.html

*/

static ll_elf_target_word elf_shnum(ll_elf_target_ehdr* ehdr)
{
   ll_elf_target_shdr* shdr = (ll_elf_target_shdr*) ((char*)ehdr + ehdr->e_shoff);
   ll_elf_target_half shnum = ehdr->e_shnum;
   return shnum != LL_ELF_SHN_UNDEF ? shnum : shdr[0].sh_size;
}

static ll_elf_target_word elf_shstrndx(ll_elf_target_ehdr* ehdr)
{
   ll_elf_target_half shstrndx = ehdr->e_shstrndx;
#if LL_ELF_HAS_SHN_XINDEX
   ll_elf_target_shdr* shdr = (ll_elf_target_shdr*) ((char*)ehdr + ehdr->e_shoff);
   return shstrndx != LL_ELF_SHN_XINDEX ? shstrndx : shdr[0].sh_link;
#else
   // some OSes do not support SHN_XINDEX yet, let's revert to
   // old way
   return shstrndx;
#endif
}

#if LL_ELF_HAS_SHN_XINDEX
static ll_elf_target_word*
get_shndx_table(ObjectCode* oc)
{
   if (LL_LIKELY(oc->shndx_table != SHNDX_TABLE_UNINIT(oc->L))) {
      return oc->shndx_table;
   }

   ll_elf_target_word  i;
   char*     ehdrC    = oc->image;
   ll_elf_target_ehdr* ehdr     = (ll_elf_target_ehdr*)ehdrC;
   ll_elf_target_shdr* shdr     = (ll_elf_target_shdr*) (ehdrC + ehdr->e_shoff);
   const ll_elf_target_word shnum = elf_shnum(ehdr);

   for (i = 0; i < shnum; i++) {
     if (shdr[i].sh_type == LL_ELF_SHT_SYMTAB_SHNDX) {
       oc->shndx_table = (ll_elf32_word*)(ehdrC + shdr[i].sh_offset);
       return oc->shndx_table;
     }
   }
   return NULL;
}
#endif

/*
 * ocInit and ocDeinit
 */

void
ocInit_ELF(ObjectCode * oc)
{
    ocDeinit_ELF(oc);

    oc->elf_info = (struct ll_elf_object_info*)LL_CALLOC_FOR(oc->L,
            1, sizeof *oc->elf_info,
            "ocInit_Elf(ObjectCodeFormatInfo)");
    // TODO: fill info
    oc->elf_info->elfHeader = (ll_elf_target_ehdr *)oc->image;
    oc->elf_info->programHeader = oc->elf_info->elfHeader->e_phnum == 0 ? NULL :
        (ll_elf_target_phdr *)((uint8_t *)oc->image + oc->elf_info->elfHeader->e_phoff);
    oc->elf_info->sectionHeader = (ll_elf_target_shdr *) ((uint8_t*)oc->image
                                            + oc->elf_info->elfHeader->e_shoff);
    oc->elf_info->sectionHeaderStrtab = (char*)((uint8_t*)oc->image +
            oc->elf_info->sectionHeader[elf_shstrndx(oc->elf_info->elfHeader)].sh_offset);

    oc->n_sections = elf_shnum(oc->elf_info->elfHeader);

    ElfRelocationTable *relTableLast = NULL;
    ElfRelocationATable *relaTableLast = NULL;
    ElfSymbolTable *symbolTablesLast = NULL;

    /* get the symbol table(s) */
    for(int i=0; i < oc->n_sections; i++) {
        if(LL_ELF_SHT_REL  == oc->elf_info->sectionHeader[i].sh_type) {
            ElfRelocationTable *relTab = (ElfRelocationTable *)LL_CALLOC_FOR(oc->L,
                    1, sizeof(ElfRelocationTable),
                    "ocInit_Elf(ElfRelocationTable");
            relTab->index = i;

            relTab->relocations =
                (ll_elf_target_rel*) ((uint8_t*)oc->elf_info->elfHeader
                                    + oc->elf_info->sectionHeader[i].sh_offset);
            relTab->n_relocations = oc->elf_info->sectionHeader[i].sh_size
                                    / sizeof(ll_elf_target_rel);
            relTab->targetSectionIndex = oc->elf_info->sectionHeader[i].sh_info;

            relTab->sectionHeader      = &oc->elf_info->sectionHeader[i];

            if(relTableLast == NULL) {
                oc->elf_info->relTable = relTab;
                relTableLast = relTab;
            } else {
                relTableLast->next = relTab;
                relTableLast = relTab;
            }

        } else if(LL_ELF_SHT_RELA == oc->elf_info->sectionHeader[i].sh_type) {
            ElfRelocationATable *relTab = (ElfRelocationATable *)LL_CALLOC_FOR(oc->L,
                    1, sizeof(ElfRelocationATable),
                    "ocInit_Elf(ElfRelocationTable");
            relTab->index = i;

            relTab->relocations =
                (ll_elf_target_rela*) ((uint8_t*)oc->elf_info->elfHeader
                                     + oc->elf_info->sectionHeader[i].sh_offset);
            relTab->n_relocations = oc->elf_info->sectionHeader[i].sh_size
                                    / sizeof(ll_elf_target_rela);
            relTab->targetSectionIndex = oc->elf_info->sectionHeader[i].sh_info;

            relTab->sectionHeader      = &oc->elf_info->sectionHeader[i];

            if(relaTableLast == NULL) {
                oc->elf_info->relaTable = relTab;
                relaTableLast = relTab;
            } else {
                relaTableLast->next = relTab;
                relaTableLast = relTab;
            }

        } else if(LL_ELF_SHT_SYMTAB == oc->elf_info->sectionHeader[i].sh_type) {

            ElfSymbolTable *symTab = (ElfSymbolTable *)LL_CALLOC_FOR(oc->L,
                    1, sizeof(ElfSymbolTable),
                    "ocInit_Elf(ElfSymbolTable");

            symTab->index = i; /* store the original index, so we can later
                                * find or assert that we are dealing with the
                                * correct symbol table */

            ll_elf_target_sym *stab = (ll_elf_target_sym*)((uint8_t*)oc->elf_info->elfHeader
                                       + oc->elf_info->sectionHeader[i].sh_offset);
            symTab->n_symbols = oc->elf_info->sectionHeader[i].sh_size
                                / sizeof(ll_elf_target_sym);
            symTab->symbols = (ElfSymbol *)LL_CALLOC_FOR(oc->L,
                    symTab->n_symbols, sizeof(ElfSymbol),
                    "ocInit_Elf(ElfSymbol)");

            /* get the strings table */
            size_t lnkIdx = oc->elf_info->sectionHeader[i].sh_link;
            symTab->names = (char*)(uint8_t*)oc->elf_info->elfHeader
                            + oc->elf_info->sectionHeader[lnkIdx].sh_offset;

            /* build the ElfSymbols from the symbols */
            for(size_t j=0; j < symTab->n_symbols; j++) {

                symTab->symbols[j].name = stab[j].st_name == 0
                                          ? "(noname)"
                                          : symTab->names + stab[j].st_name;
                symTab->symbols[j].elf_sym = &stab[j];
                /* we don't have an address for this symbol yet; this will be
                 * populated during ocGetNames. hence addr = NULL.
                 */
                symTab->symbols[j].addr  = NULL;
                symTab->symbols[j].got_addr = NULL;
            }

            /* append the ElfSymbolTable */
            if(symbolTablesLast == NULL) {
                oc->elf_info->symbolTables = symTab;
                symbolTablesLast = symTab;
            } else {
                symbolTablesLast->next = symTab;
                symbolTablesLast = symTab;
            }
        }
    }
}

void
ocDeinit_ELF(ObjectCode * oc)
{
    const ll_status saved_status = oc->L->error_status;
    const bool saved_pre_backend = oc->L->error_from_pre_backend;
    char saved_message[LL_ERROR_CAPACITY];
    if (saved_status != LL_OK)
        memcpy(saved_message, oc->L->error_message, sizeof(saved_message));
    /* free all ElfSymbolTables, and their associated
     * ElfSymbols
     */
    if(oc->elf_info != NULL) {
        /* Failed or never-started initialization leaves these lists owned by
         * the object. Successful execution already consumes and clears them. */
        freeInitFiniList(oc->L, oc->elf_info->init);
        freeInitFiniList(oc->L, oc->elf_info->fini);
        oc->elf_info->init = NULL;
        oc->elf_info->fini = NULL;
#if LL_ELF_NEED_PLT
        if (oc->sections != NULL) {
            for (int i = 0; i < oc->n_sections; ++i) {
                if (oc->sections[i].elf_info != NULL)
                    elf_freeStubs(oc->L, &oc->sections[i]);
            }
        }
#endif
        freeGot(oc);
        if (oc->elf_info->common_allocation != NULL) {
            free(oc->elf_info->common_allocation);
        } else if (oc->elf_info->common_mem != NULL) {
            ll_munmap_for_linker(oc->L, oc->elf_info->common_mem,
                                oc->elf_info->common_size, "ocDeinit_ELF(COMMON)");
        }
        ElfSymbolTable * last = oc->elf_info->symbolTables;

        while(last != NULL) {
            ElfSymbolTable * t = last;
            last = last->next;
            LL_FREE_FOR(oc->L, t->symbols);
            LL_FREE_FOR(oc->L, t);
        }

        {
            ElfRelocationTable *last = oc->elf_info->relTable;
            while (last != NULL) {
                ElfRelocationTable *t = last;
                last = last->next;
                LL_FREE_FOR(oc->L, t);
            }
        }

        {
            ElfRelocationATable *last = oc->elf_info->relaTable;
            while (last != NULL) {
                ElfRelocationATable *t = last;
                last = last->next;
                LL_FREE_FOR(oc->L, t);
            }
        }

        LL_FREE_FOR(oc->L, oc->elf_info);
        oc->elf_info = NULL;
#if LL_ELF_HAS_SHN_XINDEX
        oc->shndx_table = SHNDX_TABLE_UNINIT(oc->L);
#endif
    }
    if (saved_status != LL_OK) {
        oc->L->error_status = saved_status;
        oc->L->error_from_pre_backend = saved_pre_backend;
        memcpy(oc->L->error_message, saved_message, sizeof(saved_message));
    }
}

/*
 * Generic ELF functions
 */

int
ocVerifyImage_ELF ( ObjectCode* oc )
{
   if (!ocValidateImage_ELF(oc)) return 0;
   ll_elf_target_shdr* shdr;
   ll_elf_target_sym*  stab;
   int j, nent, nstrtab, nsymtabs;
   ll_elf_target_word i, shnum, shstrndx;
   char* sh_strtab;

   char*     ehdrC = (char*)(oc->image);
   ll_elf_target_ehdr* ehdr  = (ll_elf_target_ehdr*)ehdrC;

   if (ehdr->e_ident[LL_ELF_EI_MAG0] != LL_ELF_ELFMAG0 ||
       ehdr->e_ident[LL_ELF_EI_MAG1] != LL_ELF_ELFMAG1 ||
       ehdr->e_ident[LL_ELF_EI_MAG2] != LL_ELF_ELFMAG2 ||
       ehdr->e_ident[LL_ELF_EI_MAG3] != LL_ELF_ELFMAG3) {
      LL_LOG_ERROR_FOR(oc->L, "%s: not an ELF object", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[LL_ELF_EI_CLASS] != LL_ELF_CLASS) {
      LL_LOG_ERROR_FOR(oc->L, "%s: unsupported ELF format", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[LL_ELF_EI_DATA] == LL_ELF_ELFDATA2LSB) {
       LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "Is little-endian\n" ));
   } else
   if (ehdr->e_ident[LL_ELF_EI_DATA] == LL_ELF_ELFDATA2MSB) {
       LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "Is big-endian\n" ));
   } else {
       LL_LOG_ERROR_FOR(oc->L, "%s: unknown endianness", oc->fileName);
       return 0;
   }

   if (ehdr->e_type != LL_ELF_ET_REL) {
      LL_LOG_ERROR_FOR(oc->L, "%s: not a relocatable object (.o) file", oc->fileName);
      return 0;
   }
   LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "Is a relocatable object (.o) file\n" ));

   LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "Architecture is " ));
   switch (ehdr->e_machine) {
#if LL_ELF_HAS_EM_ARM
      case LL_ELF_EM_ARM:   LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "arm" )); break;
#endif
      case LL_ELF_EM_386:   LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "x86" )); break;
#if LL_ELF_HAS_EM_SPARC32PLUS
      case LL_ELF_EM_SPARC32PLUS:
#endif
      case LL_ELF_EM_SPARC: LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "sparc" )); break;
#if LL_ELF_HAS_EM_IA_64
      case LL_ELF_EM_IA_64: LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "ia64" )); break;
#endif
      case LL_ELF_EM_PPC:   LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "powerpc32" )); break;
#if LL_ELF_HAS_EM_PPC64
      case LL_ELF_EM_PPC64: LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "powerpc64" ));
          LL_LOG_ERROR_FOR(oc->L, "%s: RTS linker not implemented on PowerPC 64-bit",
                     oc->fileName);
          return 0;
#endif
#if LL_ELF_HAS_EM_S390
      case LL_ELF_EM_S390:  LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "s390" ));
          LL_LOG_ERROR_FOR(oc->L, "%s: RTS linker not implemented on s390",
                     oc->fileName);
          return 0;
#endif
#if LL_ELF_HAS_EM_X86_64
      case LL_ELF_EM_X86_64: LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "x86_64" )); break;
#elif LL_ELF_HAS_EM_AMD64
      case LL_ELF_EM_AMD64: LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "amd64" )); break;
#endif
#if LL_ELF_HAS_EM_AARCH64
      case LL_ELF_EM_AARCH64: LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "aarch64" )); break;
#endif
#if LL_ELF_HAS_EM_RISCV
      case LL_ELF_EM_RISCV:  LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "riscv" )); break;
#endif
#if LL_ELF_HAS_EM_LOONGARCH
      case LL_ELF_EM_LOONGARCH:  LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "loongarch64" ));
          LL_LOG_ERROR_FOR(oc->L, "%s: RTS linker not implemented on loongarch64",
                     oc->fileName);
          return 0;
#endif
       default:       LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "unknown" ));
                     LL_LOG_ERROR_FOR(oc->L, "%s: unknown architecture (e_machine == %d)"
                                , oc->fileName, ehdr->e_machine);
                     return 0;
   }

   shnum = elf_shnum(ehdr);
   LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L,
             "\nSection header table: start %ld, n_entries %d, ent_size %d\n",
             (long)ehdr->e_shoff, shnum, ehdr->e_shentsize  ));

   LL_CHECK_FOR(oc->L, ehdr->e_shentsize == sizeof(ll_elf_target_shdr));

   shdr = (ll_elf_target_shdr*) (ehdrC + ehdr->e_shoff);

   shstrndx = elf_shstrndx(ehdr);
   if (shstrndx == LL_ELF_SHN_UNDEF) {
      LL_LOG_ERROR_FOR(oc->L, "%s: no section header string table", oc->fileName);
      return 0;
   } else {
      LL_IF_DEBUG_FOR(oc->L, linker,LL_DEBUG_FOR(oc->L, "Section header string table is section %d\n",
                          shstrndx));
      sh_strtab = ehdrC + shdr[shstrndx].sh_offset;
   }

   for (i = 0; i < shnum; i++) {
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "%2d:  ", i ));
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "type=%2d  ", (int)shdr[i].sh_type ));
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "size=%4d  ", (int)shdr[i].sh_size ));
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "offs=%4d  ", (int)shdr[i].sh_offset ));
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "  (%p .. %p)  ",
               (void *)((uintptr_t)ehdrC + shdr[i].sh_offset),
               (void *)((uintptr_t)ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1)));

#define SECTION_INDEX_VALID(ndx) (ndx > LL_ELF_SHN_UNDEF && ndx < shnum)

      switch (shdr[i].sh_type) {

        case LL_ELF_SHT_REL:
        case LL_ELF_SHT_RELA:
          LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, shdr[i].sh_type == LL_ELF_SHT_REL ? "Rel  " : "RelA "));

          if (!SECTION_INDEX_VALID(shdr[i].sh_link)) {
            if (shdr[i].sh_link == LL_ELF_SHN_UNDEF)
              LL_LOG_ERROR_FOR(oc->L, "\n%s: relocation section #%d has no symbol table\n"
                         "This object file has probably been fully stripped. "
                         "Such files cannot be linked.\n",
                         oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);
            else
              LL_LOG_ERROR_FOR(oc->L, "\n%s: relocation section #%d has an invalid link field (%d)\n",
                         oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                         i, shdr[i].sh_link);
            return 0;
          }
          if (shdr[shdr[i].sh_link].sh_type != LL_ELF_SHT_SYMTAB) {
            LL_LOG_ERROR_FOR(oc->L, "\n%s: relocation section #%d does not link to a symbol table\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);
            return 0;
          }
          if (!SECTION_INDEX_VALID(shdr[i].sh_info)) {
            LL_LOG_ERROR_FOR(oc->L, "\n%s: relocation section #%d has an invalid info field (%d)\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                       i, shdr[i].sh_info);
            return 0;
          }

          break;
        case LL_ELF_SHT_SYMTAB:
          LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "Sym  "));

          if (!SECTION_INDEX_VALID(shdr[i].sh_link)) {
            LL_LOG_ERROR_FOR(oc->L, "\n%s: symbol table section #%d has an invalid link field (%d)\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                       i, shdr[i].sh_link);
            return 0;
          }
          if (shdr[shdr[i].sh_link].sh_type != LL_ELF_SHT_STRTAB) {
            LL_LOG_ERROR_FOR(oc->L, "\n%s: symbol table section #%d does not link to a string table\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);

            return 0;
          }
          break;
        case LL_ELF_SHT_STRTAB: LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "Str  ")); break;
        default:         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "     ")); break;
      }
      if (sh_strtab) {
          LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "sname=%s\n", sh_strtab + shdr[i].sh_name ));
      }
   }

   LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "\nString tables\n" ));
   nstrtab = 0;
   for (i = 0; i < shnum; i++) {
      if (shdr[i].sh_type == LL_ELF_SHT_STRTAB
          /* Ignore the section header's string table. */
          && i != shstrndx
          /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != strncmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "   section %d is a normal string table\n", i ));
         nstrtab++;
      }
   }
   if (nstrtab == 0) {
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "   no normal string tables (potentially, but not necessarily a problem)\n"));
   }
#if LL_ELF_HAS_SHN_XINDEX
   ll_elf_target_word* shndxTable = get_shndx_table(oc);
#endif
   nsymtabs = 0;
   LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "Symbol tables\n" ));
   for (i = 0; i < shnum; i++) {
      if (shdr[i].sh_type != LL_ELF_SHT_SYMTAB) continue;
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "section %d is a symbol table\n", i ));
      nsymtabs++;
      stab = (ll_elf_target_sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(ll_elf_target_sym);
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "   number of entries is apparently %d (%ld rem)\n",
               nent,
               (long)shdr[i].sh_size % sizeof(ll_elf_target_sym)
             ));
      if (0 != shdr[i].sh_size % sizeof(ll_elf_target_sym)) {
         LL_LOG_ERROR_FOR(oc->L, "%s: non-integral number of symbol table entries", oc->fileName);
         return 0;
      }
      for (j = 0; j < nent; j++) {
         ll_elf_target_word secno = stab[j].st_shndx;
#if LL_ELF_HAS_SHN_XINDEX
         /* See Note [Many ELF Sections] */
         if (secno == LL_ELF_SHN_XINDEX) {
            LL_CHECK_FOR(oc->L, shndxTable);
            secno = shndxTable[j];
         }
#endif
         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "   %2d  ", j ));
         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "  sec=%-5d  size=%-3d  val=%5p  ",
                             (int)secno,
                             (int)stab[j].st_size,
                             (char*)stab[j].st_value ));

         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "type=" ));
         switch (LL_ELF_ST_TYPE(stab[j].st_info)) {
            case LL_ELF_STT_NOTYPE:  LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "notype " )); break;
            case LL_ELF_STT_OBJECT:  LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "object " )); break;
            case LL_ELF_STT_FUNC  :  LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "func   " )); break;
            case LL_ELF_STT_SECTION: LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "section" )); break;
            case LL_ELF_STT_FILE:    LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "file   " )); break;
            default:          LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "?      " )); break;
         }
         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "  " ));

         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "bind=" ));
         switch (LL_ELF_ST_BIND(stab[j].st_info)) {
            case LL_ELF_STB_LOCAL :  LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "local " )); break;
            case LL_ELF_STB_GLOBAL:  LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "global" )); break;
            case LL_ELF_STB_WEAK  :  LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "weak  " )); break;
            default:          LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "?     " )); break;
         }
         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "  " ));

         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "other=%2x ", stab[j].st_other ));
         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "name=%s [%x]\n",
                        ehdrC + shdr[shdr[i].sh_link].sh_offset
                              + stab[j].st_name, stab[j].st_name ));
      }
   }

   if (nsymtabs == 0) {
     // Not having a symbol table is not in principle a problem.
     // When an object file has no symbols then the 'strip' program
     // typically will remove the symbol table entirely.
     LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "   no symbol tables (potentially, but not necessarily a problem)\n"));
   }

   return 1;
}

/* Figure out what kind of section it is.  Logic derived from
   Figure 1.14 ("Special Sections") of the ELF document
   ("Portable Formats Specification, Version 1.1"). */
static SectionKind getSectionKind_ELF( ll_elf_target_shdr *hdr, int *is_bss )
{
    *is_bss = false;

    if (hdr->sh_type == LL_ELF_SHT_PROGBITS
        && (hdr->sh_flags & LL_ELF_SHF_ALLOC) && (hdr->sh_flags & LL_ELF_SHF_EXECINSTR)) {
        /* .text-style section */
        return SECTIONKIND_CODE_OR_RODATA;
    }

    if (hdr->sh_type == LL_ELF_SHT_PROGBITS
            && (hdr->sh_flags & LL_ELF_SHF_ALLOC) && (hdr->sh_flags & LL_ELF_SHF_WRITE)) {
            /* .data-style section */
            return SECTIONKIND_RWDATA;
    }

    if (hdr->sh_type == LL_ELF_SHT_PROGBITS
        && (hdr->sh_flags & LL_ELF_SHF_ALLOC) && !(hdr->sh_flags & LL_ELF_SHF_WRITE)) {
        /* .rodata-style section */
        return SECTIONKIND_CODE_OR_RODATA;
    }
#if LL_ELF_HAS_SHT_INIT_ARRAY
    if (hdr->sh_type == LL_ELF_SHT_INIT_ARRAY
        && (hdr->sh_flags & LL_ELF_SHF_ALLOC) && (hdr->sh_flags & LL_ELF_SHF_WRITE)) {
       /* .init_array section */
        return SECTIONKIND_INIT_ARRAY;
    }
#endif /* not SHT_INIT_ARRAY */
#if LL_ELF_HAS_SHT_FINI_ARRAY
    if (hdr->sh_type == LL_ELF_SHT_FINI_ARRAY
        && (hdr->sh_flags & LL_ELF_SHF_ALLOC) && (hdr->sh_flags & LL_ELF_SHF_WRITE)) {
       /* .fini_array section */
        return SECTIONKIND_FINI_ARRAY;
    }
#endif /* not SHT_INIT_ARRAY */
    if (hdr->sh_type == LL_ELF_SHT_NOBITS
        && (hdr->sh_flags & LL_ELF_SHF_ALLOC) && (hdr->sh_flags & LL_ELF_SHF_WRITE)) {
        /* .bss-style section */
        *is_bss = true;
        return SECTIONKIND_RWDATA;
    }

    return SECTIONKIND_OTHER;
}

#if !LL_ELF_NEED_PLT

static void *
mapObjectFileSection (ll_linker *L, int fd, ll_elf_target_word offset, ll_elf_target_word size,
                      void **mapped_start, ll_word *mapped_size,
                      ll_word *mapped_offset, ll_status *failure_status)
{
    void *p;
    size_t pageOffset, pageSize;

    pageOffset = ll_round_down_to_page_for(L, offset);
    pageSize = ll_round_up_to_page_for(L, offset-pageOffset+size);
    p = ll_backend_map_file_write(L, fd, pageSize, pageOffset, failure_status);
    if (p == NULL) return NULL;
    *mapped_size = pageSize;
    *mapped_offset = pageOffset;
    *mapped_start = p;
    return (void*)((ll_word)p + offset - pageOffset);
}
#endif

/* Sections are independently owned RW heap blocks in NO_EXEC. All file data,
 * including metadata sections, retains its original bytes. Execution addresses
 * are assigned before any symbol is published, independently of heap layout. */
static int noexec_section_ELF(ObjectCode *oc, ll_elf_target_word index,
                              SectionKind kind, bool is_bss)
{
    const ll_elf_target_shdr *header = &oc->elf_info->sectionHeader[index];
    ll_noexec_block block;
    const size_t alignment = header->sh_addralign != 0 ? header->sh_addralign : 1;
    size_t tail_size = 0, tail_offset = header->sh_size;
#if LL_TARGET_AARCH64 || LL_TARGET_ARM || LL_TARGET_RISCV64
    if (kind != SECTIONKIND_OTHER && header->sh_size != 0 && !is_bss) {
        const size_t count = elf_numberOfStubsForSection(oc, index);
        if (count > SIZE_MAX / STUB_SIZE) {
            ll_error_set(oc->L, LL_E_NOMEM, "NO_EXEC ELF stub capacity is too large");
            return 0;
        }
        tail_size = count * STUB_SIZE;
    }
#else
    (void)is_bss;
#endif
    if (tail_size != 0) {
        if (!ll_noexec_allocate_tail(oc->L, header->sh_size, alignment,
                                     tail_size, 4, &block, &tail_offset)) return 0;
    } else if (!ll_noexec_allocate(oc->L, header->sh_size, alignment, &block)) {
        return 0;
    }
    if (header->sh_size != 0 && header->sh_type != LL_ELF_SHT_NOBITS)
        memcpy(block.site.work, oc->image + header->sh_offset, header->sh_size);
    Section *section = &oc->sections[index];
    addSection(oc->L, section, kind, SECTION_NOEXEC, block.site.work,
               header->sh_size, 0, block.allocation, 0,
               sizeof(struct ll_elf_section_info));
    section->exec_start = block.site.exec;
    if (tail_size != 0) {
        section->elf_info->stub_offset = block.site.work + tail_offset;
        section->elf_info->stub_size = tail_size;
    }
    if (kind != SECTIONKIND_OTHER && header->sh_size != 0)
        addProddableBlock(oc->L, &oc->proddables, section->start, section->size);
    return 1;
}

int
ocGetNames_ELF ( ObjectCode* oc )
{
   ll_elf_target_word i;
   int result, fd = -1;

   char*     ehdrC    = (char*)(oc->image);
   ll_elf_target_ehdr* ehdr     = (ll_elf_target_ehdr*)ehdrC;

   ll_elf_target_shdr* shdr     = (ll_elf_target_shdr*) (ehdrC + ehdr->e_shoff);
   Section * sections;
#if LL_ELF_HAS_SHN_XINDEX
   ll_elf_target_word* shndxTable = get_shndx_table(oc);
#endif
   const ll_elf_target_word shnum = elf_shnum(ehdr);

   LL_ASSERT_FOR(oc->L, oc->L->symhash != NULL);

   sections = (Section*)LL_CALLOC_FOR(oc->L, shnum, sizeof(Section),
                                       "ocGetNames_ELF(sections)");
   oc->sections = sections;
   oc->n_sections = shnum;

   if (oc->imageMapped && !ll_noexec_enabled(oc->L)) {
       fd = ll_backend_open_readonly(oc->fileName);
       if (fd == -1) {
           ll_error_set(oc->L, LL_E_IO,
                        "loadObj: can't open %" PATH_FMT, oc->fileName);
           LL_LOG_ERROR_FOR(oc->L, "loadObj: can't open %" PATH_FMT, oc->fileName);
           return 0;
       }
   }

   for (i = 0; i < shnum; i++) {
      int         is_bss = false;
      SectionKind kind   = getSectionKind_ELF(&shdr[i], &is_bss);
      SectionAlloc alloc = SECTION_NOMEM;
      void *start = NULL, *mapped_start = NULL;
      ll_word mapped_size = 0, mapped_offset = 0;
      ll_word size = shdr[i].sh_size;
      ll_word offset = shdr[i].sh_offset;
      ll_word align = shdr[i].sh_addralign == 0 ? 1 : shdr[i].sh_addralign;
      const char *sh_name = oc->elf_info->sectionHeaderStrtab + shdr[i].sh_name;

      /*
       * Identify initializer and finalizer lists
       *
       * See Note [Initializers and finalizers (ELF)].
       */
      if (kind == SECTIONKIND_CODE_OR_RODATA
            && 0 == strncmp(".init", sh_name, 5)) {
          addInitFini(oc->L, &oc->elf_info->init, &oc->sections[i], INITFINI_INIT, 0);
      } else if (kind == SECTIONKIND_CODE_OR_RODATA
            && 0 == strncmp(".fini", sh_name, 5)) {
          addInitFini(oc->L, &oc->elf_info->fini, &oc->sections[i], INITFINI_FINI, 0);
      } else if (kind == SECTIONKIND_INIT_ARRAY
            || 0 == strncmp(".init_array", sh_name, 11)) {
          uint32_t prio;
          if (sscanf(sh_name, ".init_array.%d", &prio) != 1) {
              // Sections without an explicit priority are run last
              prio = 0;
          }
          prio += 0x10000; // .init_arrays run after .ctors
          addInitFini(oc->L, &oc->elf_info->init, &oc->sections[i], INITFINI_INIT_ARRAY, prio);
          kind = SECTIONKIND_INIT_ARRAY;
      } else if (kind == SECTIONKIND_FINI_ARRAY
            || 0 == strncmp(".fini_array", sh_name, 11)) {
          uint32_t prio;
          if (sscanf(sh_name, ".fini_array.%d", &prio) != 1) {
              // Sections without an explicit priority are run last
              prio = 0;
          }
          prio += 0x10000; // .fini_arrays run before .dtors
          addInitFini(oc->L, &oc->elf_info->fini, &oc->sections[i], INITFINI_FINI_ARRAY, prio);
          kind = SECTIONKIND_FINI_ARRAY;

      /* N.B. a compilation unit may have more than one .ctor section; we
       * must run them all. See #21618 for a case where this happened */
      } else if (0 == strncmp(".ctors", sh_name, 6)) {
          uint32_t prio;
          if (sscanf(sh_name, ".ctors.%d", &prio) != 1) {
              // Sections without an explicit priority are run last
              prio = 0;
          }
          // .ctors/.dtors are executed in reverse order: higher numbers are
          // executed first
          prio = 0xffff - prio;
          addInitFini(oc->L, &oc->elf_info->init, &oc->sections[i], INITFINI_CTORS, prio);
          kind = SECTIONKIND_INIT_ARRAY;
      } else if (0 == strncmp(".dtors", sh_name, 6)) {
          uint32_t prio;
          if (sscanf(sh_name, ".dtors.%d", &prio) != 1) {
              // Sections without an explicit priority are run last
              prio = 0;
          }
          // .ctors/.dtors are executed in reverse order: higher numbers are
          // executed first
          prio = 0xffff - prio;
          addInitFini(oc->L, &oc->elf_info->fini, &oc->sections[i], INITFINI_DTORS, prio);
          kind = SECTIONKIND_FINI_ARRAY;
      }


      if (ll_noexec_enabled(oc->L) && shdr[i].sh_type != LL_ELF_SHT_NULL) {
          if (!noexec_section_ELF(oc, i, kind, is_bss)) goto fail;
      } else if (is_bss && size > 0) {
         /* This is a non-empty .bss section.  Allocate zeroed space for
            it, and set its .sh_offset field such that
            ehdrC + .sh_offset == addr_of_zeroed_space.  */
#if RTS_LINKER_USE_MMAP
          if (USE_CONTIGUOUS_MMAP || oc->L->services.cfg.always_pic) {
              /* The space for bss sections is already preallocated */
              LL_CHECK_FOR(oc->L, oc->bssBegin != NULL);
              alloc = SECTION_NOMEM;
              LL_CHECK_FOR(oc->L, oc->image != 0x0);
              start =
                oc->image + ll_round_up_to_align(oc->bssBegin - oc->image, align);
              oc->bssBegin = (char*)start + size;
              LL_CHECK_FOR(oc->L, oc->bssBegin <= oc->bssEnd);
          } else {
              /* Use mmapForLinker to allocate .bss, otherwise the malloced
               * address might be out of range for sections that are mmaped.
               */
              alloc = SECTION_MMAP;
              start = ll_mmap_anon_for_linker(oc->L, size);
              if (start == NULL) {
                  const int saved_errno = errno;
                  ll_error_set(oc->L, LL_E_NOMEM,
                               "failed to mmap memory for bss. errno = %d",
                               saved_errno);
                  LL_LOG_ERROR_FOR(oc->L, "failed to mmap memory for bss. "
                                   "errno = %d", saved_errno);
                  goto fail;
              }
              mapped_start = start;
              mapped_offset = 0;
              mapped_size = ll_round_up_to_page_for(oc->L, size);
          }
          LL_CHECK_FOR(oc->L, start != 0x0);
#else
          alloc = SECTION_MALLOC;
          start = LL_CALLOC_FOR(oc->L, 1, size, "ocGetNames_ELF(BSS)");
          mapped_start = start;
#endif
         /*
         debugBelch("BSS section at 0x%x, size %d\n",
                         zspace, shdr[i].sh_size);
         */
          addSection(oc->L, &sections[i], kind, alloc, start, size,
                     mapped_offset, mapped_start, mapped_size, sizeof(struct ll_elf_section_info));

          oc->sections[i].elf_info->nstubs = 0;
          oc->sections[i].elf_info->stub_offset = NULL;
          oc->sections[i].elf_info->stub_size = 0;
          oc->sections[i].elf_info->stubs = NULL;
      } else if (kind != SECTIONKIND_OTHER && size > 0) {

#if LL_ELF_NEED_PLT
          /* To support stubs next to sections, we will use the following
           * layout:
           *
           * .--------------.
           * | Section data |
           * |--------------|
           * | Stub space   |
           * '--------------'
           *
           * This ensures that the plt stubs are in range for the section data,
           * Unless the section data exceeds the size for relative jump, in
           * which case I wouldn't know how to solve this, without starting to
           * break up the section itself.
           */

          unsigned nstubs = elf_numberOfStubsForSection(oc, i);
          unsigned stub_space = STUB_SIZE * nstubs;
          unsigned full_size = size+stub_space;

          // use M32 allocator to avoid fragmentation and relocations impossible
          // to fulfil (cf #24432)
          bool executable = kind == SECTIONKIND_CODE_OR_RODATA;
          m32_allocator *allocator = executable ? oc->rx_m32 : oc->rw_m32;

          // Correctly align the section. This is particularly important for
          // the alignment of .rodata.cstNN sections.
          start = m32_alloc_for(oc->L, allocator, full_size, align);
          if (start == NULL) goto section_no_memory;
          alloc = SECTION_M32;

          /* copy only the image part over; we don't want to copy data
           * into the stub part.
           */
          memcpy(start, oc->image + offset, size);
#else
          if (USE_CONTIGUOUS_MMAP || oc->L->services.cfg.always_pic) {
              // already mapped.
              start = oc->image + offset;
              alloc = SECTION_NOMEM;
          }
          // use the m32 allocator if either the image is not mapped
          // (i.e. we cannot map the sections separately), or if the section
          // size is small.
          else if (!oc->imageMapped || size < ll_page_size_for(oc->L) / 3) {
              bool executable = kind == SECTIONKIND_CODE_OR_RODATA;
              m32_allocator *allocator = executable ? oc->rx_m32 : oc->rw_m32;
              // Correctly align the section. This is particularly important for
              // the alignment of .rodata.cstNN sections.
              //
              // llvm will emit see paddq statements for x86_64 under
              // optimisation and load from RODATA sections, specifically
              // .rodata.cst16. Also we may encounter .rodata.cst32 sections
              // in objects using AVX instructions (see #23066).
              start = m32_alloc_for(oc->L, allocator, size, align);
              if (start == NULL) goto section_no_memory;
              memcpy(start, oc->image + offset, size);
              alloc = SECTION_M32;
          } else {
              ll_status mapping_status;
              start = mapObjectFileSection(oc->L, fd, offset, size,
                                           &mapped_start, &mapped_size,
                                           &mapped_offset, &mapping_status);
              if (start == NULL) {
                  ll_error_set(oc->L, mapping_status,
                               "loadObj: could not map a section of %" PATH_FMT,
                               OC_INFORMATIVE_FILENAME(oc));
                  goto fail;
              }
              alloc = SECTION_MMAP;
          }
#endif
          addSection(oc->L, &sections[i], kind, alloc, start, size,
                     mapped_offset, mapped_start, mapped_size, sizeof(struct ll_elf_section_info));

#if LL_ELF_NEED_PLT
          oc->sections[i].elf_info->nstubs = 0;
          oc->sections[i].elf_info->stub_offset = (uint8_t*)start + size;
          oc->sections[i].elf_info->stub_size = stub_space;
          oc->sections[i].elf_info->stubs = NULL;
#else
          oc->sections[i].elf_info->nstubs = 0;
          oc->sections[i].elf_info->stub_offset = NULL;
          oc->sections[i].elf_info->stub_size = 0;
          oc->sections[i].elf_info->stubs = NULL;
#endif

          addProddableBlock(oc->L, &oc->proddables, start, size);
      } else {
          void *metadata = shdr[i].sh_type == LL_ELF_SHT_NULL || shdr[i].sh_type == LL_ELF_SHT_NOBITS
                           ? NULL : oc->image + offset;
          /* Empty definitions still need a non-NULL address. Their ignored
           * file offset need not lie within the image, so use its base. */
          if (size == 0 && metadata == NULL) metadata = oc->image;
          addSection(oc->L, &oc->sections[i], kind, alloc, metadata, size,
                     0, 0, 0, sizeof(struct ll_elf_section_info));
          oc->sections[i].elf_info->nstubs = 0;
          oc->sections[i].elf_info->stub_offset = NULL;
          oc->sections[i].elf_info->stub_size = 0;
          oc->sections[i].elf_info->stubs = NULL;
      }
      oc->sections[i].elf_info->name          = sh_name;
      oc->sections[i].elf_info->sectionHeader = &shdr[i];
      if (ll_noexec_enabled(oc->L) && shdr[i].sh_type == LL_ELF_SHT_NULL)
          oc->sections[i].exec_start = 0;

   }

   /* All section addresses must exist before constructing any symbol address.
    * Section tables need not place symbol tables after the sections they name. */
   if (oc->elf_info->symbolTables != NULL) {

      /* copy stuff into this module's object symbol table */

      oc->n_symbols = 0;
      for(ElfSymbolTable *symTab = oc->elf_info->symbolTables;
          symTab != NULL; symTab = symTab->next) {
          oc->n_symbols += symTab->n_symbols;
      }

      oc->symbols = LL_CALLOC_FOR(oc->L, oc->n_symbols, sizeof(Symbol_t),
                                   "ocGetNames_ELF(oc->symbols)");
      // Note calloc: if we fail partway through initializing symbols, we need
      // to undo the additions to the symbol table so far. We know which ones
      // have been added by whether the entry is NULL or not.

      unsigned curSymbol = 0;

      size_t common_size = 0;
      size_t common_used = 0;
      bool new_common = false;
      for(ElfSymbolTable *symTab = oc->elf_info->symbolTables;
           symTab != NULL; symTab = symTab->next) {
           for (size_t j = 0; j < symTab->n_symbols; j++) {
               ElfSymbol *symbol = &symTab->symbols[j];
               if (LL_ELF_SHN_COMMON == symTab->symbols[j].elf_sym->st_shndx) {
                   /* Skip COMMON symbols already defined by a previously-loaded
                    * object; we will reuse the existing allocation. */
                   if (!lookupStrHashTable(oc->L->symhash, symTab->symbols[j].name)) {
                       new_common = true;
                       if (symbol->elf_sym->st_size > SIZE_MAX - common_size) {
                           ll_error_set(oc->L, LL_E_UNSUPPORTED,
                                        "ELF COMMON allocation is too large");
                           goto fail;
                       }
                       common_size += symbol->elf_sym->st_size;
                   }
               }
           }
      }
      void * common_mem = NULL;
      if (ll_noexec_enabled(oc->L) && new_common) {
          ll_noexec_block block;
          if (!ll_noexec_allocate(oc->L, common_size, 8, &block)) goto fail;
          common_mem = block.site.work;
          oc->elf_info->common_mem = common_mem;
          oc->elf_info->common_size = common_size;
          oc->elf_info->common_exec = block.site.exec;
          oc->elf_info->common_allocation = block.allocation;
      } else if(common_size > 0) {
          const size_t page_size = ll_page_size_for(oc->L);
          if (common_size > SIZE_MAX - (page_size - 1)) {
              ll_error_set(oc->L, LL_E_UNSUPPORTED,
                           "ELF rounded COMMON allocation is too large");
              goto fail;
          }
          oc->elf_info->common_size = common_size;
          common_mem = ll_mmap_anon_for_linker(oc->L, common_size);
          oc->elf_info->common_mem = common_mem;
          if (common_mem == NULL) {
              ll_error_set(oc->L, LL_E_NOMEM,
                           "ocGetNames_ELF: Failed to allocate memory for SHN_COMMONs");
              LL_LOG_ERROR_FOR(oc->L,
                               "ocGetNames_ELF: Failed to allocate memory for SHN_COMMONs");
              goto fail;
          }
      }

      //TODO: we ignore local symbols anyway right? So we can use the
      //      shdr[i].sh_info to get the index of the first non-local symbol
      // ie we should use j = shdr[i].sh_info
       for(ElfSymbolTable *symTab = oc->elf_info->symbolTables;
           symTab != NULL; symTab = symTab->next) {
           for (size_t j = 0; j < symTab->n_symbols; j++) {

               char isLocal = false; /* avoids uninit-var warning */
               ll_hs_bool isWeak = LL_HS_BOOL_FALSE;
               ll_symbol_name *nm = symTab->symbols[j].name;
               unsigned short shndx = symTab->symbols[j].elf_sym->st_shndx;

               ElfSymbol *symbol = &symTab->symbols[j];

               ll_elf_target_word secno;


               /* See Note [Many ELF Sections] */
               /* Note that future checks for special SHN_* numbers should check
                * the shndx variable, not the section number in secno. Sections
                * with the real number in the SHN_LORESERVE..HIRESERVE range
                * will have shndx SHN_XINDEX and a secno with one of the
                * reserved values. */
               secno = shndx;
#if LL_ELF_HAS_SHN_XINDEX
               if (shndx == LL_ELF_SHN_XINDEX) {
                  LL_CHECK_FOR(oc->L, shndxTable);
                  secno = shndxTable[j];
               }
#endif
               /* Figure out if we want to add it; if so, set ad to its
                  address.  Otherwise leave ad == NULL. */

               bool common_already_defined = false;
               if (shndx == LL_ELF_SHN_COMMON) {
                   isLocal = false;
                   RtsSymbolInfo *existing = lookupStrHashTable(oc->L->symhash, nm);
                   if (existing != NULL) {
                       if (!ll_failure_alias(oc, existing->owner)) goto fail;
                       /* COMMON symbol already allocated by a previously-loaded
                        * object; reuse that address so relocations resolve to
                        * the same storage. */
                       if(symbol->elf_sym->st_size > existing->size) {
                           ll_error_set(oc->L, LL_E_DUPSYM,
                                   "linker: trying to link COMMON symbols %s with incompatible sizes: previous size %llu, new size %llu\n",
                                   nm,
                                   (long long unsigned int) existing->size,
                                   (long long unsigned int) symbol->elf_sym->st_size);
                           LL_LOG_ERROR_FOR(oc->L, "linker: trying to link COMMON symbols %s with incompatible sizes: previous size %llu, new size %llu\n",
                                   nm,
                                   (long long unsigned int) existing->size,
                                   (long long unsigned int) symbol->elf_sym->st_size);
                           goto fail;
                       }
                       symbol->addr = existing->value;
                       common_already_defined = true;
                       LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                                LL_DEBUG_FOR(oc->L, "COMMON symbol, size %llu name %s reusing address %p\n",
                                           (long long unsigned int) symbol->elf_sym->st_size, nm, symbol->addr));
                   } else {
                       LL_CHECK_FOR(oc->L, common_used <= common_size);
                       LL_CHECK_FOR(oc->L, common_mem != NULL ||
                                    symbol->elf_sym->st_size == 0);
                       /* A zero-sized COMMON-only image needs an address,
                        * but no separate allocation. Its aliases retain this
                        * image through the existing lifetime dependency. */
                       if (ll_noexec_enabled(oc->L))
                           symbol->addr = (void *)(oc->elf_info->common_exec + common_used);
                       else
                           symbol->addr = common_mem == NULL ? oc->image :
                               (void *)((uintptr_t)common_mem + common_used);
                       common_used += symbol->elf_sym->st_size;
                       LL_CHECK_FOR(oc->L, common_used <= common_size);
                       LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                                LL_DEBUG_FOR(oc->L, "COMMON symbol, size %llu name %s allocated at %p\n",
                                           (long long unsigned int) symbol->elf_sym->st_size, nm, symbol->addr));
                   }

                   /* Pointless to do addProddableBlock() for this area,
                      since the linker should never poke around in it. */
               } else if ((LL_ELF_ST_BIND(symbol->elf_sym->st_info) == LL_ELF_STB_GLOBAL
                           || LL_ELF_ST_BIND(symbol->elf_sym->st_info) == LL_ELF_STB_LOCAL
                           || LL_ELF_ST_BIND(symbol->elf_sym->st_info) == LL_ELF_STB_WEAK
                                                                  )
                          /* and not an undefined symbol */
                          && shndx != LL_ELF_SHN_UNDEF
                          /* and not in a "special section" */
                          && (shndx < LL_ELF_SHN_LORESERVE
#if LL_ELF_HAS_SHN_XINDEX
                                  || shndx == LL_ELF_SHN_XINDEX
#endif
                          )
                          &&
                          /* and it's a not a section or string table or
                           * anything silly */
                          (LL_ELF_ST_TYPE(symbol->elf_sym->st_info) == LL_ELF_STT_FUNC
                          || LL_ELF_ST_TYPE(symbol->elf_sym->st_info) == LL_ELF_STT_OBJECT
                          || LL_ELF_ST_TYPE(symbol->elf_sym->st_info) == LL_ELF_STT_NOTYPE
                          )
                       ) {
                   /* Section 0 is the undefined section, hence > and not >=. */
                   LL_CHECK_FOR(oc->L, secno > 0 && secno < shnum);
                   /*
                   if (shdr[secno].sh_type == SHT_NOBITS) {
                      debugBelch("   BSS symbol, size %d off %d name %s\n",
                                      stab[j].st_size, stab[j].st_value, nm);
                   }
                   */
                   if (ll_noexec_enabled(oc->L))
                       symbol->addr = (void *)(oc->sections[secno].exec_start +
                                               symbol->elf_sym->st_value);
                   else
                       symbol->addr = (ll_symbol_addr*)(
                               (intptr_t) oc->sections[secno].exec_start +
                               (intptr_t) symbol->elf_sym->st_value);
                   LL_CHECK_FOR(oc->L, symbol->addr != 0x0);
                   if (LL_ELF_ST_BIND(symbol->elf_sym->st_info) == LL_ELF_STB_LOCAL) {
                       isLocal = true;
                       isWeak = false;
                   } else { /* STB_GLOBAL or STB_WEAK */
                       LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                                LL_DEBUG_FOR(oc->L, "addOTabName(GLOB): %10p  %s %s\n",
                                           symbol->addr, oc->fileName, nm));
                       isLocal = false;
                       isWeak = LL_ELF_ST_BIND(symbol->elf_sym->st_info)
                                == LL_ELF_STB_WEAK;
                   }
               }

               ll_sym_type sym_type;
               if (LL_ELF_ST_TYPE(symbol->elf_sym->st_info) == LL_ELF_STT_FUNC) {
                   sym_type = LL_SYM_CODE;
               } else {
                   sym_type = LL_SYM_DATA;
               }
               if(LL_ELF_ST_VISIBILITY(symbol->elf_sym->st_other) == LL_ELF_STV_HIDDEN) {
                   sym_type |= LL_SYM_HIDDEN;
               }

               /* And the decision is ... */

               if (symbol->addr != NULL) {
                   LL_CHECK_FOR(oc->L, nm != NULL);
                   /* Acquire! */
                   if (!isLocal && !common_already_defined) {

                       if (isWeak == LL_HS_BOOL_TRUE) {
                           setWeakSymbol(oc, nm);
                       }
                       if (!ghciInsertSymbolTable(oc->L, oc->fileName,
                                                  nm, symbol->addr, isWeak, sym_type, symbol->elf_sym->st_size, oc)
                           ) {
                           goto fail;
                       }
                       oc->symbols[curSymbol].name = nm;
                       oc->symbols[curSymbol].addr = symbol->addr;
                       oc->symbols[curSymbol].type = sym_type;
                       curSymbol++;
                   }
               } else {
                   /* Skip. */
                   LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                            LL_DEBUG_FOR(oc->L, "skipping `%s'\n",
                                               nm)
                   );

                   /*
                   debugBelch(
                      "skipping   bind = %d,  type = %d,  secno = %d   `%s'\n",
                      (int)ELF_ST_BIND(stab[j].st_info),
                      (int)ELF_ST_TYPE(stab[j].st_info),
                      (int)secno,
                      nm
                   );
                   */
               }
           }
      }
   }

   if(makeGot( oc )) {
       if (oc->L->error_status == LL_OK)
           ll_error_set(oc->L, LL_E_NOMEM, "Failed to create GOT for %s",
                        OC_INFORMATIVE_FILENAME(oc));
       LL_LOG_ERROR_FOR(oc->L, "Failed to create GOT for %s",
                  oc->archiveMemberName
                  ? oc->archiveMemberName
                  : oc->fileName);
       goto fail;
   }
   result = 1;
   goto end;

section_no_memory:
   /* M32 may already have recorded a more specific allocation/state error. */
   if (oc->L->error_status == LL_OK) {
       ll_error_set(oc->L, LL_E_NOMEM,
                    "loadObj: could not allocate section storage for %" PATH_FMT,
                    OC_INFORMATIVE_FILENAME(oc));
   }
fail:
   result = 0;
   goto end;

end:
   if (fd >= 0) ll_backend_close_file(fd);
   return result;
}

// the aarch64 and riscv64 linkers use relocateObjectCodeAarch64() and
// relocateObjectCodeRISCV64() (respectively), see elf_reloc_aarch64.{h,c} and
// elf_reloc_riscv64.{h,c}
#if !LL_TARGET_AARCH64 && !LL_TARGET_RISCV64

/* Do ELF relocations which lack an explicit addend.  All x86-linux
   and arm-linux relocations appear to be of this form. */
static int
do_Elf_Rel_relocations ( ObjectCode* oc, char* ehdrC,
                         ll_elf_target_shdr* shdr, int shnum )
{
   int j;

   ll_elf_target_word* targ;
   ll_elf_target_rel*  rtab = (ll_elf_target_rel*) (ehdrC + shdr[shnum].sh_offset);

   int         nent = shdr[shnum].sh_size / sizeof(ll_elf_target_rel);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;

   ElfSymbolTable *stab = NULL;
   for(ElfSymbolTable * st = oc->elf_info->symbolTables;
       st != NULL; st = st->next) {
       if((int)st->index == symtab_shndx) {
           stab = st;
           break;
       }
   }
   LL_CHECK_FOR(oc->L, stab != NULL);

   targ  = (ll_elf_target_word*)oc->sections[target_shndx].start;
   LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L,
                "relocations for section %d using symtab %d\n",
                target_shndx, symtab_shndx));

   /* Skip sections that we're not interested in. */
   if (oc->sections[target_shndx].kind == SECTIONKIND_OTHER) {
       LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "skipping (target section not loaded)"));
       return 1;
   }

   /* The following nomenclature is used for the operation:
    * - S -- (when used on its own) is the address of the symbol.
    * - A -- is the addend for the relocation.
    * - P -- is the address of the place being relocated (derived from r_offset).
    * - Pa - is the adjusted address of the place being relocated, defined as (P & 0xFFFFFFFC).
    * - T -- is 1 if the target symbol S has type STT_FUNC and the symbol addresses a Thumb instruction; it is 0 otherwise.
    * - B(S) is the addressing origin of the output segment defining the symbol S. The origin is not required to be the
    *        base address of the segment. This value must always be word-aligned.
    * - GOT_ORG is the addressing origin of the Global Offset Table (the indirection table for imported data addresses).
    *        This value must always be word-aligned.  See §4.6.1.8, Proxy generating relocations.
    * - GOT(S) is the address of the GOT entry for the symbol S.
    *
    * See the ELF for "ARM Specification" for details:
    * https://developer.arm.com/architectures/system-architectures/software-standards/abi
    */

   for (j = 0; j < nent; j++) {
       ll_elf_target_addr offset = rtab[j].r_offset;
       ll_elf_target_addr info   = rtab[j].r_info;
       if (LL_ELF_R_TYPE(info) == 0) {
           continue;
       }

       ll_site site = {.work = (uint8_t *)targ + offset,
                       .exec = oc->sections[target_shndx].exec_start + offset};
#if LL_TARGET_I386 || LL_TARGET_ARM
       ll_elf_target_addr P = (ll_elf_target_addr)site.exec;
       uint8_t *pP = site.work;
#else
       ll_elf_target_addr P = ((ll_elf_target_addr)targ) + offset;
       ll_elf_target_word *pP = (ll_elf_target_word *)P;
       (void)site;
#endif
#if LL_TARGET_I386 || LL_DEBUG_CODE
       ll_elf_target_addr A;
#if LL_TARGET_ARM
       const size_t debug_width = (LL_ELF_R_TYPE(info) == COMPAT_R_ARM_THM_JUMP8 ||
                                   LL_ELF_R_TYPE(info) == COMPAT_R_ARM_THM_JUMP11)
                                ? 2 : sizeof(A);
       A = 0;
       memcpy(&A, site.work, debug_width);
#elif LL_TARGET_I386
       memcpy(&A, site.work, sizeof(A));
#else
       A = *pP;
#endif
#endif
       ll_elf_target_addr  S;
       void*     S_tmp;
#if LL_TARGET_ARM
       int is_target_thm=0, T=0;
#endif

       ElfSymbol * symbol = NULL;

       LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "Rel entry %3d is raw(%6p %6p): ",
                                   j, (void*)offset, (void*)info ));
       if (!info) {
           LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, " ZERO" ));
           S = 0;
       } else {
           symbol = &stab->symbols[LL_ELF_R_SYM(info)];
           /* First see if it is a local symbol. */
           if (LL_ELF_ST_BIND(symbol->elf_sym->st_info) == LL_ELF_STB_LOCAL || strncmp(symbol->name, "_GLOBAL_OFFSET_TABLE_", 21) == 0) {
               S = (ll_elf_target_addr)symbol->addr;
           } else {
               S_tmp = lookupDependentSymbol( oc->L, symbol->name, oc, NULL );
               S = (ll_elf_target_addr)S_tmp;
           }
           if (!S) {
               LL_LOG_ERROR_FOR(oc->L, "%s: unknown symbol `%s'",
                          oc->fileName, symbol->name);
               return 0;
           }
           LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "`%s' resolves to %p\n", symbol->name,
                                       (void*)S ));

#if LL_TARGET_ARM
           /*
            * 4.5.3 Symbol Values
            *
            * In addition to the normal rules for symbol values the following
            * rules shall also apply to symbols of type STT_FUNC:
            * - If the symbol addresses an ARM instruction, its value is the
            *   address of the instruction (in a relocatable object, the
            *   offset of the instruction from the start of the section
            *   containing it).
            * - If the symbol addresses a Thumb instruction, its value is the
            *   address of the instruction with bit zero set (in a relocatable
            *   object, the section offset with bit zero set).
            * - For the purposes of relocation the value used shall be the
            *   address of the instruction (st_value & ~1).
            *
            *  Note: This allows a linker to distinguish ARM and Thumb code
            *        symbols without having to refer to the map. An ARM symbol
            *        will always have an even value, while a Thumb symbol will
            *        always have an odd value. However, a linker should strip
            *        the discriminating bit from the value before using it for
            *        relocation.
            *
            * (source: ELF for the ARM Architecture
            *          ARM IHI 0044F, current through ABI release 2.10
            *          24th November 2015)
            */
           if(LL_ELF_ST_TYPE(symbol->elf_sym->st_info) == LL_ELF_STT_FUNC) {
               is_target_thm = S & 0x1;
               T = is_target_thm;
               S &= ~1;
           }
#endif
       }

       int reloc_type = LL_ELF_R_TYPE(info);
#if LL_TARGET_I386 || LL_DEBUG_CODE
       LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                LL_DEBUG_FOR(oc->L, "Reloc: P = %p   S = %p   A = %p   type=%d\n",
                           (void*)P, (void*)S, (void*)A, reloc_type ));
#endif
#if LL_DEBUG_CODE
#if LL_TARGET_ARM
       checkProddableBlock(oc->L, &oc->proddables, pP, debug_width);
#else
       checkProddableBlock(oc->L, &oc->proddables, pP, sizeof(ll_elf_target_word) );
#endif
#else
       (void) pP; /* suppress unused varialbe warning in non-debug build */
#endif

#if LL_TARGET_I386 || LL_TARGET_ARM
       uintptr_t got_slot = 0;
       if (symbol != NULL && symbol->got_addr != NULL)
           got_slot = oc->elf_info->got_exec +
               ((uintptr_t)symbol->got_addr - (uintptr_t)oc->elf_info->got_start);
       const ll_reloc_context context = {.symbol_index = LL_ELF_R_SYM(info),
           .symbol_name = symbol != NULL ? symbol->name : NULL,
           .format_symbol_type = symbol != NULL ? LL_ELF_ST_TYPE(symbol->elf_sym->st_info) : 0,
           .got_exec = got_slot};
#if LL_TARGET_I386
       if (!ll_elf_i386_reloc_ops.apply(oc, &oc->sections[target_shndx],
                                        reloc_type, site, S, A, LL_SYM_DATA, &context)) return 0;
#else
       if (!ll_elf_arm_reloc_ops.apply(oc, &oc->sections[target_shndx],
                                       reloc_type, site, S | T, 0, LL_SYM_DATA, &context)) return 0;
#endif
#else
       ll_error_set(oc->L, LL_E_UNSUPPORTED,
                    "%s: unhandled ELF relocation(Rel) type %" LL_FMT_WORD "\n",
                    oc->fileName, (ll_w)reloc_type);
       LL_LOG_ERROR_FOR(oc->L, "%s: unhandled ELF relocation(Rel) type %" LL_FMT_WORD "\n",
                        oc->fileName, (ll_w)reloc_type);
       return 0;
#endif

   }
   return 1;
}

/* Do ELF relocations for which explicit addends are supplied. */
static int
do_Elf_Rela_relocations ( ObjectCode* oc, char* ehdrC,
                          ll_elf_target_shdr* shdr, int shnum )
{
   int j;
   ll_symbol_name* symbol = NULL;
   ll_elf_target_rela* rtab = (ll_elf_target_rela*) (ehdrC + shdr[shnum].sh_offset);
   ll_elf_target_sym*  stab;
   char*     strtab;
   int         nent = shdr[shnum].sh_size / sizeof(ll_elf_target_rela);
   int symtab_shndx = shdr[shnum].sh_link;
   int strtab_shndx = shdr[symtab_shndx].sh_link;
   int target_shndx = shdr[shnum].sh_info;
#if LL_ELF_HAS_SHN_XINDEX
   ll_elf_target_word* shndx_table = get_shndx_table(oc);
#endif
#if LL_DEBUG_CODE || LL_TARGET_POWERPC || LL_TARGET_X86_64
   /* This #if def only serves to avoid unused-var warnings. */
   ll_elf_target_addr targ = (ll_elf_target_addr) oc->sections[target_shndx].start;
#endif

   stab  = (ll_elf_target_sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   strtab= (char*)    (ehdrC + shdr[ strtab_shndx ].sh_offset);

   LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "relocations for section %d using symtab %d\n",
                          target_shndx, symtab_shndx ));

   /* Skip sections that we're not interested in. */
   if (oc->sections[target_shndx].kind == SECTIONKIND_OTHER) {
           LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "skipping (target section not loaded)"));
           return 1;
   }

   for (j = 0; j < nent; j++) {
#if LL_DEBUG_CODE || LL_TARGET_POWERPC || LL_TARGET_X86_64
      /* This #if def only serves to avoid unused-var warnings. */
      ll_elf_target_addr  offset = rtab[j].r_offset;
      ll_elf_target_addr  P      = targ + offset;
      ll_elf_target_addr  A      = rtab[j].r_addend;
#endif
#if LL_TARGET_POWERPC
      ll_elf_target_addr  value;
#endif
      ll_elf_target_addr  info   = rtab[j].r_info;
      ll_elf_target_addr  S;
      void*     S_tmp;
#     if LL_TARGET_POWERPC
      ll_elf_target_sword delta;
#     endif

      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "Rel entry %3d is raw(%6p %6p %6p)   ",
                             j, (void*)offset, (void*)info,
                                (void*)A ));
      if (!info) {
         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, " ZERO" ));
         S = 0;
      } else {
         ll_elf_target_sym sym = stab[LL_ELF_R_SYM(info)];
         const uint32_t reloc_type = LL_ELF_R_TYPE(info);
         if (ll_noexec_enabled(oc->L) &&
             (reloc_type == COMPAT_R_X86_64_TLSGD ||
              reloc_type == COMPAT_R_X86_64_GOTTPOFF)) {
                ll_error_set(oc->L, LL_E_UNSUPPORTED,
                    reloc_type == COMPAT_R_X86_64_TLSGD
                    ? "ELF TLSGD is unavailable in NO_EXEC mode"
                    : "ELF GOTTPOFF is unavailable in NO_EXEC mode");
                return 0;
         }
         if (reloc_type == COMPAT_R_X86_64_TLSGD) {
            /*
             * No support for TLSGD variables *defined* by the object,
             * only references to *external* TLS variables in already
             * loaded shared objects (the executable, libc, ...) are
             * supported.  See Note [TLSGD relocation] in elf_tlsgd.c.
             */
            symbol = sym.st_name == 0 ? "(noname)" : strtab+sym.st_name;
            if (LL_ELF_ST_BIND(sym.st_info) == LL_ELF_STB_LOCAL
                || sym.st_value != 0 || sym.st_name == 0) {
                ll_error_set(oc->L, LL_E_UNSUPPORTED,
                             "%s: unsupported internal ELF TLSGD relocation for symbol `%s'",
                             oc->fileName, symbol);
                LL_LOG_ERROR_FOR(oc->L, "%s: unsupported internal ELF TLSGD relocation for"
                           " symbol `%s'", oc->fileName, symbol);
                return 0;
            }
#if LL_TARGET_X86_64 && defined(x86_64_HOST_ARCH) && defined(freebsd_HOST_OS)
            S = lookupTlsgdSymbol(symbol, LL_ELF_R_SYM(info), oc);
#else
            ll_error_set(oc->L, LL_E_UNSUPPORTED,
                         "%s: ELF TLSGD relocation for symbol `%s' not supported on the target platform",
                         oc->fileName, symbol);
            LL_LOG_ERROR_FOR(oc->L, "%s: ELF TLSGD relocation for symbol `%s'"
                       " not supported on the target platform",
                       oc->fileName, symbol);
            return 0;
#endif
         } else if (LL_ELF_ST_BIND(sym.st_info) == LL_ELF_STB_LOCAL) {
            /*
             * For local symbols, we can get the address directly from the ELF
             * symbol table.
             *
             * XXX: Is STB_LOCAL the right test here?  Should we instead be
             * checking whether the symbol is *defined* by the current object?
             * Defined globals also need relocation.  Perhaps the point is that
             * conflicts are resolved in favour of any prior definition, so we
             * must look at the accumulated symbol table instead (which has
             * already been updated with our global symbols by the time we get
             * here).
             */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            /* See Note [Many ELF Sections] */
            ll_elf_target_word secno = sym.st_shndx;
#if LL_ELF_HAS_SHN_XINDEX
            if (secno == LL_ELF_SHN_XINDEX) {
              secno = shndx_table[LL_ELF_R_SYM(info)];
            }
#endif
            if (secno == LL_ELF_SHN_ABS) {
                S = (ll_elf_target_addr)stab[LL_ELF_R_SYM(info)].st_value;
            } else if (secno < (ll_elf_target_word)oc->n_sections) {
                S = (ll_elf_target_addr)oc->sections[secno].exec_start
                    + stab[LL_ELF_R_SYM(info)].st_value;
            } else {
                LL_LOG_ERROR_FOR(oc->L, "%s: invalid section index %u for symbol `%s'",
                                 oc->fileName, (unsigned)secno, symbol);
                return 0;
            }
         } else {
            /* If not local, look up the name in our global table. */
            symbol = strtab + sym.st_name;
            S_tmp = lookupDependentSymbol( oc->L, symbol, oc, NULL );
            S = (ll_elf_target_addr)S_tmp;
         }
         if (!S) {
           LL_LOG_ERROR_FOR(oc->L, "%s: unknown symbol `%s'", oc->fileName, symbol);
           return 0;
         }
         LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "`%s' resolves to %p\n", symbol, (void*)S));
      }

#if LL_DEBUG_CODE
      LL_IF_DEBUG_FOR(oc->L, linker_verbose,LL_DEBUG_FOR(oc->L, "Reloc: P = %p   S = %p   A = %p\n",
                                         (void*)P, (void*)S, (void*)A ));
      checkProddableBlock(oc->L, &oc->proddables, (void*)P, sizeof(ll_elf_target_word));
#endif

#if LL_TARGET_POWERPC
      value = S + A;
#endif

#if LL_TARGET_X86_64
      const ll_reloc_context context = {
          .symbol_index = LL_ELF_R_SYM(info),
          .symbol_name = symbol,
          .format_symbol_type = info != 0
              ? LL_ELF_ST_TYPE(stab[LL_ELF_R_SYM(info)].st_info) : 0
      };
      const ll_site site = {.work = (uint8_t *)P,
          .exec = oc->sections[target_shndx].exec_start + offset};
      /* Existing ELF resolution does not request a normalized provider type.
       * Raw referencing-file type retains the original eligibility checks. */
      if (!ll_elf_x86_64_reloc_ops.apply(oc, &oc->sections[target_shndx],
              LL_ELF_R_TYPE(info), site, S, rtab[j].r_addend, (ll_sym_type)0,
              &context))
          return 0;
#else
      switch (LL_ELF_R_TYPE(info)) {
#        if LL_TARGET_POWERPC
         case LL_ELF_R_PPC_ADDR16_LO:
            *(ll_elf32_half*) P = value;
            break;

         case LL_ELF_R_PPC_ADDR16_HI:
            *(ll_elf32_half*) P = value >> 16;
            break;

         case LL_ELF_R_PPC_ADDR16_HA:
            *(ll_elf32_half*) P = (value + 0x8000) >> 16;
            break;

         case LL_ELF_R_PPC_ADDR32:
            *(ll_elf32_word *) P = value;
            break;

         case LL_ELF_R_PPC_REL32:
            *(ll_elf32_word *) P = value - P;
            break;

         case LL_ELF_R_PPC_PLTREL24:
            value -= 0x8000; /* See Note [.LCTOC1 in PPC PIC code] */
            LL_FALLTHROUGH;
         case LL_ELF_R_PPC_REL24:
            delta = value - P;

            if( delta << 6 >> 6 != delta )
            {
               value = (ll_elf_target_addr)(&ll_make_symbol_extra_powerpc( oc, LL_ELF_R_SYM(info), value )
                                        ->jumpIsland);
               delta = value - P;

               if( value == 0 || delta << 6 >> 6 != delta )
               {
                  ll_error_set(oc->L, LL_E_RELOC_RANGE,
                               "Unable to make SymbolExtra for #%d",
                               (int)LL_ELF_R_SYM(info));
                  LL_LOG_ERROR_FOR(oc->L, "Unable to make SymbolExtra for #%d",
                        LL_ELF_R_SYM(info) );
                  return 0;
               }
            }

            *(ll_elf_target_word *) P = (*(ll_elf_target_word *) P & 0xfc000003)
                                          | (delta & 0x3fffffc);
            break;

         case LL_ELF_R_PPC_REL16_LO:
            *(ll_elf32_half*) P = value - P;
            break;

         case LL_ELF_R_PPC_REL16_HI:
            *(ll_elf32_half*) P = (value - P) >> 16;
            break;

         case LL_ELF_R_PPC_REL16_HA:
            *(ll_elf32_half*) P = (value + 0x8000 - P) >> 16;
            break;
#        endif

         default:
            ll_error_set(oc->L, LL_E_UNSUPPORTED,
                         "%s: unhandled ELF relocation(RelA) type %" LL_FMT_WORD "\n",
                         oc->fileName, (ll_w)LL_ELF_R_TYPE(info));
            LL_LOG_ERROR_FOR(oc->L, "%s: unhandled ELF relocation(RelA) type %" LL_FMT_WORD "\n",
                  oc->fileName, (ll_w)LL_ELF_R_TYPE(info));
            return 0;
      }

#endif /* LL_TARGET_X86_64 */

   }
   return 1;
}
#endif /* !aarch64_HOST_ARCH */


static bool
ocMprotect_Elf( ObjectCode *oc )
{
    if (ll_noexec_enabled(oc->L)) return true;
    for(int i=0; i < oc->n_sections; i++) {
        Section *section = &oc->sections[i];
        if(section->size == 0) continue;
        switch (section->kind) {
        case SECTIONKIND_CODE_OR_RODATA:
            if (section->alloc != SECTION_M32) {
                // N.B. m32 handles protection of its allocations during
                // flushing.
                if (!ll_mprotect_for_linker(oc->L, section->mapped_start,
                                            section->mapped_size, MEM_READ_EXECUTE)) {
                    ll_failure_protection(oc);
                    return false;
                }
            }
            break;
        default:
            break;
        }
    }

    return true;
}

int
ocResolve_ELF ( ObjectCode* oc )
{
#if LL_TARGET_X86_64
    /* A supplied integer cannot describe the running process's TLS state.
     * Reject host-dependent forms before fillGot can attempt symbol lookup. */
    if (ll_noexec_enabled(oc->L)) {
        for (ElfRelocationATable *table = oc->elf_info->relaTable;
             table != NULL; table = table->next) {
            for (size_t i = 0; i < table->n_relocations; ++i) {
                const uint32_t type = LL_ELF_R_TYPE(table->relocations[i].r_info);
                if (type == COMPAT_R_X86_64_TLSGD || type == COMPAT_R_X86_64_GOTTPOFF) {
                    ll_error_set(oc->L, LL_E_UNSUPPORTED,
                        type == COMPAT_R_X86_64_TLSGD
                        ? "ELF TLSGD is unavailable in NO_EXEC mode"
                        : "ELF GOTTPOFF is unavailable in NO_EXEC mode");
                    return 0;
                }
            }
        }
    }
#endif
   char*     ehdrC = (char*)(oc->image);
   ll_elf_target_ehdr* ehdr  = (ll_elf_target_ehdr*) ehdrC;
   ll_elf_target_shdr* shdr  = (ll_elf_target_shdr*) (ehdrC + ehdr->e_shoff);
   const ll_elf_target_word shnum = elf_shnum(ehdr);

#if LL_ELF_HAS_SHN_XINDEX
    ll_elf_target_word* shndxTable = get_shndx_table(oc);
#endif

    /* resolve section symbols
     * these are special symbols that point to sections, and have no name.
     * Usually there should be one symbol for each text and data section.
     *
     * We need to resolve (assign addresses) to them, to be able to use them
     * during relocation.
     */
    for(ElfSymbolTable *symTab = oc->elf_info->symbolTables;
        symTab != NULL; symTab = symTab->next) {
        for (size_t i = 0; i < symTab->n_symbols; i++) {
            ElfSymbol *symbol = &symTab->symbols[i];
            if(LL_ELF_STT_SECTION == LL_ELF_ST_TYPE(symbol->elf_sym->st_info)) {
                /* NOTE: We assume that oc->sections corresponds to the
                 *       sections in the object file.  This is currently true,
                 *       and will stay true, unless we start to compress
                 *       oc->sections by not having an entry for sections we
                 *       are not interested in.
                 */


                /* See Note [Many ELF Sections] */
                /* Note that future checks for special SHN_* numbers should
                 * check the shndx variable, not the section number in secno.
                 * Sections with the real number in the SHN_LORESERVE..HIRESERVE
                 * range will have shndx SHN_XINDEX and a secno with one of the
                 * reserved values.
                 */
                ll_elf_target_word secno = symbol->elf_sym->st_shndx;
#if LL_ELF_HAS_SHN_XINDEX
                if (secno == LL_ELF_SHN_XINDEX) {
                    LL_CHECK_FOR(oc->L, shndxTable);
                    secno = shndxTable[i];
                }
#endif
                if (secno >= (ll_elf_target_word)oc->n_sections || oc->sections[secno].start == NULL) {
                    ll_error_set(oc->L, LL_E_FORMAT, "invalid section index %lu for section symbol", (unsigned long)secno);
                    return 0;
                }
                LL_CHECK_FOR(oc->L, symbol->elf_sym->st_name == 0);
                LL_CHECK_FOR(oc->L, symbol->elf_sym->st_value == 0);
                symbol->addr = (void *)oc->sections[secno].exec_start;
            }
        }
    }

    if(fillGot( oc ))
        return 0;
    /* silence warnings */
    (void) shnum;
    (void) shdr;

#if LL_TARGET_AARCH64 || LL_TARGET_RISCV64
    /* use new relocation design */
    if(relocateObjectCode( oc ))
        return 0;
#else
    /* Process the relocation sections. */
    for (ll_elf_target_word i = 0; i < shnum; i++) {
        if (shdr[i].sh_type == LL_ELF_SHT_REL) {
          bool ok = do_Elf_Rel_relocations ( oc, ehdrC, shdr, i );
          if (!ok)
              return ok;
        }
        else
        if (shdr[i].sh_type == LL_ELF_SHT_RELA) {
          bool ok = do_Elf_Rela_relocations ( oc, ehdrC, shdr, i );
          if (!ok)
              return ok;
        }
    }
#endif

    if (ll_noexec_enabled(oc->L)) return 1;

#if LL_TARGET_POWERPC
    ocFlushInstructionCache( oc );
#elif LL_TARGET_RISCV64
    /* New-style pseudo-polymorph (by architecture) call */
    flushInstructionCache( oc );
#endif

    return ocMprotect_Elf(oc);
}

/*
 * Note [Initializers and finalizers (ELF)]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The System V ABI describes a facility for allowing object code to mark
 * functions to be run at load time. These functions are known as
 * "initializers" (or "constructors"). Initializers are recorded in a section
 * marked with the DT_INIT tag (often with the name `.init`).
 *
 * There is also a similar mechanism for code to be run at unload time (e.g.
 * during program termination). These are known as finalizers and are collected
 * in `.fini` section.
 *
 * For more about how the code generator emits initializers and finalizers see
 * Note [Initializers and finalizers in Cmm] in GHC.Cmm.InitFini.
 *
 * See also: the "Initialization and Termination Functions" section of the
 * System V ABI.
 *
 * Note [GCC 6 init/fini section workaround]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The System V ABI specifies that .init_array and .fini_array sections should
 * be marked with the SHT_INIT_ARRAY/SHT_FINI_ARRAY section types. However, it
 * seems that GCC 6 (at least on i386) produces sections *named*
 * .init_array/.fini_array but marks them as SHT_PROGBITS. Consequently we need
 * to augment the usual section type check (which in an ideal world would be
 * sufficient) with a check looking at the section name to catch this case.
 */

// Run the constructors/initializers of an ObjectCode.
// Returns 1 on success.
// See Note [Initializers and finalizers (ELF)].
int ocRunInit_ELF( ObjectCode *oc )
{
    if (oc && oc->elf_info && oc->elf_info->init && !ll_noexec_enabled(oc->L)) {
        return runInit(oc->L, &oc->elf_info->init);
    }
    return true;
}

// Run the finalizers of an ObjectCode.
// Returns 1 on success.
// See Note [Initializers and finalizers (ELF)].
int ocRunFini_ELF( ObjectCode *oc )
{
    if (oc && oc->elf_info && oc->elf_info->fini && !ll_noexec_enabled(oc->L)) {
        return runFini(oc->L, &oc->elf_info->fini);
    }
    return true;
}

/*
 * PowerPC & X86_64 ELF specifics
 */

#if LL_ELF_NEED_SYMBOL_EXTRAS

static int refreshExtras_ELF(ObjectCode *oc)
{
  ocInit_ELF(oc);
  return 1;
}

int ocAllocateExtras_ELF( ObjectCode *oc )
{
  ll_elf_target_ehdr *ehdr = (ll_elf_target_ehdr *) oc->image;
  ll_elf_target_shdr* shdr = (ll_elf_target_shdr *) ( ((char *)oc->image) + ehdr->e_shoff );
  ll_elf_target_shdr* symtab = NULL;
  ll_elf_target_word shnum = elf_shnum(ehdr);
  int bssSize = 0;

  for (ll_elf_target_word i = 0; i < shnum; ++i) {
    if(shdr[i].sh_type == LL_ELF_SHT_SYMTAB) {
      symtab = &shdr[i];
    } else {
      int isBss = 0;
      getSectionKind_ELF(&shdr[i], &isBss);
      if (!ll_noexec_enabled(oc->L) && isBss && shdr[i].sh_size > 0) {
        ll_word align = shdr[i].sh_addralign == 0 ? 1 : shdr[i].sh_addralign;
        bssSize += ll_round_up_to_align(shdr[i].sh_size, align);
      }
    }
  }

  if (symtab == NULL)
  {
    // Not having a symbol table is not in principle a problem.
    // When an object file has no symbols then the 'strip' program
    // typically will remove the symbol table entirely.
    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "The ELF file %s contains no symtab\n",
             oc->archiveMemberName ? oc->archiveMemberName : oc->fileName ));
    return 1;
  }

  if( symtab->sh_entsize != sizeof( ll_elf_target_sym ) )
  {
    ll_error_set(oc->L, LL_E_FORMAT,
      "The entry size (%d) of the symtab isn't %d\n",
      (int) symtab->sh_entsize, (int) sizeof( ll_elf_target_sym ) );
    LL_LOG_ERROR_FOR(oc->L, "The entry size (%d) of the symtab isn't %d\n",
      (int) symtab->sh_entsize, (int) sizeof( ll_elf_target_sym ) );

    return 0;
  }

  return ocAllocateExtras(oc, symtab->sh_size / sizeof( ll_elf_target_sym ), 0, bssSize,
                          LL_TARGET_SYMBOL_EXTRA_SIZE, refreshExtras_ELF);
}

#endif /* NEED_SYMBOL_EXTRAS */

// Try interpreting an object which couldn't be loaded as a linker script and
// load the first object in the linker GROUP ( ... ) directive (see comment below).
//
// Receives the non-NULL error message outputted from an attempt to load an
// object (eg `loadNativeObj_POSIX` ).
//
// Returns the handle to the loaded object first mentioned in the linker script.
// If this process fails at any point, the function returns NULL and outputs a
// new error message.
#if LL_NATIVE_ELF
void * loadNativeObjFromLinkerScript_ELF(ll_linker *L, char **errmsg)
{
   // GHC #2615
   // On some systems (e.g., Gentoo Linux) dynamic files (e.g. libc.so)
   // contain linker scripts rather than ELF-format object code. This
   // code handles the situation by recognizing the real object code
   // file name given in the linker script.
   //
   // If an "invalid ELF header" error occurs, it is assumed that the
   // .so file contains a linker script instead of ELF object code.
   // In this case, the code looks for the GROUP ( ... ) linker
   // directive. If one is found, the first file name inside the
   // parentheses is treated as the name of a dynamic library and the
   // code attempts to dlopen that file. If this is also unsuccessful,
   // an error message is returned.

#define NMATCH 5
   regmatch_t match[NMATCH];
   FILE* fp;
   size_t match_length;
#define MAXLINE 1000
   char line[MAXLINE];
   int result;
   void* r = NULL;

   ll_assert_lock_held_for(L, &L->linker_mutex);

   // see if the error message is due to an invalid ELF header
   LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "errmsg = '%s'\n", *errmsg));
   result = regexec(&L->re_invalid, *errmsg, (size_t) NMATCH, match, 0);
   LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "result = %i\n", result));
   if (result == 0) {
      // success -- try to read the named file as a linker script
      match_length = (size_t) ll_min_intmax((match[1].rm_eo - match[1].rm_so),
                                 MAXLINE-1);
      strncpy(line, (*errmsg+(match[1].rm_so)),match_length);
      line[match_length] = '\0'; // make sure string is null-terminated
      LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "file name = '%s'\n", line));
      if ((fp = __rts_fopen(line, "r")) == NULL) {
         // return original error if open fails
         return NULL;
      }
      // try to find a GROUP or INPUT ( ... ) command
      while (fgets(line, MAXLINE, fp) != NULL) {
         LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "input line = %s", line));
         if (regexec(&L->re_realso, line, (size_t) NMATCH, match, 0) == 0) {
            // success -- try to dlopen the first named file
            LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "match%s\n",""));
            line[match[2].rm_eo] = '\0';
            LL_FREE_FOR(L, (void*)*errmsg); // Free old message before creating new one
            r = loadNativeObj_POSIX(L, line+match[2].rm_so, errmsg);
            break;
         }
         // if control reaches here, no GROUP or INPUT ( ... ) directive
         // was found and the original error message is returned to the
         // caller
      }
      fclose(fp);
   }

   return r;
}

#endif /* LL_NATIVE_ELF */

static int ll_elf_init(ObjectCode *oc)
{
    /* Preserve the existing pre-init verification and the loadOc recheck. */
    if (!ocVerifyImage_ELF(oc)) return 0;
    ocInit_ELF(oc);
    return 1;
}
static int ll_elf_extras(ObjectCode *oc)
{
#if LL_ELF_NEED_SYMBOL_EXTRAS
    return ocAllocateExtras_ELF(oc);
#else
    (void)oc;
    return 1;
#endif
}
const struct ll_format_ops ll_format_elf = {
    .name = "ELF", .sniff = ll_format_sniff_elf,
    .verify = ocVerifyImage_ELF, .init = ll_elf_init,
    .alloc_extras = ll_elf_extras, .get_names = ocGetNames_ELF,
    .resolve = ocResolve_ELF, .run_init = ocRunInit_ELF,
    .run_fini = ocRunFini_ELF, .deinit = ocDeinit_ELF
};

#endif /* elf */
