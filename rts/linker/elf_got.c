#include "Rts.h"
#include "elf_got.h"
#include "linker/Elf.h"
#include "linker/MMap.h"

#include <string.h>

#if defined(OBJFORMAT_ELF)
/*
 * Check if we need a global offset table slot for a
 * given symbol
 */
bool
needGotSlot(Elf_Sym * symbol) {
    /* Any symbol that might be referenced via a GOT relocation needs a slot.
     * STB_LOCAL symbols can have GOT-relative relocations in some toolchains
     * (e.g. Android NDK's libm.a has local data symbols like approx_tab
     * referenced via R_AARCH64_ADR_GOT_PAGE).
     */
    /* Include named local object/func symbols — some toolchains
     * (e.g. Android NDK) generate GOT-relative relocations for local
     * data symbols like approx_tab in libm.a. Exclude unnamed symbols,
     * STT_FILE symbols (source filenames), and STT_NOTYPE symbols. */
    return ELF_ST_BIND(symbol->st_info) == STB_GLOBAL
        || ELF_ST_BIND(symbol->st_info) == STB_WEAK
        || (ELF_ST_BIND(symbol->st_info) == STB_LOCAL
            && symbol->st_name != 0
            && ELF_ST_TYPE(symbol->st_info) != STT_FILE
            && ELF_ST_TYPE(symbol->st_info) != STT_NOTYPE)
        // Section symbols exist primarily for relocation
        // and as such may need a GOT slot.
        || ELF_ST_TYPE(symbol->st_info) == STT_SECTION;

}

bool
makeGot(ObjectCode * oc) {
    size_t got_slots = 0;

    /* we need to find all symbol tables (elf can have multiple)
     * and need to iterate over all symbols, to check how many
     * got slots we need at most
     */
    ASSERT( oc->info != NULL );
    ASSERT( oc->info->sectionHeader != NULL );
    for(int i=0; i < oc->n_sections; i++) {
        if(SHT_SYMTAB == oc->info->sectionHeader[i].sh_type) {
            Elf_Sym *symTab =
                (Elf_Sym*)((uint8_t*)oc->info->elfHeader
                                   + oc->info->sectionHeader[i].sh_offset);
            size_t n_symbols = oc->info->sectionHeader[i].sh_size
                               / sizeof(Elf_Sym);
            for(size_t j=0; j < n_symbols; j++) {
                if(needGotSlot(&symTab[j])) {
                    got_slots += 1;
                }
            }
        }
    }
    if(got_slots > 0) {
        oc->info->got_size = got_slots * sizeof(void *);
#if USE_LINKER_POOL
        void * mem = linkerPoolAllocGot(oc->info->got_size);
#else
        void * mem = mmapAnonForLinker(oc->info->got_size);
#endif
        if (mem == NULL) {
            errorBelch("makeGot: allocation failed (size=%zu)", oc->info->got_size);
            return EXIT_FAILURE;
        }

        oc->info->got_start = (void*)mem;
        /* update got_addr */
        size_t slot = 0;
        for(ElfSymbolTable *symTab = oc->info->symbolTables;
            symTab != NULL; symTab = symTab->next) {

            for(size_t i=0; i < symTab->n_symbols; i++)
                if(needGotSlot(symTab->symbols[i].elf_sym))
                    symTab->symbols[i].got_addr
                            = (uint8_t *)oc->info->got_start
                              + (slot++ * sizeof(void*));
        }
    }
    return EXIT_SUCCESS;
}

bool
fillGot(ObjectCode * oc) {
    /* fill the GOT table */
    for(ElfSymbolTable *symTab = oc->info->symbolTables;
        symTab != NULL; symTab = symTab->next) {

        for(size_t i=0; i < symTab->n_symbols; i++) {
            ElfSymbol * symbol = &symTab->symbols[i];

            if(needGotSlot(symbol->elf_sym)) {

                /* no type are undefined symbols */
                // Note STT_SECTION symbols should have their address
                // set prior to the fillGot call in ocResolve_ELF.
                if(   STT_NOTYPE == ELF_ST_TYPE(symbol->elf_sym->st_info)
                   || STB_WEAK   == ELF_ST_BIND(symbol->elf_sym->st_info)) {
                    if(0x0 == symbol->addr) {
                        symbol->addr = lookupDependentSymbol(symbol->name, oc, NULL);
                        if(0x0 == symbol->addr) {
                            if(0 == strncmp(symbol->name,"_GLOBAL_OFFSET_TABLE_",21)) {
                                symbol->addr = oc->info->got_start;
                            } else {
                                errorBelch("Failed to lookup symbol: %s,"
                                           " you might consider using --optimistic-linking\n",
                                           symbol->name);

                                // if --optimistic-linking is passed into the
                                // RTS we allow the linker to optimistically
                                // continue
                                if (RtsFlags.MiscFlags.linkerOptimistic) {
                                    errorBelch("Failed to lookup symbol: %s,"
                                               " optimistically continuing.\n",
                                               symbol->name);
                                    symbol->addr = (void*) 0xDEADBEEF;
                                } else {
                                    return EXIT_FAILURE;
                                }

                            }
                        }
                    } else {
                        // we already have the address.
                    }
                } /* else it was defined somewhere in the same object, and
                  * we should have the address already.
                  */

                if(0x0 == symbol->addr) {
                    errorBelch(
                        "Something went wrong! Symbol %s has null address.\n",
                            symbol->name);
                    return EXIT_FAILURE;
                }

                if(0x0 == symbol->got_addr) {
                    errorBelch("Not good either!");
                    return EXIT_FAILURE;
                }

                *(void**)symbol->got_addr = symbol->addr;
            }
        }
    }

#if USE_LINKER_POOL
    // When using the linker pool, we cannot mprotect individual
    // sub-allocations. Protection is handled by linkerPoolProtect().
#else
    // We are done initializing the GOT; freeze it.
    if(mprotect(oc->info->got_start, oc->info->got_size, PROT_READ) != 0) {
        sysErrorBelch("unable to protect memory");
    }
#endif
    return EXIT_SUCCESS;
}

bool
verifyGot(ObjectCode * oc) {
    for(ElfSymbolTable *symTab = oc->info->symbolTables;
        symTab != NULL; symTab = symTab->next) {
        for(size_t i=0; i < symTab->n_symbols; i++) {
            ElfSymbol * symbol = &symTab->symbols[i];
            if(symbol->got_addr) {
                CHECK((void*)(*(void**)symbol->got_addr)
                      == (void*)symbol->addr);
            }
            CHECK(0 == ((uintptr_t)symbol->addr & 0xffff000000000000));
        }
    }
    return EXIT_SUCCESS;
}

void
freeGot(ObjectCode * oc) {
//    munmapForLinker(oc->info->got_start, oc->info->got_size, "freeGot);
    oc->info->got_start = 0x0;
    oc->info->got_size = 0;
}
#endif
