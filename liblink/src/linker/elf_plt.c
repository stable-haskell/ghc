#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "elf_plt.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#if LL_TARGET_ARM || LL_TARGET_AARCH64 || LL_TARGET_RISCV64
#if LL_BUILD_ELF

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define _makeStub       ADD_SUFFIX(elf_makeStub)
#define elf_needStubForRel  ADD_SUFFIX(elf_needStubForRel)
#define elf_needStubForRela ADD_SUFFIX(elf_needStubForRela)

unsigned
elf_numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex) {
    unsigned n = 0;
    for(ElfRelocationTable *t = oc->elf_info->relTable; t != NULL; t = t->next)
        if(t->targetSectionIndex == sectionIndex)
            for(size_t i=0; i < t->n_relocations; i++)
                if(elf_needStubForRel(&t->relocations[i]))
                    n += 1;

    for(ElfRelocationATable *t = oc->elf_info->relaTable; t != NULL; t = t->next)
        if(t->targetSectionIndex == sectionIndex)
            for(size_t i=0; i < t->n_relocations; i++)
                if(elf_needStubForRela(&t->relocations[i]))
                    n += 1;
    return n;
}

bool
elf_findStub(Section * section,
          void* * addr,
          uint8_t flags) {
    for(ElfStub * s = section->elf_info->stubs; s != NULL; s = s->next) {
        if(   s->target == *addr
           && s->flags  == flags) {
            *addr = s->addr;
            return EXIT_SUCCESS;
        }
    }
    return EXIT_FAILURE;
}

bool
elf_makeStub(ll_linker *L, Section * section,
          void* * addr,
          void* got_addr,
          uint8_t flags) {

    ElfStub * s = LL_CALLOC_FOR(L, 1, sizeof(ElfStub), "makeStub");
    LL_ASSERT_FOR(L, s != NULL);
    s->target = *addr;
    s->got_addr = got_addr;
    s->flags  = flags;
    s->next = NULL;
    s->addr = (uint8_t *)section->elf_info->stub_offset + STUB_SIZE * section->elf_info->nstubs;

#if LL_TARGET_RISCV64
    if((*_makeStub)(L, s))
#else
    if((*_makeStub)(s))
#endif
        return EXIT_FAILURE;

    if(section->elf_info->stubs == NULL) {
        LL_ASSERT_FOR(L, section->elf_info->nstubs == 0);
        /* no stubs yet, let's just create this one */
        section->elf_info->stubs = s;
    } else {
        ElfStub * tail = section->elf_info->stubs;
        while(tail->next != NULL) tail = tail->next;
        tail->next = s;
    }
    section->elf_info->nstubs += 1;
    *addr = s->addr;
    return EXIT_SUCCESS;
}

/* Keep the original emitter and list operations above as the single native
 * implementation. Only the returned address gains a separate execution view. */
static ll_site
stubSite(const Section *section, void *work)
{
    const uintptr_t offset = (uintptr_t)work - (uintptr_t)section->start;
    return (ll_site){.work = work, .exec = section->exec_start + offset};
}

bool
elf_findStubSite(Section *section, void *target, uint8_t flags, ll_site *site)
{
    void *work = target;
    if (elf_findStub(section, &work, flags)) return EXIT_FAILURE;
    *site = stubSite(section, work);
    return EXIT_SUCCESS;
}

bool
elf_makeStubSite(ll_linker *L, Section *section, void *target, void *got_addr,
                 uint8_t flags, ll_site *site)
{
    void *work = target;
    if (elf_makeStub(L, section, &work, got_addr, flags)) return EXIT_FAILURE;
    *site = stubSite(section, work);
    return EXIT_SUCCESS;
}

void
elf_freeStubs(ll_linker *L, Section * section) {
    if(section->elf_info->nstubs == 0)
        return;
    ElfStub * last = section->elf_info->stubs;
    while(last->next != NULL) {
        ElfStub * t = last;
        last = last->next;
        LL_FREE_FOR(L, t);
    }
    LL_FREE_FOR(L, last);
    section->elf_info->stubs = NULL;
    section->elf_info->nstubs = 0;
}

#endif // OBJECTFORMAT_ELF
#endif // arm/aarch64_HOST_ARCH
