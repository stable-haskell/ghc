#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "elf_plt.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#if defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH) || defined(riscv64_HOST_ARCH)
#if defined(OBJFORMAT_ELF)

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define _makeStub       ADD_SUFFIX(makeStub)
#define needStubForRel  ADD_SUFFIX(needStubForRel)
#define needStubForRela ADD_SUFFIX(needStubForRela)

unsigned
numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex) {
    unsigned n = 0;
    for(ElfRelocationTable *t = oc->info->relTable; t != NULL; t = t->next)
        if(t->targetSectionIndex == sectionIndex)
            for(size_t i=0; i < t->n_relocations; i++)
                if(needStubForRel(&t->relocations[i]))
                    n += 1;

    for(ElfRelocationATable *t = oc->info->relaTable; t != NULL; t = t->next)
        if(t->targetSectionIndex == sectionIndex)
            for(size_t i=0; i < t->n_relocations; i++)
                if(needStubForRela(&t->relocations[i]))
                    n += 1;
    return n;
}

bool
findStub(Section * section,
          void* * addr,
          uint8_t flags) {
    for(Stub * s = section->info->stubs; s != NULL; s = s->next) {
        if(   s->target == *addr
           && s->flags  == flags) {
            *addr = s->addr;
            return EXIT_SUCCESS;
        }
    }
    return EXIT_FAILURE;
}

bool
makeStub(ll_linker *L, Section * section,
          void* * addr,
          void* got_addr,
          uint8_t flags) {

    Stub * s = LL_CALLOC_FOR(L, 1, sizeof(Stub), "makeStub");
    LL_ASSERT_FOR(L, s != NULL);
    s->target = *addr;
    s->got_addr = got_addr;
    s->flags  = flags;
    s->next = NULL;
    s->addr = (uint8_t *)section->info->stub_offset + STUB_SIZE * section->info->nstubs;

#if defined(riscv64_HOST_ARCH)
    if((*_makeStub)(L, s))
#else
    if((*_makeStub)(s))
#endif
        return EXIT_FAILURE;

    if(section->info->stubs == NULL) {
        LL_ASSERT_FOR(L, section->info->nstubs == 0);
        /* no stubs yet, let's just create this one */
        section->info->stubs = s;
    } else {
        Stub * tail = section->info->stubs;
        while(tail->next != NULL) tail = tail->next;
        tail->next = s;
    }
    section->info->nstubs += 1;
    *addr = s->addr;
    return EXIT_SUCCESS;
}

void
freeStubs(ll_linker *L, Section * section) {
    if(section->info->nstubs == 0)
        return;
    Stub * last = section->info->stubs;
    while(last->next != NULL) {
        Stub * t = last;
        last = last->next;
        LL_FREE_FOR(L, t);
    }
    section->info->stubs = NULL;
    section->info->nstubs = 0;
}

#endif // OBJECTFORMAT_ELF
#endif // arm/aarch64_HOST_ARCH
