#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "plt.h"
#include <string.h>

#if defined(aarch64_HOST_ARCH)

#if defined(OBJFORMAT_MACHO)

#include <mach/machine.h>
#include <mach-o/fat.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define _makeStub       ADD_SUFFIX(makeStub)
#define needStubForRel  ADD_SUFFIX(needStubForRel)

unsigned
numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex) {
    unsigned n = 0;

    MachOSection *section = &oc->info->macho_sections[sectionIndex];
    const char *relocations = oc->image + section->reloff;
    if(section->size > 0) {
        for(size_t i = 0; i < section->nreloc; i++) {
            MachORelocationInfo relocation;
            memcpy(&relocation, relocations + i * sizeof(relocation),
                   sizeof(relocation));
            if(needStubForRel(&relocation))
                n += 1;
        }
    }

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
          uint8_t flags) {

    Stub * s = (Stub *)LL_CALLOC_FOR(L, sizeof(Stub), 1, "makeStub");
    if (s == NULL) {
        ll_error_set(L, LL_E_NOMEM, "cannot allocate Mach-O branch stub metadata");
        return EXIT_FAILURE;
    }
    s->target = *addr;
    s->flags  = flags;
    s->next = NULL;
    s->addr = (uint8_t *)section->info->stub_offset + STUB_SIZE * section->info->nstubs;

    if((*_makeStub)(s)) {
        LL_FREE_FOR(L, s);
        ll_error_set(L, LL_E_RELOC_RANGE, "cannot build Mach-O branch stub");
        return EXIT_FAILURE;
    }

    if(section->info->stubs == NULL) {
        LL_CHECK_FOR(L, section->info->nstubs == 0);
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
    if(NULL == section || section->info == NULL)
        return;
    Stub * stub = section->info->stubs;
    while(stub != NULL) {
        Stub * next = stub->next;
        LL_FREE_FOR(L, stub);
        stub = next;
    }
    section->info->stubs = NULL;
    section->info->nstubs = 0;
}

#endif // OBJECTFORMAT_MACHO
#endif // aarch64_HOST_ARCH
