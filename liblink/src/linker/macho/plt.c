#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "plt.h"
#include <string.h>

#if LL_TARGET_AARCH64

#if LL_BUILD_MACHO


#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define _makeStub       ADD_SUFFIX(macho_makeStub)
#define macho_needStubForRel  ADD_SUFFIX(macho_needStubForRel)

unsigned
macho_numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex) {
    unsigned n = 0;

    MachOSection *section = &oc->macho_info->macho_sections[sectionIndex];
    const char *relocations = oc->image + section->reloff;
    if(section->size > 0) {
        for(size_t i = 0; i < section->nreloc; i++) {
            MachORelocationInfo relocation;
            memcpy(&relocation, relocations + i * sizeof(relocation),
                   sizeof(relocation));
            if(macho_needStubForRel(&relocation))
                n += 1;
        }
    }

    return n;
}

bool
macho_findStub(Section * section,
          void* * addr,
          uint8_t flags) {
    for(MachOStub * s = section->macho_info->stubs; s != NULL; s = s->next) {
        if(   s->target == *addr
           && s->flags  == flags) {
            *addr = s->addr;
            return EXIT_SUCCESS;
        }
    }
    return EXIT_FAILURE;
}

bool
macho_makeStub(ll_linker *L, Section * section,
          void* * addr,
          uint8_t flags) {

    if (section == NULL || section->macho_info == NULL ||
        section->macho_info->stub_offset == NULL ||
        section->macho_info->nstubs >= section->macho_info->stub_size / STUB_SIZE) {
        ll_error_set(L, LL_E_RELOC_RANGE, "Mach-O branch stub space is unavailable");
        return EXIT_FAILURE;
    }
    MachOStub * s = (MachOStub *)LL_CALLOC_FOR(L, sizeof(MachOStub), 1, "makeStub");
    if (s == NULL) {
        ll_error_set(L, LL_E_NOMEM, "cannot allocate Mach-O branch stub metadata");
        return EXIT_FAILURE;
    }
    s->target = *addr;
    s->flags  = flags;
    s->next = NULL;
    s->addr = (uint8_t *)section->macho_info->stub_offset + STUB_SIZE * section->macho_info->nstubs;

    if((*_makeStub)(s)) {
        LL_FREE_FOR(L, s);
        ll_error_set(L, LL_E_RELOC_RANGE, "cannot build Mach-O branch stub");
        return EXIT_FAILURE;
    }

    if(section->macho_info->stubs == NULL) {
        LL_CHECK_FOR(L, section->macho_info->nstubs == 0);
        /* no stubs yet, let's just create this one */
        section->macho_info->stubs = s;
    } else {
        MachOStub * tail = section->macho_info->stubs;
        while(tail->next != NULL) tail = tail->next;
        tail->next = s;
    }
    section->macho_info->nstubs += 1;
    *addr = s->addr;
    return EXIT_SUCCESS;
}

static ll_site
macho_stubSite(const Section *section, void *work)
{
    const uintptr_t offset = (uintptr_t)work - (uintptr_t)section->start;
    return (ll_site){.work = work, .exec = section->exec_start + offset};
}

bool
macho_findStubSite(Section *section, void *target, uint8_t flags, ll_site *site)
{
    void *work = target;
    if (macho_findStub(section, &work, flags)) return EXIT_FAILURE;
    *site = macho_stubSite(section, work);
    return EXIT_SUCCESS;
}

bool
macho_makeStubSite(ll_linker *L, Section *section, void *target,
                   uint8_t flags, ll_site *site)
{
    void *work = target;
    if (macho_makeStub(L, section, &work, flags)) return EXIT_FAILURE;
    *site = macho_stubSite(section, work);
    return EXIT_SUCCESS;
}

void
macho_freeStubs(ll_linker *L, Section * section) {
    if(NULL == section || section->macho_info == NULL)
        return;
    MachOStub * stub = section->macho_info->stubs;
    while(stub != NULL) {
        MachOStub * next = stub->next;
        LL_FREE_FOR(L, stub);
        stub = next;
    }
    section->macho_info->stubs = NULL;
    section->macho_info->nstubs = 0;
}

#endif // OBJECTFORMAT_MACHO
#endif // aarch64_HOST_ARCH
