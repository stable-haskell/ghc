/* Check the static-symbol relocation path using the real PE resolver. */
#define main archive_status_original_main
#include "archive_error_status.c"
#undef main
#include "Linker.c"
#include "linker/PEi386.h"

static struct {
    ll_symbol_extra_x86_64 extra;
    uint32_t field;
} storage;
static m32_allocator *selected_allocator;
static void *static_extra_alloc(ll_linker *owner, m32_allocator *allocator,
                                size_t size, size_t alignment)
{
    (void)owner;
    CHECK(size == sizeof(storage.extra) && alignment == 8,
          "unexpected extra allocation request");
    selected_allocator = allocator;
    return &storage.extra;
}
#define m32_alloc_for static_extra_alloc
#include "linker/PEi386.c"
#undef m32_alloc_for

static void run_static_case(ll_linker *owner, uint16_t symbol_type,
                            m32_allocator *expected_allocator)
{
    ObjectCode object = {0};
    Section sections[3] = {0};
    struct ll_coff_object_info *info = LL_CALLOC_FOR(owner, 1, sizeof(*info), "test info");
    COFF_HEADER_INFO *header = LL_CALLOC_FOR(owner, 1, sizeof(*header), "test header");
    COFF_symbol *symbol = LL_CALLOC_FOR(owner, 1, sizeof(*symbol), "test symbol");
    struct ll_coff_section_info *section_info = LL_CALLOC_FOR(owner, 1, sizeof(*section_info), "test section");
    COFF_reloc *reloc = LL_CALLOC_FOR(owner, 1, sizeof(*reloc), "test relocation");
    if (!info || !header || !symbol || !section_info || !reloc) {
        CHECK(false, "fixture allocation failed");
        return;
    }
    memset(&storage, 0, sizeof(storage));
    selected_allocator = NULL;
    object.L = owner;
    object.fileName = (pathchar *)"static-extra";
    object.coff_info = info;
    object.sections = sections;
    object.n_sections = 3;
    object.rw_m32 = (m32_allocator *)(uintptr_t)1;
    object.rx_m32 = (m32_allocator *)(uintptr_t)2;
    info->ch_info = header;
    info->symbols = symbol;
    header->type = COFF_IMAGE;
    header->numberOfSections = 1;
    header->numberOfSymbols = 1;
    symbol->og.StorageClass = LL_COFF_IMAGE_SYM_CLASS_STATIC;
    symbol->og.SectionNumber = 2;
    symbol->og.Type = symbol_type;
    memcpy(symbol->og.N.ShortName, "static", sizeof("static"));
    sections[0].start = &storage.field;
    sections[0].exec_start = (uintptr_t)&storage.field;
    sections[0].kind = SECTIONKIND_RWDATA;
    sections[0].coff_info = section_info;
    section_info->relocs = reloc;
    section_info->noRelocs = 1;
    reloc->Type = 4;
    uintptr_t target = (uintptr_t)&storage.field + UINT64_C(0x200000000);
    sections[1].start = (void *)target;
    sections[1].exec_start = target;
    initProddableBlockSet(&object.proddables);
    addProddableBlock(owner, &object.proddables, &storage.field, sizeof(storage.field));
    ll_error_clear(owner);
    CHECK(ocResolve_PEi386(&object), "static relocation did not resolve");
    CHECK(selected_allocator == expected_allocator, "static extra selected wrong allocator");
    CHECK(storage.extra.addr == target, "static extra did not retain target");
    if (symbol_type == 0x20) {
        const uint8_t jump[] = {0xff, 0x25, 0xf2, 0xff, 0xff, 0xff};
        CHECK(memcmp(storage.extra.jumpIsland, jump, sizeof(jump)) == 0,
              "static code extra has wrong jump bytes");
        CHECK(storage.field == (uint32_t)((uintptr_t)storage.extra.jumpIsland -
                                          (uintptr_t)&storage.field - 4),
              "static code relocation used the wrong destination");
    } else {
        CHECK(storage.field == (uint32_t)((uintptr_t)&storage.extra.addr -
                                          (uintptr_t)&storage.field - 4),
              "static data relocation used the wrong destination");
    }
    freeProddableBlocks(owner, &object.proddables);
    releaseOcInfo(&object);
}

int main(void)
{
    ll_linker owner = {0};
    ll_compat_host_ops ops = make_ops(&host_a);
    ll_compat_config config = {.always_pic = true};
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    run_static_case(&owner, 0, (m32_allocator *)(uintptr_t)1);
    run_static_case(&owner, 0x20, (m32_allocator *)(uintptr_t)2);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    CHECK(host_a.live == 0, "static extra fixture leaked ownership");
    return failures != 0;
}
