/* Exercise ARM64 COFF relocations through the production apply and resolver. */
#define main archive_status_original_main
#include "archive_error_status.c"
#undef main
#include "Linker.c"
#include "linker/PEi386.c"

static void put32(uint8_t *p, uint32_t value) { memcpy(p, &value, sizeof(value)); }
static uint32_t get32(const uint8_t *p) { uint32_t v; memcpy(&v, p, sizeof(v)); return v; }

static void check_apply(ll_linker *L, ObjectCode *oc, Section *section,
                        COFF_symbol *sym, uint16_t type, uint32_t insn,
                        uintptr_t S, uintptr_t P, uint32_t expected)
{
    uint8_t bytes[8] = {0};
    put32(bytes + 1, insn);
    ll_site site = {.work = bytes + 1, .exec = P};
    ll_error_clear(L);
    bool ok = pe_arm64_reloc(oc, section, sym, "test", type, site, S);
    if (!ok) fprintf(stderr, "type %u: %s\n", type, L->error_message);
    CHECK(ok, "relocation failed");
    if (get32(bytes + 1) != expected) fprintf(stderr, "type %u: got %08x expected %08x\n", type, get32(bytes + 1), expected);
    CHECK(get32(bytes + 1) == expected, "relocation bits differ");
    CHECK(bytes[0] == 0 && bytes[5] == 0, "relocation wrote outside its field");
}

static void check_range(ll_linker *L, ObjectCode *oc, Section *section,
                        COFF_symbol *sym, uint16_t type, uint32_t insn,
                        uintptr_t S, uintptr_t P)
{
    uint8_t bytes[8] = {0};
    put32(bytes + 1, insn);
    ll_site site = {.work = bytes + 1, .exec = P};
    ll_error_clear(L);
    CHECK(!pe_arm64_reloc(oc, section, sym, "test", type, site, S) &&
          L->error_status == LL_E_RELOC_RANGE, "invalid relocation was not rejected");
    CHECK(get32(bytes + 1) == insn, "failed relocation changed its field");
}

static void test_object(ll_linker *owner, const char *path)
{
    FILE *input = fopen(path, "rb");
    CHECK(input != NULL, "could not open ARM64 COFF fixture");
    if (!input || fseek(input, 0, SEEK_END) != 0) return;
    long length = ftell(input);
    if (length <= 0 || length > 1024 * 1024 || fseek(input, 0, SEEK_SET) != 0) {
        CHECK(false, "invalid ARM64 fixture length");
        return;
    }
    char *image = LL_MALLOC_FOR(owner, (size_t)length, "ARM64 fixture image");
    CHECK(fread(image, 1, (size_t)length, input) == (size_t)length && fclose(input) == 0,
          "could not read ARM64 fixture");
    ObjectCode *object = mkOc(owner, STATIC_OBJECT, (pathchar *)path, image,
                              (int)length, false, NULL, 0);
    if (!object || !ll_init_object(object) || !object->format_ops->verify(object) ||
        !object->format_ops->get_names(object)) {
        fprintf(stderr, "ARM64 COFF preparation: %s\n", owner->error_message);
        CHECK(false, "ARM64 COFF fixture did not load");
        return;
    }
    CHECK(object->format_kind == LL_OBJECT_FORMAT_COFF, "ARM64 format identity differs");
    int count = object->n_sections;
    CHECK(count < 128, "too many ARM64 fixture sections");
    Section *text_section = NULL;
    for (int i = 0; i < count && i < 128; ++i) {
        Section *section = &object->sections[i];
        if (!section->coff_info) continue;
        if (section->kind != SECTIONKIND_DEBUG && section->kind != SECTIONKIND_OTHER &&
            !(section->coff_info->props & (LL_COFF_IMAGE_SCN_MEM_DISCARDABLE | LL_COFF_IMAGE_SCN_LNK_REMOVE)) &&
            section->start && section->size) {
            CHECK(section->alloc == SECTION_NOEXEC && section->exec_start != (uintptr_t)section->start,
                  "ARM64 section does not use synthetic storage");
        }
        if (section->coff_info->name && strcmp(section->coff_info->name, ".text") == 0)
            text_section = section;
    }
    CHECK(ghciInsertSymbolTable(owner, (pathchar *)"fixture", "arm64_target",
          (void *)(uintptr_t)0x40000000, false, LL_SYM_CODE, 0, NULL), "target insertion failed");
    CHECK(ghciInsertSymbolTable(owner, (pathchar *)"fixture", "arm64_data",
          (void *)(uintptr_t)0x10010000, false, LL_SYM_DATA, 0, NULL), "data insertion failed");
    bool resolved = object->format_ops->resolve(object);
    if (!resolved) fprintf(stderr, "ARM64 COFF resolve: %s\n", owner->error_message);
    CHECK(resolved, "ARM64 COFF fixture did not resolve");
    if (resolved && text_section && text_section->coff_info) {
        CHECK(text_section->coff_info->arm64_stub_used == 1, "far call did not use a branch island");
        if (text_section->coff_info->arm64_stub_used == 1) {
            uint64_t target;
            memcpy(&target, text_section->coff_info->arm64_stubs + 8, sizeof(target));
            CHECK(target == UINT64_C(0x40000000), "branch island points at wrong target");
        }
    }
    ghciRemoveSymbolTable(owner, "arm64_target", NULL);
    ghciRemoveSymbolTable(owner, "arm64_data", NULL);
    removeOcSymbols(object);
    freeObjectCode(object);
}

int main(int argc, char **argv)
{
    if (argc != 2) { fprintf(stderr, "usage: pei386_arm64_reloc fixture.obj\n"); return 2; }
    ll_linker owner = {0};
    ll_compat_host_ops ops = make_ops(&host_a);
    ll_compat_config config = {.flags = LL_CFG_NO_EXEC | LL_CFG_ALL_FORMATS, .noexec_base = 0x10000000};
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ObjectCode object = {0};
    Section sections[3] = {0};
    struct ll_coff_section_info section_info = {0};
    struct ll_coff_object_info info = {0};
    COFF_HEADER_INFO header = {.type = COFF_IMAGE};
    COFF_symbol sym = {0};
    object.L = &owner; object.sections = sections; object.n_sections = 3;
    object.coff_info = &info; info.ch_info = &header;
    sections[0].coff_info = &section_info;
    sections[1].start = (void *)(uintptr_t)0x20000000;
    sections[1].exec_start = 0x20000000; sections[1].size = 0x1000;
    sym.og.SectionNumber = 2;
    const uintptr_t P = 0x10000000;
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_ADDR32, 7, 0x12340000, P, 0x12340007);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_ADDR32NB, 3, 0x10001234, P, 0x1237);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_REL32, 4, P + 0x100, P, 0x100);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_REL32, 0, UINT64_C(0x280000003), UINT64_C(0x200000000), 0x7fffffff);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_REL32, 0, UINT64_C(0x180000004), UINT64_C(0x200000000), 0x80000000);
    check_range(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_REL32, 0, UINT64_C(0x280000004), UINT64_C(0x200000000));
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_BRANCH26, 0x94000000, P + 0x100, P, 0x94000040);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_BRANCH19, 0x54000000, P - 4, P, 0x54ffffe0);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_BRANCH14, 0x36000000, P + 4, P, 0x36000020);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_PAGEBASE_REL21, 0x90000000, P + 0x3000, P, 0xf0000000);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_PAGEOFFSET_12A, 0x91000000, P + 0x123, P, 0x91048c00);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_PAGEOFFSET_12L, 0xf9400000, P + 0x120, P, 0xf9409000);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_SECREL, 5, 0x20000080, P, 0x85);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_BRANCH26, 0x14000000, P + 0x07fffffc, P, 0x15ffffff);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_BRANCH26, 0x14000000, P - 0x08000000, P, 0x16000000);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_PAGEBASE_REL21, 0x90000000, UINT64_C(0x2fffff000), UINT64_C(0x200000000), 0xf07fffe0);
    check_apply(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_PAGEBASE_REL21, 0x90000000, UINT64_C(0x100000000), UINT64_C(0x200000000), 0x90800000);
    check_range(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_PAGEBASE_REL21, 0x90000000, UINT64_C(0x300000000), UINT64_C(0x200000000));
    check_range(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_ADDR32, 1, UINT64_C(0xffffffff), P);
    check_range(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_BRANCH19, 0x54000000, P + 0x100000, P);
    check_range(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_BRANCH14, 0x36000000, P + 0x8000, P);
    check_range(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_BRANCH26, 0x14000000, P + 2, P);
    check_range(&owner, &object, &sections[0], &sym, LL_COFF_IMAGE_REL_ARM64_PAGEOFFSET_12L, 0xf9400000, P + 3, P);
    uint8_t two[4] = {0, 1, 0, 0};
    ll_site section_site = {.work = two + 1, .exec = P};
    CHECK(pe_arm64_reloc(&object, &sections[0], &sym, "test", LL_COFF_IMAGE_REL_ARM64_SECTION, section_site, 0x20000080), "SECTION failed");
    CHECK(two[0] == 0 && two[1] == 3 && two[2] == 0 && two[3] == 0, "SECTION wrote wrong width or index");
    uint8_t wide[10] = {0};
    uint64_t addend = 5, value;
    memcpy(wide + 1, &addend, sizeof(addend));
    ll_site wide_site = {.work = wide + 1, .exec = P};
    CHECK(pe_arm64_reloc(&object, &sections[0], &sym, "test", LL_COFF_IMAGE_REL_ARM64_ADDR64, wide_site, UINT64_C(0x123456789abcdef0)), "ADDR64 failed");
    memcpy(&value, wide + 1, sizeof(value));
    CHECK(value == UINT64_C(0x123456789abcdef5) && wide[0] == 0 && wide[9] == 0, "ADDR64 differs");
    uint8_t island[16] = {0}, branch[4];
    section_info.arm64_stubs = island; section_info.arm64_stubs_exec = P + 0x1000;
    section_info.arm64_stub_count = 1;
    put32(branch, UINT32_C(0x94000000));
    ll_site branch_site = {.work = branch, .exec = P};
    CHECK(pe_arm64_reloc(&object, &sections[0], &sym, "test", LL_COFF_IMAGE_REL_ARM64_BRANCH26, branch_site, P + 0x20000000), "far branch island failed");
    memcpy(&value, island + 8, sizeof(value));
    CHECK(get32(branch) == UINT32_C(0x94000400) && get32(island) == UINT32_C(0x58000050) &&
          get32(island + 4) == UINT32_C(0xd61f0200) && value == P + 0x20000000, "branch island differs");
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    test_object(&owner, argv[1]);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    CHECK(host_a.live == 0, "ARM64 relocation test leaked bookkeeping");
    if (failures) return 1;
    puts("pei386_arm64_reloc: PASS");
    return 0;
}
