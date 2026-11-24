/* Data-only controls of the real AArch64 ELF table and symbol producer. */
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "linker/elf_reloc_aarch64.h"
#include "ll_path.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !LL_BUILD_ELF || !LL_TARGET_AARCH64
#error "elf_aarch64_sites requires the AArch64 ELF target"
#endif
#include "linker/elf_reloc_aarch64.c"
#include "linker/elf_plt.c"
#include "linker/elf_plt_aarch64.c"
#include "linker/elf_util.c"

static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "elf_aarch64_sites:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

static void fatal(void *ctx, const char *message, va_list args)
{
    (void)ctx;
    (void)vfprintf(stderr, message, args);
    abort();
}

static void *allocate(void *ctx, size_t count, size_t size, const char *tag)
{
    (void)tag;
    void *p = calloc(count, size);
    if (p == NULL) abort();
    ++*(size_t *)ctx;
    return p;
}

static void release(void *ctx, void *p)
{
    if (p != NULL) {
        --*(size_t *)ctx;
        free(p);
    }
}

static uint32_t word(const uint8_t *bytes)
{
    uint32_t value;
    memcpy(&value, bytes, sizeof(value));
    return value;
}

static void put_word(uint8_t *bytes, uint32_t value)
{
    memcpy(bytes, &value, sizeof(value));
}

static int apply(ObjectCode *object, Section *section, uint32_t type,
                 uint8_t *work, uintptr_t exec, uintptr_t S, int64_t A,
                 uintptr_t got)
{
    const ll_reloc_context context = {.symbol_index = 19,
        .symbol_name = "same", .format_symbol_type = LL_ELF_STT_OBJECT,
        .got_exec = got};
    const ll_site site = {.work = work, .exec = exec};
    return ll_elf_aarch64_reloc_ops.apply(object, section, type, site, S, A,
                                        LL_SYM_DATA, &context);
}

static void check_fields(ObjectCode *object, Section *section)
{
    /* P is deliberately unrelated to the aligned heap storage. */
    const uintptr_t P = UINT32_C(0x10000008);
    static const struct {
        uint32_t type, instruction;
        uintptr_t S, got;
        int64_t A;
        uint64_t expected;
        size_t width;
    } cases[] = {
        {COMPAT_R_AARCH64_ABS64, 0, UINT64_C(0xfedcba9876543210), 0, -4,
            UINT64_C(0xfedcba987654320c), 8},
        {COMPAT_R_AARCH64_ABS32, 0, 0x1000, 0, -8, 0xff8, 4},
        {COMPAT_R_AARCH64_ABS16, 0, 0x100, 0, -4, 0xfc, 2},
        {COMPAT_R_AARCH64_PREL64, 0, 0x10000028, 0, -8, 0x18, 8},
        {COMPAT_R_AARCH64_PREL32, 0, 0x0fffffe8, 0, -8, 0xffffffd8, 4},
        {COMPAT_R_AARCH64_PREL16, 0, 0x10000028, 0, 4, 0x24, 2},
        {COMPAT_R_AARCH64_ADR_PREL_PG_HI21, 0x90000003,
            0x10001200, 0, 0, 0xb0000003, 4},
        {COMPAT_R_AARCH64_ADD_ABS_LO12_NC, 0x91000022,
            0x12345abc, 0, 0, 0x912af022, 4},
        {COMPAT_R_AARCH64_LDST8_ABS_LO12_NC, 0x39000022,
            0x12345abc, 0, 0, 0x392af022, 4},
        {COMPAT_R_AARCH64_LDST16_ABS_LO12_NC, 0x79000022,
            0x12345246, 0, 0, 0x79048c22, 4},
        {COMPAT_R_AARCH64_LDST32_ABS_LO12_NC, 0xb9000022,
            0x1234548c, 0, 0, 0xb9048c22, 4},
        {COMPAT_R_AARCH64_LDST64_ABS_LO12_NC, 0xf9000022,
            0x12345918, 0, 0, 0xf9048c22, 4},
        {COMPAT_R_AARCH64_LDST128_ABS_LO12_NC, 0x3dc00022,
            0x12345120, 0, 0, 0x3dc04822, 4},
        {COMPAT_R_AARCH64_JUMP26, 0x14000000,
            0x10000018, 0, -4, 0x14000003, 4},
        {COMPAT_R_AARCH64_CALL26, 0x94000000,
            0x10000000, 0, 4, 0x97ffffff, 4},
        {COMPAT_R_AARCH64_ADR_GOT_PAGE, 0x90000003,
            0x77770000, 0x10003080, 0, 0xf0000003, 4},
        {COMPAT_R_AARCH64_LD64_GOT_LO12_NC, 0xf9000022,
            0x77770000, 0x10000918, 0, 0xf9048c22, 4}
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        uint8_t *bytes = section->start;
        memset(bytes, 0xa5, 128);
        put_word(bytes + 8, cases[i].instruction);
        uint8_t before[128];
        memcpy(before, bytes, sizeof(before));
        ll_error_clear(object->L);
        CHECK(apply(object, section, cases[i].type, bytes + 8, P,
                    cases[i].S, cases[i].A, cases[i].got), "field apply failed");
        uint8_t expected[8];
        for (size_t b = 0; b < sizeof(expected); ++b)
            expected[b] = (uint8_t)(cases[i].expected >> (8 * b));
        CHECK(memcmp(bytes + 8, expected, cases[i].width) == 0,
              "field used work instead of exec, S or selected GOT");
        CHECK(memcmp(bytes, before, 8) == 0 &&
              memcmp(bytes + 8 + cases[i].width, before + 8 + cases[i].width,
                     128 - 8 - cases[i].width) == 0, "field changed adjacent bytes");
        CHECK(object->L->error_status == LL_OK, "field set an error");
    }
}

static void check_errors(ObjectCode *object, Section *section)
{
    static const struct {
        uint32_t type, instruction;
        uintptr_t S, got;
        int64_t A, computed;
        ll_status status;
        const char *reason;
    } cases[] = {
        {COMPAT_R_AARCH64_ABS32, 0, 0x80000000, 0, 0, INT64_C(2147483648),
            LL_E_RELOC_RANGE, "value is out of range"},
        {COMPAT_R_AARCH64_ABS16, 0, 0x8000, 0, 0, 32768,
            LL_E_RELOC_RANGE, "value is out of range"},
        {COMPAT_R_AARCH64_LDST64_ABS_LO12_NC, 0xf9000022, 0x12345003, 0, 0, 0,
            LL_E_RELOC_RANGE, "symbol value is not aligned for the relocation"},
        {COMPAT_R_AARCH64_LD64_GOT_LO12_NC, 0xf9000022, 0x12345000,
            0x10000919, 0, 0x919, LL_E_RELOC_RANGE,
            "GOT value is not eight-byte aligned"},
        {COMPAT_R_AARCH64_LDST64_ABS_LO12_NC, 0x39000022, 0x12345008, 0, 0, 8,
            LL_E_FORMAT, "relocation scale does not match the instruction"},
        {UINT32_MAX, 0, 1, 0, -7, -7, LL_E_UNSUPPORTED, "unsupported relocation type"}
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        memset(section->start, 0xa5, 128);
        put_word((uint8_t *)section->start + 8, cases[i].instruction);
        uint8_t before[128];
        memcpy(before, section->start, sizeof(before));
        ll_error_clear(object->L);
        CHECK(!apply(object, section, cases[i].type,
                     (uint8_t *)section->start + 8, 0x10000008,
                     cases[i].S, cases[i].A, cases[i].got), "invalid field succeeded");
        char expected[LL_ERROR_CAPACITY];
        const int n = snprintf(expected, sizeof(expected),
            "AArch64 relocation type %" PRIu32 ": %s (addend %" PRId64 ")",
            cases[i].type, cases[i].reason, cases[i].computed);
        CHECK(n >= 0 && (size_t)n < sizeof(expected), "expected error truncated");
        CHECK(object->L->error_status == cases[i].status &&
              strcmp(object->L->error_message, expected) == 0, "exact error changed");
        CHECK(memcmp(section->start, before, sizeof(before)) == 0,
              "failed field wrote storage");
    }
    ll_error_clear(object->L);
    CHECK(ll_elf_aarch64_reloc_ops.apply(object, NULL, COMPAT_R_AARCH64_NONE,
          (ll_site){0}, 0, 0, (ll_sym_type)0, NULL), "NONE accessed absent inputs");
    CHECK(object->L->error_status == LL_OK, "NONE changed diagnostics");
}

static void check_branches(ObjectCode *object, Section *section, size_t *live)
{
    uint8_t *bytes = section->start;
    memset(bytes, 0xa5, 128);
    const uintptr_t target = UINT64_C(0x700012345678);
    put_word(bytes + 8, 0x94000000);
    ll_error_clear(object->L);
    CHECK(apply(object, section, COMPAT_R_AARCH64_CALL26, bytes + 8,
                section->exec_start + 8, target, 4, 0), "far call failed");
    CHECK(word(bytes + 8) == 0x9400000f && *live == 1,
          "far call used stub work instead of exec");
    put_word(bytes + 16, 0x94000000);
    CHECK(apply(object, section, COMPAT_R_AARCH64_CALL26, bytes + 16,
                section->exec_start + 16, target, 4, 0), "repeated far call failed");
    CHECK(word(bytes + 16) == 0x9400000d && *live == 1,
          "repeated far call did not reuse its target");
    put_word(bytes + 24, 0x14000000);
    CHECK(apply(object, section, COMPAT_R_AARCH64_JUMP26, bytes + 24,
                section->exec_start + 24, target + 8, 0, 0), "distinct far jump failed");
    CHECK(word(bytes + 24) == 0x1400000f && *live == 2,
          "distinct far jump lost stub identity/order");
    CHECK(section->elf_info->stubs->target == (void *)target &&
          section->elf_info->stubs->next->target == (void *)(target + 8),
          "stub targets incorporated the addend or lost full width");
    for (size_t i = 104; i < 128; ++i)
        CHECK(bytes[i] == 0xa5, "stub changed adjacent storage");
    elf_freeStubs(object->L, section);
    CHECK(*live == 0, "branch metadata retained");
    put_word(bytes + 8, 0x94000000);
    ll_error_clear(object->L);
    CHECK(!apply(object, section, COMPAT_R_AARCH64_CALL26, bytes + 8,
                 UINT64_C(0x400000000), target, 0, 0),
          "out-of-range stub destination succeeded");
    CHECK(word(bytes + 8) == 0x94000000 && *live == 1 &&
          object->L->error_status == LL_E_RELOC_RANGE &&
          strcmp(object->L->error_message,
            "AArch64 relocation type 283: branch remains out of range after a stub "
            "(addend -16911433664)") == 0,
          "stub-range failure changed bytes, error or existing allocation behavior");
    elf_freeStubs(object->L, section);
    CHECK(*live == 0, "failed branch metadata retained");
}

static void check_symbol_selection(ObjectCode *object, Section *section)
{
    uint8_t *got = malloc(8192);
    CHECK(got != NULL, "cannot allocate GOT fixture");
    if (got == NULL) return;
    ll_elf_target_sym raw = {.st_info = LL_ELF64_ST_INFO(LL_ELF_STB_GLOBAL, LL_ELF_STT_OBJECT)};
    ElfSymbol symbols[2][2] = {0};
    for (size_t i = 0; i < 2; ++i) {
        symbols[i][1] = (ElfSymbol){.name = "same", .elf_sym = &raw,
            .addr = (void *)(uintptr_t)UINT64_C(0x700012345678),
            .got_addr = got + 4088 + 8 * i};
    }
    ElfSymbolTable tables[2] = {{.index = 2, .symbols = symbols[0], .n_symbols = 2},
                              {.index = 3, .symbols = symbols[1], .n_symbols = 2}};
    tables[0].next = &tables[1];
    ll_elf_target_shdr headers[2] = {{.sh_link = 2}, {.sh_link = 3}};
    ll_elf_target_rela relocs[2][2] = {
        {{.r_offset = 8, .r_info = LL_ELF64_R_INFO(1, COMPAT_R_AARCH64_ADR_GOT_PAGE)},
         {.r_offset = 16, .r_info = LL_ELF64_R_INFO(1, COMPAT_R_AARCH64_LD64_GOT_LO12_NC)}},
        {{.r_offset = 24, .r_info = LL_ELF64_R_INFO(1, COMPAT_R_AARCH64_ADR_GOT_PAGE)},
         {.r_offset = 32, .r_info = LL_ELF64_R_INFO(1, COMPAT_R_AARCH64_LD64_GOT_LO12_NC)}}
    };
    ElfRelocationATable reloc_tables[2] = {
        {.targetSectionIndex = 0, .sectionHeader = &headers[0],
         .relocations = relocs[0], .n_relocations = 2},
        {.targetSectionIndex = 0, .sectionHeader = &headers[1],
         .relocations = relocs[1], .n_relocations = 2}};
    reloc_tables[0].next = &reloc_tables[1];
    struct ll_elf_object_info info = {.symbolTables = tables, .relaTable = reloc_tables,
        .got_start = got, .got_size = 8192, .got_exec = 0x10009000};
    object->elf_info = &info;
    uint8_t *bytes = section->start;
    memset(bytes, 0xa5, 128);
    for (size_t i = 0; i < 2; ++i) {
        put_word(bytes + 8 + 16 * i, 0x90000003);
        put_word(bytes + 16 + 16 * i, 0xf9000022);
    }
    ll_error_clear(object->L);
    CHECK(relocateObjectCodeAarch64(object) == EXIT_SUCCESS,
          "selected-symbol object relocation failed");
    CHECK(word(bytes + 8) == 0xb0000043 && word(bytes + 16) == 0xf907fc22 &&
          word(bytes + 24) == 0xd0000043 && word(bytes + 32) == 0xf9000022,
          "equal S/index in different tables merged selected GOT slots");
    CHECK(object->L->error_status == LL_OK, "symbol producer set an error");

    symbols[0][1].addr = NULL;
    CHECK(relocateObjectCodeAarch64(object) == EXIT_FAILURE &&
          object->L->error_status == LL_E_STATE &&
          strcmp(object->L->error_message,
            "AArch64 relocation type 311 has no address for same in aarch64-sites.o") == 0,
          "missing-address diagnostic changed");
    ll_error_clear(object->L);
    symbols[0][1].addr = symbols[1][1].addr;

    /* NONE must skip symbol selection, even with an absent symbol table. */
    relocs[0][0].r_info = LL_ELF64_R_INFO(999, COMPAT_R_AARCH64_NONE);
    reloc_tables[0].n_relocations = 1;
    reloc_tables[0].next = NULL;
    info.symbolTables = NULL;
    uint8_t before[128];
    memcpy(before, bytes, sizeof(before));
    CHECK(relocateObjectCodeAarch64(object) == EXIT_SUCCESS &&
          memcmp(bytes, before, sizeof(before)) == 0, "NONE selected or wrote a symbol");

    info.symbolTables = tables;
    ll_elf_target_rel implicit = {.r_offset = 8,
        .r_info = LL_ELF64_R_INFO(1, COMPAT_R_AARCH64_CALL26)};
    ElfRelocationTable rel_table = {.targetSectionIndex = 0, .sectionHeader = headers,
        .relocations = &implicit, .n_relocations = 1};
    info.relTable = &rel_table;
    CHECK(relocateObjectCodeAarch64(object) == EXIT_FAILURE &&
          object->L->error_status == LL_E_UNSUPPORTED &&
          strcmp(object->L->error_message,
            "AArch64 relocation type 283: relocations without explicit addends are unsupported (addend 0)") == 0 &&
          memcmp(bytes, before, sizeof(before)) == 0,
          "implicit REL failure or unwritten storage changed");
    object->elf_info = NULL;
    free(got);
}

int main(void)
{
    uint8_t *bytes = malloc(128);
    if (bytes == NULL) return 1;
    size_t live = 0;
    ll_linker owner = {0};
    owner.services.host.ctx = &live;
    owner.services.host.fatal = fatal;
    owner.services.host.calloc_ = allocate;
    owner.services.host.free_ = release;
    struct ll_elf_section_info elf = {.stub_offset = bytes + 64, .stub_size = 40};
    Section section = {.start = bytes, .size = 32, .kind = SECTIONKIND_CODE_OR_RODATA,
        .exec_start = 0x10000000, .elf_info = &elf};
    ObjectCode object = {.L = &owner, .sections = &section, .n_sections = 1,
                         .fileName = WSTR("aarch64-sites.o")};
    CHECK(ll_elf_aarch64_reloc_ops.machine == LL_ELF_EM_AARCH64 &&
          ll_elf_aarch64_reloc_ops.extra_bytes_per_symbol == 0, "wrong table identity");
    check_fields(&object, &section);
    check_errors(&object, &section);
    check_branches(&object, &section, &live);
    section.size = 40;
    check_symbol_selection(&object, &section);
    CHECK(live == 0, "metadata leak");
    free(bytes);
    if (failures != 0) return 1;
    (void)puts("elf_aarch64_sites: PASS (exec arithmetic, selected GOT, veneers, errors, REL/NONE)");
    return 0;
}
