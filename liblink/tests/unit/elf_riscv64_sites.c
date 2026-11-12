/* Exercise the real RISC-V typed relocation apply table with distinct sites. */
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "linker/elf_reloc_riscv64.h"
#include "linker/SymbolExtras.h"
#include "ll_path.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !LL_BUILD_ELF || !LL_TARGET_RISCV64
#error "elf_riscv64_sites requires the RISC-V ELF target"
#endif
#include "linker/elf_reloc_riscv64.c"
#include "linker/elf_plt.c"
#include "linker/elf_plt_riscv64.c"
#include "linker/elf_util.c"

static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "elf_riscv64_sites:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

static uint32_t word(const uint8_t *p)
{
    uint32_t value;
    memcpy(&value, p, sizeof(value));
    return value;
}

static void put_word(uint8_t *p, uint32_t value)
{
    memcpy(p, &value, sizeof(value));
}

static int apply(ObjectCode *oc, Section *section, size_t symbol_index,
                 uint32_t type,
                 uint8_t *work, uintptr_t exec, uintptr_t symbol,
                 int64_t addend, uintptr_t got_exec, bool paired_valid,
                 int32_t paired_hi)
{
    const ll_reloc_context context = {
        .symbol_index = symbol_index,
        .symbol_name = "fixture_symbol",
        .format_symbol_type = LL_ELF_STT_OBJECT,
        .got_exec = got_exec,
        .paired_hi_valid = paired_valid,
        .paired_hi_value = paired_hi
    };
    const ll_site site = {.work = work, .exec = exec};
    return ll_elf_riscv64_reloc_ops.apply(oc, section, type, site, symbol,
                                           addend, LL_SYM_DATA, &context);
}

static void check_scalar_and_got(ObjectCode *oc, Section *section)
{
    uint8_t *bytes = malloc(32);
    uint8_t before[32];
    if (bytes == NULL) abort();
    Section work_section = *section;
    work_section.start = bytes;
    work_section.size = 32;
    memset(bytes, 0xa5, 32);
    memcpy(before, bytes, 32);
    const uintptr_t synthetic_p = UINT32_C(0x10000008);
    CHECK(apply(oc, &work_section, 7, LL_ELF_R_RISCV_64, bytes + 8, synthetic_p,
                UINT64_C(0x123456789abcdef0), -16, 0, false, 0),
          "R_RISCV_64 apply failed");
    uint64_t scalar;
    memcpy(&scalar, bytes + 8, sizeof(scalar));
    CHECK(scalar == UINT64_C(0xffffffff9abcdee0),
          "scalar relocation did not preserve the backend's signed 32-bit result");
    CHECK(memcmp(bytes, before, 8) == 0 && memcmp(bytes + 16, before + 16, 16) == 0,
          "scalar relocation changed adjacent canaries");

    /* GOT_HI20 must use the selected slot's execution address. */
    memset(bytes, 0xa5, 32);
    put_word(bytes + 8, UINT32_C(0x00000517)); /* auipc a0, 0 */
    memcpy(before, bytes, 32);
    CHECK(apply(oc, &work_section, 7, LL_ELF_R_RISCV_GOT_HI20, bytes + 8,
                synthetic_p, UINT32_C(0x70000000), 0,
                UINT32_C(0x10012000), false, 0),
          "R_RISCV_GOT_HI20 apply failed");
    CHECK(word(bytes + 8) == UINT32_C(0x00012517),
          "GOT_HI20 used the symbol address or work address instead of selected GOT exec");
    CHECK(memcmp(bytes, before, 8) == 0 && memcmp(bytes + 12, before + 12, 20) == 0,
          "GOT_HI20 changed adjacent canaries");
    free(bytes);
}

static void check_paired_lo(ObjectCode *oc, Section *section)
{
    struct LoCase { int32_t hi; uint32_t type, initial, expected; };
    static const struct LoCase cases[] = {
        {0, LL_ELF_R_RISCV_PCREL_LO12_I, UINT32_C(0x00050513),
         UINT32_C(0x00050513)},
        {0x12345, LL_ELF_R_RISCV_PCREL_LO12_I, UINT32_C(0x00050513),
         UINT32_C(0x34550513)},
        {-0x123, LL_ELF_R_RISCV_PCREL_LO12_S, UINT32_C(0x00a53023),
         UINT32_C(0xeca53ea3)}
    };
    uint8_t *bytes = malloc(32);
    if (bytes == NULL) abort();
    Section work_section = *section;
    work_section.start = bytes;
    work_section.size = 32;
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        memset(bytes, 0xa5, 32);
        put_word(bytes + 8, cases[i].initial);
        uint8_t before[32];
        memcpy(before, bytes, 32);
        CHECK(apply(oc, &work_section, 7, cases[i].type, bytes + 8,
                    UINT32_C(0x10000018), UINT32_C(0x10000008), 0, 0,
                    true, cases[i].hi), "paired PCREL_LO apply failed");
        CHECK(word(bytes + 8) == cases[i].expected,
              "paired LO did not consume the signed paired HI result");
        CHECK(memcmp(bytes, before, 8) == 0 && memcmp(bytes + 12, before + 12, 20) == 0,
              "paired LO changed adjacent canaries");
    }

    memset(bytes, 0xa5, 32);
    put_word(bytes + 8, UINT32_C(0x00050513));
    uint8_t before[32];
    memcpy(before, bytes, 32);
    ll_error_clear(oc->L);
    CHECK(!apply(oc, &work_section, 7, LL_ELF_R_RISCV_PCREL_LO12_I, bytes + 8,
                 UINT32_C(0x10000018), UINT32_C(0x10000008), 0, 0,
                 false, 0), "missing paired HI unexpectedly succeeded");
    CHECK(oc->L->error_status == LL_E_FORMAT &&
          strcmp(oc->L->error_message,
              "Missing HI relocation for R_RISCV_PCREL_LO12_I: P 0x10000018 "
              "S 0x10000008 fixture_symbol") == 0,
          "missing paired HI status/message changed");
    CHECK(memcmp(bytes, before, 32) == 0,
          "missing paired HI wrote storage");
    free(bytes);
}

static void check_branch_range(ObjectCode *oc, Section *section)
{
    uint8_t *bytes = malloc(16);
    uint8_t before[16];
    if (bytes == NULL) abort();
    Section work_section = *section;
    work_section.start = bytes;
    work_section.size = 16;
    memset(bytes, 0xa5, 16);
    put_word(bytes + 4, UINT32_C(0x00000063)); /* beq x0, x0, 0 */
    memcpy(before, bytes, 16);
    ll_error_clear(oc->L);
    CHECK(!apply(oc, &work_section, 7, LL_ELF_R_RISCV_BRANCH, bytes + 4,
                 UINT32_C(0x10000000), UINT32_C(0x10002000), 0, 0,
                 false, 0), "out-of-range branch unexpectedly succeeded");
    CHECK(oc->L->error_status == LL_E_RELOC_RANGE,
          "out-of-range branch returned the wrong status");
    CHECK(memcmp(bytes, before, 16) == 0,
          "out-of-range branch wrote its instruction or adjacent canaries");
    free(bytes);
}

static void check_fake_extras(ObjectCode *oc, Section *section)
{
    ll_symbol_extra_riscv64 extras[2] = {{0}, {0}};
    uint8_t *instructions = malloc(3 * 4);
    if (instructions == NULL) abort();
    Section work_section = *section;
    work_section.start = instructions;
    work_section.size = 3 * 4;
    const uint32_t initial = UINT32_C(0x00000517); /* auipc a0, 0 */
    for (size_t i = 0; i < 3; ++i) put_word(instructions + 4 * i, initial);
    oc->symbol_extras = (SymbolExtra *)extras;
    oc->symbol_extras_exec = UINT32_C(0x10000804);
    oc->first_symbol_extra = 7;
    oc->n_symbol_extras = 2;
    oc->symbol_extra_stride = sizeof(extras[0]);

    const uintptr_t p = UINT32_C(0x10000008);
    const uintptr_t target = UINT32_C(0x70001234);
    CHECK(apply(oc, &work_section, 7, LL_ELF_R_RISCV_GOT_HI20, instructions, p,
                target, 0, 0, false, 0), "fake-extra GOT_HI20 failed");
    CHECK(apply(oc, &work_section, 7, LL_ELF_R_RISCV_GOT_HI20, instructions + 4, p,
                target, 0, 0, false, 0), "repeated fake-extra GOT_HI20 failed");
    CHECK(apply(oc, &work_section, 8, LL_ELF_R_RISCV_GOT_HI20, instructions + 8, p,
                target, 0, 0, false, 0), "distinct-index fake-extra GOT_HI20 failed");
    CHECK(extras[0].addr == target && extras[1].addr == target,
          "fake extras did not store their selected symbol targets");
    CHECK(word(instructions) == initial && word(instructions + 4) == initial &&
          word(instructions + 8) == UINT32_C(0x00001517),
          "fake extras did not reuse same-index or isolate distinct-index exec sites");
    oc->symbol_extras = NULL;
    oc->symbol_extras_exec = 0;
    oc->first_symbol_extra = 0;
    oc->n_symbol_extras = 0;
    oc->symbol_extra_stride = 0;
    free(instructions);
}

static void check_selected_got_producer(ObjectCode *oc, Section *section)
{
    uint8_t got[16] = {0};
    ll_elf_target_sym raw = {
        .st_info = LL_ELF64_ST_INFO(LL_ELF_STB_GLOBAL, LL_ELF_STT_OBJECT)
    };
    ElfSymbol symbols_a[2] = {0}, symbols_b[2] = {0};
    const uintptr_t same_symbol = UINT32_C(0x70001234);
    symbols_a[1] = (ElfSymbol){.name = "same", .elf_sym = &raw,
        .addr = (void *)same_symbol, .got_addr = got};
    symbols_b[1] = (ElfSymbol){.name = "same", .elf_sym = &raw,
        .addr = (void *)same_symbol, .got_addr = got + 8};
    ElfSymbolTable symbol_tables[2] = {
        {.index = 2, .symbols = symbols_a, .n_symbols = 2},
        {.index = 3, .symbols = symbols_b, .n_symbols = 2}
    };
    symbol_tables[0].next = &symbol_tables[1];
    ll_elf_target_shdr headers[2] = {{.sh_link = 2}, {.sh_link = 3}};
    ll_elf_target_rela relocs[2] = {
        {.r_offset = 8, .r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_GOT_HI20)},
        {.r_offset = 12, .r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_GOT_HI20)}
    };
    ElfRelocationATable tables[2] = {
        {.targetSectionIndex = 0, .sectionHeader = &headers[0],
         .relocations = &relocs[0], .n_relocations = 1},
        {.targetSectionIndex = 0, .sectionHeader = &headers[1],
         .relocations = &relocs[1], .n_relocations = 1}
    };
    tables[0].next = &tables[1];
    struct ll_elf_object_info info = {
        .symbolTables = symbol_tables, .relaTable = tables,
        .got_start = got, .got_size = sizeof(got),
        .got_exec = UINT32_C(0x10000804)
    };
    uint8_t *instructions = malloc(32);
    if (instructions == NULL) abort();
    memset(instructions, 0xa5, 32);
    put_word(instructions + 8, UINT32_C(0x00000517));
    put_word(instructions + 12, UINT32_C(0x00000517));
    uint8_t before[32];
    memcpy(before, instructions, sizeof(before));
    Section selected = *section;
    selected.start = instructions;
    selected.size = 32;
    selected.exec_start = UINT32_C(0x10000000);
    oc->sections = &selected;
    oc->elf_info = &info;
    CHECK(relocateObjectCodeRISCV64(oc) == EXIT_SUCCESS,
          "selected-GOT relocation producer failed");
    CHECK(word(instructions + 8) == UINT32_C(0x00000517) &&
          word(instructions + 12) == UINT32_C(0x00001517),
          "producer merged GOT slots for equal symbol addresses");
    CHECK(memcmp(instructions, before, 8) == 0 &&
          memcmp(instructions + 16, before + 16, 16) == 0,
          "selected-GOT producer changed canaries");
    oc->sections = section;
    oc->elf_info = NULL;
    free(instructions);
}

static void check_unsupported_paired_hi(ObjectCode *oc, Section *section)
{
    ll_elf_target_sym raw = {
        .st_info = LL_ELF64_ST_INFO(LL_ELF_STB_GLOBAL, LL_ELF_STT_OBJECT)
    };
    ElfSymbol symbols[2] = {0};
    symbols[1] = (ElfSymbol){.name = "tls_pair", .elf_sym = &raw,
        .addr = (void *)(uintptr_t)(section->exec_start + 8)};
    ElfSymbolTable symbol_table = {.index = 4, .symbols = symbols, .n_symbols = 2};
    ll_elf_target_shdr header = {.sh_link = 4};
    ll_elf_target_rela relocs[2] = {
        {.r_offset = 8, .r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_TLS_GD_HI20)},
        {.r_offset = 16, .r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_PCREL_LO12_I)}
    };
    ElfRelocationATable table = {.targetSectionIndex = 0, .sectionHeader = &header,
        .relocations = relocs, .n_relocations = 2};
    struct ll_elf_object_info info = {.symbolTables = &symbol_table};
    oc->elf_info = &info;
    bool found = false;
    int32_t value = 0;
    ll_error_clear(oc->L);
    CHECK(!pairedHiValue(oc, section, &table, 1,
                         (addr_t)(section->exec_start + 8), &found, &value),
          "matched unsupported TLS HI was accepted by paired lookup");
    CHECK(found && oc->L->error_status == LL_E_UNSUPPORTED,
          "matched unsupported TLS HI did not propagate its error status");
    oc->elf_info = NULL;
}

int main(void)
{
    uint8_t *storage = malloc(64);
    if (storage == NULL) abort();
    memset(storage, 0, 64);
    ll_linker owner = {0};
    ll_install_host_for(&owner, NULL, NULL);
    Section section = {.start = storage, .size = 64,
        .kind = SECTIONKIND_CODE_OR_RODATA, .exec_start = UINT32_C(0x10000000)};
    ObjectCode object = {.L = &owner, .sections = &section, .n_sections = 1,
                         .fileName = WSTR("riscv-sites.o")};
    CHECK(ll_elf_riscv64_reloc_ops.machine == LL_ELF_EM_RISCV,
          "wrong RISC-V relocation table identity");
    CHECK(ll_elf_riscv64_reloc_ops.extra_bytes_per_symbol ==
              sizeof(ll_symbol_extra_riscv64),
          "wrong RISC-V symbol-extra payload size");
    check_scalar_and_got(&object, &section);
    check_paired_lo(&object, &section);
    check_branch_range(&object, &section);
    check_fake_extras(&object, &section);
    check_selected_got_producer(&object, &section);
    check_unsupported_paired_hi(&object, &section);
    free(storage);
    if (failures != 0) return 1;
    (void)puts("elf_riscv64_sites: PASS (distinct sites, GOT, paired LO, branch range)");
    return 0;
}
