#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/ElfTypes.h"
#include "linker/elf_reloc_aarch64.c"

#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#if !defined(linux_HOST_OS) || !defined(aarch64_HOST_ARCH) \
    || !defined(OBJFORMAT_ELF)
#error "aarch64_reloc_errors requires generated Linux AArch64 ELF headers"
#endif

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "aarch64_reloc_errors:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static unsigned failures;
static unsigned fatal_calls;
static unsigned default_fatal_calls;
static ll_linker owner;

static _Noreturn void fail_on_fatal(void *ctx, const char *format, va_list args)
{
    (void)ctx;
    (void)format;
    (void)args;
    ++fatal_calls;
    (void)fprintf(stderr, "aarch64_reloc_errors: unexpected fatal callback\n");
    _Exit(99);
}

static _Noreturn void fail_on_default_fatal(void *ctx,
                                            const char *format,
                                            va_list args)
{
    (void)ctx;
    (void)format;
    (void)args;
    ++default_fatal_calls;
    (void)fprintf(stderr,
                  "aarch64_reloc_errors: unexpected default fatal callback\n");
    _Exit(98);
}

static void clear_owner_error(void)
{
    ll_error_clear(&owner);
}

static bool has_owner_error(ll_status status)
{
    return owner.error_status == status && owner.error_message[0] != '\0';
}

union Backing {
    uint64_t u64[8];
    uint32_t u32[16];
    uint16_t u16[32];
};

static void setup_object(ObjectCode *oc,
                         struct ObjectCodeFormatInfo *info,
                         Section *section,
                         struct SectionFormatInfo *section_info,
                         ElfSymbolTable *symbol_table,
                         ElfSymbol *symbols,
                         union Backing *backing)
{
    memset(oc, 0, sizeof(*oc));
    memset(info, 0, sizeof(*info));
    memset(section, 0, sizeof(*section));
    memset(section_info, 0, sizeof(*section_info));
    memset(symbol_table, 0, sizeof(*symbol_table));
    memset(symbols, 0, 2 * sizeof(*symbols));
    memset(backing, 0, sizeof(*backing));
    oc->L = &owner;
    oc->info = info;
    oc->sections = section;
    oc->n_sections = 1;
    section->start = backing->u64;
    section->size = sizeof(*backing);
    section->kind = SECTIONKIND_CODE_OR_RODATA;
    section->info = section_info;
    symbol_table->index = 1;
    symbol_table->symbols = symbols;
    symbol_table->n_symbols = 2;
    info->symbolTables = symbol_table;
}

static void setup_rela(struct ObjectCodeFormatInfo *info,
                       ElfRelocationATable *table,
                       Elf_Shdr *header,
                       Elf_Rela *relocations,
                       size_t count)
{
    memset(table, 0, sizeof(*table));
    memset(header, 0, sizeof(*header));
    header->sh_link = 1;
    table->targetSectionIndex = 0;
    table->sectionHeader = header;
    table->relocations = relocations;
    table->n_relocations = count;
    info->relaTable = table;
}

static void test_scalar_encodings(void)
{
    union Backing backing;
    Section section = {0};
    Elf_Rel rel = {0};
    memset(&backing, 0, sizeof(backing));

    section.start = &backing.u64[1];
    section.size = sizeof(backing.u64[1]);
    backing.u64[0] = UINT64_C(0xfeedfacecafebeef);
    backing.u64[1] = UINT64_MAX;
    backing.u64[2] = UINT64_C(0x1122334455667788);
    rel.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_ABS64);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel,
                              INT64_C(0x123456789abcdef0)) == EXIT_SUCCESS,
          "ABS64 encoding failed");
    CHECK(backing.u64[1] == UINT64_C(0x123456789abcdef0)
              && backing.u64[0] == UINT64_C(0xfeedfacecafebeef)
              && backing.u64[2] == UINT64_C(0x1122334455667788),
          "ABS64 did not write eight bytes or changed a neighbor");
    CHECK(owner.error_status == LL_OK, "successful ABS64 left an error");

    section.start = &backing.u32[1];
    section.size = sizeof(backing.u32[1]);
    backing.u32[0] = UINT32_C(0xfeedface);
    backing.u32[1] = UINT32_C(0xaaaaaaaa);
    backing.u32[2] = UINT32_C(0x11223344);
    rel.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_ABS32);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel, INT32_MAX) == EXIT_SUCCESS
              && backing.u32[1] == UINT32_C(0x7fffffff)
              && backing.u32[0] == UINT32_C(0xfeedface)
              && backing.u32[2] == UINT32_C(0x11223344),
          "ABS32 positive boundary or four-byte canaries failed");
    backing.u32[1] = UINT32_C(0xaaaaaaaa);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel, INT32_MIN) == EXIT_SUCCESS
              && backing.u32[1] == UINT32_C(0x80000000),
          "ABS32 negative boundary failed");

    for (int direction = 0; direction < 2; ++direction) {
        int64_t overflow = direction == 0
                               ? (int64_t)INT32_MAX + 1
                               : (int64_t)INT32_MIN - 1;
        uint32_t before = UINT32_C(0xa5c37e19);
        backing.u32[1] = before;
        clear_owner_error();
        CHECK(encodeAddendAarch64(&owner, &section, &rel, overflow)
                  == EXIT_FAILURE
                  && backing.u32[1] == before,
              "ABS32 overflow changed the target word");
        CHECK(has_owner_error(LL_E_RELOC_RANGE),
              "ABS32 overflow status or diagnostic missing");
    }

    section.start = &backing.u16[1];
    section.size = sizeof(backing.u16[1]);
    backing.u16[0] = UINT16_C(0xface);
    backing.u16[1] = UINT16_C(0xaaaa);
    backing.u16[2] = UINT16_C(0x3344);
    rel.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_ABS16);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel, INT16_MAX) == EXIT_SUCCESS
              && backing.u16[1] == UINT16_C(0x7fff)
              && backing.u16[0] == UINT16_C(0xface)
              && backing.u16[2] == UINT16_C(0x3344),
          "ABS16 positive boundary or two-byte canaries failed");
    backing.u16[1] = UINT16_C(0xaaaa);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel, INT16_MIN) == EXIT_SUCCESS
              && backing.u16[1] == UINT16_C(0x8000),
          "ABS16 negative boundary failed");

    for (int direction = 0; direction < 2; ++direction) {
        int64_t overflow = direction == 0
                               ? (int64_t)INT16_MAX + 1
                               : (int64_t)INT16_MIN - 1;
        uint16_t before = UINT16_C(0xa519);
        backing.u16[1] = before;
        clear_owner_error();
        CHECK(encodeAddendAarch64(&owner, &section, &rel, overflow)
                  == EXIT_FAILURE
                  && backing.u16[1] == before,
              "ABS16 overflow changed the target halfword");
        CHECK(has_owner_error(LL_E_RELOC_RANGE),
              "ABS16 overflow status or diagnostic missing");
    }
}

static void test_branch_and_adrp_boundaries(void)
{
    union Backing backing;
    Section section = {0};
    Elf_Rel rel = {0};
    const uint64_t before_canary = UINT64_C(0x1122334455667788);
    const uint64_t after_canary = UINT64_C(0x8877665544332211);

    memset(&backing, 0, sizeof(backing));
    section.start = &backing.u64[1];
    section.size = sizeof(backing.u64[1]);
    backing.u64[0] = before_canary;
    backing.u64[2] = after_canary;

    backing.u64[1] = UINT64_C(0xcafebabd14000000);
    rel.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_JUMP26);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel,
                              INT64_C(134217724)) == EXIT_SUCCESS
              && backing.u32[2] == UINT32_C(0x15ffffff)
              && backing.u64[1] == UINT64_C(0xcafebabd15ffffff)
              && backing.u64[0] == before_canary
              && backing.u64[2] == after_canary,
          "JUMP26 positive signed reach boundary differs from literal oracle");
    backing.u64[1] = UINT64_C(0xcafebabd14000000);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel,
                              -INT64_C(134217728)) == EXIT_SUCCESS
              && backing.u32[2] == UINT32_C(0x16000000)
              && backing.u64[1] == UINT64_C(0xcafebabd16000000)
              && backing.u64[0] == before_canary
              && backing.u64[2] == after_canary,
          "JUMP26 negative signed reach boundary differs from literal oracle");

    for (int direction = 0; direction < 2; ++direction) {
        int64_t overflow = direction == 0
                               ? INT64_C(134217728)
                               : -INT64_C(134217732);
        const uint64_t before_word = UINT64_C(0xcafebabd14000000);
        backing.u64[1] = before_word;
        clear_owner_error();
        CHECK(encodeAddendAarch64(&owner, &section, &rel, overflow)
                  == EXIT_FAILURE
                  && backing.u64[1] == before_word
                  && backing.u64[0] == before_canary
                  && backing.u64[2] == after_canary,
              "JUMP26 overflow changed instruction bytes");
        CHECK(has_owner_error(LL_E_RELOC_RANGE),
              "JUMP26 overflow status or diagnostic missing");
    }

    rel.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_ADR_PREL_PG_HI21);
    backing.u64[1] = UINT64_C(0xcafebabd90000000);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel,
                              INT64_C(4294963200)) == EXIT_SUCCESS
              && backing.u32[2] == UINT32_C(0xf07fffe0)
              && backing.u64[1] == UINT64_C(0xcafebabdf07fffe0)
              && backing.u64[0] == before_canary
              && backing.u64[2] == after_canary,
          "ADRP positive page-reach boundary differs from literal oracle");
    backing.u64[1] = UINT64_C(0xcafebabd90000000);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel,
                              -INT64_C(4294967296)) == EXIT_SUCCESS
              && backing.u32[2] == UINT32_C(0x90800000)
              && backing.u64[1] == UINT64_C(0xcafebabd90800000)
              && backing.u64[0] == before_canary
              && backing.u64[2] == after_canary,
          "ADRP negative page-reach boundary differs from literal oracle");

    for (int direction = 0; direction < 2; ++direction) {
        int64_t overflow = direction == 0
                               ? INT64_C(4294967296)
                               : -INT64_C(4294971392);
        const uint64_t before_word = UINT64_C(0xcafebabd90000000);
        backing.u64[1] = before_word;
        clear_owner_error();
        CHECK(encodeAddendAarch64(&owner, &section, &rel, overflow)
                  == EXIT_FAILURE
                  && backing.u64[1] == before_word
                  && backing.u64[0] == before_canary
                  && backing.u64[2] == after_canary,
              "ADRP overflow changed instruction bytes");
        CHECK(has_owner_error(LL_E_RELOC_RANGE),
              "ADRP overflow status or diagnostic missing");
    }
}

static void test_scaled_instruction_encoding(void)
{
    union Backing backing;
    Section section = {0};
    Elf_Rel rel = {0};
    section.start = &backing.u64[1];
    section.size = sizeof(backing.u64[1]);

    backing.u64[1] = UINT64_C(0xf9400000);
    rel.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_LDST64_ABS_LO12_NC);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel, 8) == EXIT_SUCCESS
              && backing.u32[2] == UINT32_C(0xf9400400),
          "LDST64_ABS_LO12_NC valid scale encoding differs from literal oracle");

    backing.u64[1] = UINT64_C(0xb9400000);
    uint64_t before = backing.u64[1];
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel, 8) == EXIT_FAILURE
              && backing.u64[1] == before,
          "nonzero LDST64 scale mismatch changed instruction bytes");
    CHECK(has_owner_error(LL_E_FORMAT),
          "nonzero LDST64 scale mismatch status or diagnostic missing");

    backing.u64[1] = UINT64_C(0xb9400400);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel, 0) == EXIT_SUCCESS
              && backing.u64[1] == UINT64_C(0xb9400000),
          "zero-offset legacy scale bypass changed behavior");

    backing.u64[1] = UINT64_C(0x91000000);
    rel.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_ADD_ABS_LO12_NC);
    clear_owner_error();
    CHECK(encodeAddendAarch64(&owner, &section, &rel, 0x123) == EXIT_SUCCESS
              && backing.u32[2] == UINT32_C(0x91048c00),
          "ADD_ABS_LO12_NC immediate differs from literal oracle");
}

static void test_relocation_loop_statuses(void)
{
    union Backing backing;
    ObjectCode oc;
    struct ObjectCodeFormatInfo info;
    struct SectionFormatInfo section_info;
    Section section;
    ElfSymbolTable symbol_table;
    ElfSymbol symbols[2];
    Elf_Shdr header = {0};
    Elf_Rela relocation = {0};
    ElfRelocationATable table;

    setup_object(&oc, &info, &section, &section_info, &symbol_table,
                 symbols, &backing);
    setup_rela(&info, &table, &header, &relocation, 1);
    symbols[1].name = "zero-address-test";
    relocation.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_ABS64);
    relocation.r_addend = 9;
    backing.u64[0] = UINT64_C(0xfeedfacecafebeef);
    symbols[1].addr = NULL;
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_FAILURE
              && backing.u64[0] == UINT64_C(0xfeedfacecafebeef),
          "zero-address RelA did not fail without changing the target");
    CHECK(has_owner_error(LL_E_STATE),
          "zero-address RelA lost state status or diagnostic");

    relocation.r_info = ELF64_R_INFO(1, UINT64_C(0x7fffffff));
    backing.u64[0] = UINT64_C(0x0123456789abcdef);
    symbols[1].addr = (ll_symbol_addr *)(void *)&backing.u64[4];
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_FAILURE
              && backing.u64[0] == UINT64_C(0x0123456789abcdef),
          "unknown RelA did not fail with unchanged payload");
    CHECK(has_owner_error(LL_E_UNSUPPORTED),
          "unknown RelA lost unsupported status or diagnostic");

    relocation.r_info = ELF64_R_INFO(999, COMPAT_R_AARCH64_NONE);
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_SUCCESS
              && owner.error_status == LL_OK
              && backing.u64[0] == UINT64_C(0x0123456789abcdef),
          "R_AARCH64_NONE did not skip without symbol lookup or writes");

    Elf_Rel implicit = {0};
    ElfRelocationTable rel_table = {0};
    implicit.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_ABS64);
    rel_table.targetSectionIndex = 0;
    rel_table.sectionHeader = &header;
    rel_table.relocations = &implicit;
    rel_table.n_relocations = 1;
    info.relaTable = NULL;
    info.relTable = &rel_table;
    backing.u64[0] = UINT64_C(0x8877665544332211);
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_FAILURE
              && backing.u64[0] == UINT64_C(0x8877665544332211),
          "SHT_REL did not fail without changing target bytes");
    CHECK(has_owner_error(LL_E_UNSUPPORTED),
          "SHT_REL lost unsupported status or diagnostic");
}

static void test_full_loop_scalar64(void)
{
    union Backing backing;
    ObjectCode oc;
    struct ObjectCodeFormatInfo info;
    struct SectionFormatInfo section_info;
    Section section;
    ElfSymbolTable symbol_table;
    ElfSymbol symbols[2];
    Elf_Shdr header = {0};
    Elf_Rela relocation = {0};
    ElfRelocationATable table;
    const uint64_t neighbor = UINT64_C(0xa1b2c3d4e5f60718);

    setup_object(&oc, &info, &section, &section_info, &symbol_table,
                 symbols, &backing);
    setup_rela(&info, &table, &header, &relocation, 1);
    backing.u64[0] = UINT64_MAX;
    backing.u64[1] = neighbor;
    symbols[1].addr = (ll_symbol_addr *)(uintptr_t)UINT64_C(0x6abcdef012345678);
    relocation.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_ABS64);
    relocation.r_addend = INT64_C(0x1234);
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_SUCCESS
              && owner.error_status == LL_OK
              && backing.u64[0] == UINT64_C(0x6abcdef0123468ac)
              && backing.u64[1] == neighbor,
          "ABS64 relocation loop narrowed the value or changed its neighbor");

    uintptr_t place = (uintptr_t)(void *)&backing.u64[0];
    uintptr_t target = place + (uintptr_t)UINT64_C(0x123456789);
    symbols[1].addr = (ll_symbol_addr *)target;
    relocation.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_PREL64);
    relocation.r_addend = INT64_C(0x100000000);
    backing.u64[0] = UINT64_MAX;
    backing.u64[1] = neighbor;
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_SUCCESS
              && owner.error_status == LL_OK
              && backing.u64[0] == UINT64_C(0x223456789)
              && backing.u64[1] == neighbor,
          "PREL64 relocation loop narrowed the value or changed its neighbor");
}

static void test_scaled_address_failures(void)
{
    union Backing backing;
    ObjectCode oc;
    struct ObjectCodeFormatInfo info;
    struct SectionFormatInfo section_info;
    Section section;
    ElfSymbolTable symbol_table;
    ElfSymbol symbols[2];
    Elf_Shdr header = {0};
    Elf_Rela relocation = {0};
    ElfRelocationATable table;
    uint64_t source_value = 0;
    uint8_t *misaligned = (uint8_t *)(void *)&source_value + 1;

    setup_object(&oc, &info, &section, &section_info, &symbol_table,
                 symbols, &backing);
    setup_rela(&info, &table, &header, &relocation, 1);
    relocation.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_LDST64_ABS_LO12_NC);
    symbols[1].addr = (ll_symbol_addr *)(void *)misaligned;
    backing.u32[0] = UINT32_C(0xf9400000);
    uint32_t before = backing.u32[0];
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_FAILURE
              && backing.u32[0] == before,
          "misaligned scaled address did not fail before writing");
    CHECK(has_owner_error(LL_E_RELOC_RANGE),
          "misaligned scaled address lost range status or diagnostic");

    symbols[1].addr = (ll_symbol_addr *)(void *)&source_value;
    uintptr_t source = (uintptr_t)(void *)&source_value;
    relocation.r_addend = (int64_t)(((uintptr_t)0x100 - (source & (uintptr_t)0xfff))
                                    & (uintptr_t)0xfff);
    backing.u32[0] = UINT32_C(0xb9400000);
    before = backing.u32[0];
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_FAILURE
              && backing.u32[0] == before,
          "nonzero computed low12 scale mismatch changed instruction bytes");
    CHECK(has_owner_error(LL_E_FORMAT),
          "nonzero computed low12 scale mismatch lost format status");

    relocation.r_addend = (int64_t)(((uintptr_t)0 - (source & (uintptr_t)0xfff))
                                    & (uintptr_t)0xfff);
    backing.u32[0] = UINT32_C(0xb9400400);
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_SUCCESS
              && owner.error_status == LL_OK
              && backing.u32[0] == UINT32_C(0xb9400000),
          "zero computed low12 offset did not retain the legacy scale bypass");
}

static void test_branch_stub_fallback(void)
{
    union Backing backing;
    ObjectCode oc;
    struct ObjectCodeFormatInfo info;
    struct SectionFormatInfo section_info;
    Section section;
    ElfSymbolTable symbol_table;
    ElfSymbol symbols[2];
    Elf_Shdr header = {0};
    Elf_Rela relocation = {0};
    ElfRelocationATable table;
    Stub stub = {0};
    uintptr_t place_bits = 0;
    uintptr_t far_target_bits;
    const uintptr_t reach = (uintptr_t)1 << 29;

    setup_object(&oc, &info, &section, &section_info, &symbol_table,
                 symbols, &backing);
    setup_rela(&info, &table, &header, &relocation, 1);
    place_bits = (uintptr_t)(void *)&backing.u64[0];
    far_target_bits = place_bits > reach ? place_bits - reach
                                         : place_bits + reach;
    relocation.r_info = ELF64_R_INFO(1, COMPAT_R_AARCH64_CALL26);
    symbols[1].addr = (ll_symbol_addr *)(uintptr_t)far_target_bits;
    stub.target = symbols[1].addr;
    stub.addr = (void *)&backing.u32[1];
    section_info.stubs = &stub;
    section_info.nstubs = 1;
    backing.u32[0] = UINT32_C(0x94000000);
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_SUCCESS
              && owner.error_status == LL_OK
              && backing.u32[0] == UINT32_C(0x94000001),
          "near cached branch stub did not resolve a far target");

    symbols[1].addr = (ll_symbol_addr *)far_target_bits;
    stub.target = symbols[1].addr;
    stub.addr = (void *)far_target_bits;
    backing.u32[0] = UINT32_C(0x94000000);
    clear_owner_error();
    CHECK(relocateObjectCodeAarch64(&oc) == EXIT_FAILURE
              && backing.u32[0] == UINT32_C(0x94000000),
          "out-of-reach cached branch stub failure changed instruction bytes");
    CHECK(has_owner_error(LL_E_RELOC_RANGE),
          "out-of-reach branch stub lost range status or diagnostic");
}

int main(void)
{
    owner.services.host.fatal = fail_on_fatal;
    ll_default_linker.services.host.fatal = fail_on_default_fatal;
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, "aarch64-default-sentinel",
           sizeof("aarch64-default-sentinel"));

    test_scalar_encodings();
    test_branch_and_adrp_boundaries();
    test_scaled_instruction_encoding();
    test_relocation_loop_statuses();
    test_full_loop_scalar64();
    test_scaled_address_failures();
    test_branch_stub_fallback();

    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && strcmp(ll_default_linker.error_message,
                        "aarch64-default-sentinel") == 0,
          "operation changed the default-linker error sentinel");
    CHECK(fatal_calls == 0, "operation called the fatal callback");
    CHECK(default_fatal_calls == 0,
          "operation called the default-linker fatal callback");
    if (failures != 0) return 1;
    (void)puts("AArch64 ELF relocation errors: PASS");
    return 0;
}
