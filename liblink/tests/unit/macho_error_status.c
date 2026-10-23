#define _DARWIN_C_SOURCE 1
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"

#if !defined(darwin_HOST_OS) || !defined(aarch64_HOST_ARCH) || !defined(OBJFORMAT_MACHO)
#include <stdio.h>
int main(void)
{
    (void)puts("SKIP: Mach-O AArch64 relocation status test requires native arm64 Darwin");
    return 0;
}
#else
#include "linker/ProddableBlocks.h"
#include "linker/MachO.c"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "macho_error_status:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

enum { HOST_CAPACITY = 192 };
typedef struct {
    void *owned[HOST_CAPACITY];
    size_t live;
    unsigned logs;
    unsigned fatals;
} Host;

static Host owner_host;
static Host default_host;
static unsigned failures;

static void remember(Host *host, void *pointer)
{
    CHECK(pointer != NULL, "linker host allocation returned NULL");
    CHECK(host->live < HOST_CAPACITY, "host allocation ledger is full");
    if (pointer != NULL && host->live < HOST_CAPACITY)
        host->owned[host->live++] = pointer;
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    void *pointer = malloc(size == 0 ? 1 : size);
    remember(host, pointer);
    return pointer;
}

static void *host_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    void *pointer = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    remember(host, pointer);
    return pointer;
}

static void *host_realloc(void *ctx, void *pointer, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    if (pointer == NULL) return host_malloc(ctx, size, what);
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == pointer) {
            void *resized = realloc(pointer, size == 0 ? 1 : size);
            if (resized != NULL) host->owned[i] = resized;
            return resized;
        }
    }
    CHECK(false, "realloc pointer was not allocated by owner host");
    return NULL;
}

static void host_free(void *ctx, void *pointer)
{
    Host *host = ctx;
    if (pointer == NULL) return;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == pointer) {
            host->owned[i] = host->owned[--host->live];
            free(pointer);
            return;
        }
    }
    CHECK(false, "free pointer was not allocated by owner host");
}

static void host_log(void *ctx, int kind, const char *format, va_list args)
{
    Host *host = ctx;
    char message[512];
    int written = vsnprintf(message, sizeof(message), format, args);
    CHECK(written >= 0 && (size_t)written < sizeof(message), "host log formatting failed");
    (void)kind;
    ++host->logs;
}

static _Noreturn void host_fatal(void *ctx, const char *format, va_list args)
{
    Host *host = ctx;
    char message[512];
    (void)vsnprintf(message, sizeof(message), format, args);
    ++host->fatals;
    (void)fprintf(stderr, "unexpected linker fatal: %s\n", message);
    _Exit(98);
}

static size_t host_page_size(void *ctx)
{
    (void)ctx;
    return 4096;
}

static ll_compat_host_ops make_ops(Host *host)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = host;
    ops.malloc_ = host_malloc;
    ops.calloc_ = host_calloc;
    ops.realloc_ = host_realloc;
    ops.free_ = host_free;
    ops.log = host_log;
    ops.fatal = host_fatal;
    ops.page_size = host_page_size;
    return ops;
}

static void check_status(ll_linker *owner, ll_status expected, const char *message)
{
    CHECK(owner->error_status == expected, message);
    CHECK(owner->error_message[0] != '\0', "failed operation left no diagnostic");
}

static void clear_owner_error(ll_linker *owner)
{
    ll_error_clear(owner);
}

static void test_decode_encode(ll_linker *owner)
{
    uint64_t storage = UINT64_C(0xAABBCCDDEEFF0000);
    Section section = {0};
    ObjectCode oc = {0};
    MachORelocationInfo relocation = {0};
    int64_t decoded = 123;
    oc.L = owner;
    oc.fileName = "synthetic-arm64-relocations.o";
    section.start = &storage;
    section.size = sizeof(storage);
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(owner, &oc.proddables, &storage, sizeof(storage));

    relocation.r_type = ARM64_RELOC_UNSIGNED;
    relocation.r_length = 3;
    clear_owner_error(owner);
    CHECK(decodeAddend(&oc, &section, &relocation, &decoded),
          "valid UNSIGNED addend did not decode");
    CHECK(decoded == (int64_t)UINT64_C(0xAABBCCDDEEFF0000),
          "UNSIGNED addend decoded to the wrong value");
    CHECK(owner->error_status == LL_OK, "valid decode left an error status");

    relocation.r_type = (uint8_t)15;
    decoded = 123;
    clear_owner_error(owner);
    CHECK(!decodeAddend(&oc, &section, &relocation, &decoded),
          "unknown relocation type decoded successfully");
    check_status(owner, LL_E_UNSUPPORTED, "unknown decode status was not UNSUPPORTED");
    CHECK(decoded == 123, "failed decode changed the caller's output");

    relocation.r_type = ARM64_RELOC_UNSIGNED;
    relocation.r_length = 2;
    storage = UINT64_C(0x1122334400000000);
    clear_owner_error(owner);
    CHECK(encodeAddend(&oc, &section, &relocation, INT64_C(0x12345678), NULL),
          "valid UNSIGNED addend did not encode");
    const unsigned char expected32[4] = {0x78, 0x56, 0x34, 0x12};
    CHECK(memcmp(&storage, expected32, sizeof(expected32)) == 0,
          "UNSIGNED encoding did not produce the expected little-endian bytes");
    CHECK(owner->error_status == LL_OK, "valid encode left an error status");

    unsigned char byte = 0xA5;
    section.start = &byte;
    section.size = sizeof(byte);
    addProddableBlock(owner, &oc.proddables, &byte, sizeof(byte));
    relocation.r_length = 0;
    clear_owner_error(owner);
    CHECK(encodeAddend(&oc, &section, &relocation, 127, NULL),
          "valid positive byte addend did not encode");
    CHECK(byte == 0x7F, "positive byte addend had the wrong literal encoding");
    byte = 0xA5;
    clear_owner_error(owner);
    CHECK(encodeAddend(&oc, &section, &relocation, -128, NULL),
          "valid negative byte addend did not encode");
    CHECK(byte == 0x80, "negative byte addend had the wrong literal encoding");
    byte = 0xA5;
    clear_owner_error(owner);
    CHECK(!encodeAddend(&oc, &section, &relocation, 256, NULL),
          "out-of-range byte relocation encoded successfully");
    check_status(owner, LL_E_RELOC_RANGE, "encode range error was not RELOC_RANGE");
    CHECK(byte == 0xA5, "rejected encode modified its target byte");

    relocation.r_type = (uint8_t)15;
    clear_owner_error(owner);
    CHECK(!encodeAddend(&oc, &section, &relocation, 1, NULL),
          "unknown relocation type encoded successfully");
    check_status(owner, LL_E_UNSUPPORTED, "unknown encode status was not UNSUPPORTED");
    CHECK(byte == 0xA5, "unsupported encode modified its target byte");
    freeProddableBlocks(owner, &oc.proddables);
}

typedef struct {
    uint8_t type;
    uint8_t length;
    int64_t positive_overflow;
    int64_t negative_overflow;
    const char *name;
} EncodeRangeCase;

static void test_encode_range_matrix(ll_linker *owner)
{
    const int64_t branch_limit = INT64_C(1) << 28;
    const int64_t page_limit = INT64_C(1) << 33;
    const int64_t page_negative_overflow = -((INT64_C(1) << 21) + 1) * 4096;
    const EncodeRangeCase cases[] = {
        {ARM64_RELOC_UNSIGNED, 0, 256, -257, "UNSIGNED"},
        {ARM64_RELOC_SUBTRACTOR, 0, 256, -257, "SUBTRACTOR"},
        {ARM64_RELOC_BRANCH26, 2, branch_limit, -branch_limit - 4, "BRANCH26"},
        {ARM64_RELOC_PAGE21, 2, page_limit, page_negative_overflow, "PAGE21"},
        {ARM64_RELOC_GOT_LOAD_PAGE21, 2, page_limit, page_negative_overflow,
         "GOT_LOAD_PAGE21"},
        {ARM64_RELOC_PAGEOFF12, 2, 4096, -4097, "PAGEOFF12"},
        {ARM64_RELOC_GOT_LOAD_PAGEOFF12, 2, 4096, -4097,
         "GOT_LOAD_PAGEOFF12"},
    };
    _Alignas(uint64_t) unsigned char aligned_storage[24];

    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        const EncodeRangeCase *test = &cases[i];
        for (size_t polarity = 0; polarity < 2; ++polarity) {
            memset(aligned_storage, 0xA5, sizeof(aligned_storage));
            uint32_t *target = (uint32_t *)(void *)(aligned_storage + 8);
            Section section = {0};
            ObjectCode oc = {0};
            MachORelocationInfo relocation = {0};
            oc.L = owner;
            oc.fileName = "synthetic-arm64-relocations.o";
            section.start = target;
            section.size = sizeof(uint32_t);
            relocation.r_type = test->type;
            relocation.r_length = test->length;
            initProddableBlockSet(&oc.proddables);
            addProddableBlock(owner, &oc.proddables, aligned_storage,
                              sizeof(aligned_storage));
            clear_owner_error(owner);
            int64_t value = polarity == 0 ? test->positive_overflow
                                          : test->negative_overflow;
            CHECK(!encodeAddend(&oc, &section, &relocation, value, NULL),
                  "out-of-range addend was accepted by encoder");
            check_status(owner, LL_E_RELOC_RANGE, test->name);
            for (size_t byte_index = 0; byte_index < sizeof(aligned_storage);
                 ++byte_index)
                CHECK(aligned_storage[byte_index] == 0xA5,
                      "range rejection changed target bytes or canaries");
            freeProddableBlocks(owner, &oc.proddables);
        }
    }
}

static void test_unresolved_symbol(ll_linker *owner)
{
    MachONList nlist = {0};
    MachOSymbol symbol = {0};
    ObjectCode oc = {0};
    uint64_t result = UINT64_C(0xfeedfacecafebeef);
    nlist.n_type = N_UNDF | N_EXT;
    symbol.name = (ll_symbol_name *)"_wp24_missing_relocation_status_fixture";
    symbol.nlist = &nlist;
    oc.L = owner;
    oc.fileName = "synthetic-arm64-relocations.o";
    clear_owner_error(owner);
    CHECK(!symbol_value(&oc, &symbol, &result),
          "missing external symbol resolved successfully");
    check_status(owner, LL_E_UNRESOLVED, "missing symbol status was not UNRESOLVED");
    CHECK(result == UINT64_C(0xfeedfacecafebeef),
          "failed symbol lookup changed the caller's output");

    MachONList local_nlist = {0};
    MachOSymbol local_symbol = {0};
    local_nlist.n_type = N_SECT;
    local_symbol.name = (ll_symbol_name *)"_local_symbol_value_control";
    local_symbol.addr = (ll_symbol_addr *)(uintptr_t)UINT64_C(0x12345678);
    local_symbol.nlist = &local_nlist;
    result = 0;
    clear_owner_error(owner);
    CHECK(symbol_value(&oc, &local_symbol, &result),
          "local symbol_value path did not succeed");
    CHECK(result == UINT64_C(0x12345678), "local symbol_value returned wrong address");
    CHECK(owner->error_status == LL_OK, "local symbol_value left an error status");
}

static void run_relocation_case(ll_linker *owner, uint8_t type, uint8_t length,
                                int32_t explicit_symbol, uint32_t target_word,
                                MachOSymbol *symbols, size_t symbol_count,
                                size_t relocation_count, ll_status expected,
                                uint32_t expected_word, const char *message)
{
    _Alignas(16) uint32_t backing[7];
    uint32_t before[7];
    for (size_t i = 0; i < sizeof(backing) / sizeof(backing[0]); ++i)
        backing[i] = UINT32_C(0xCAFE0000) + (uint32_t)i;
    uint32_t *word = &backing[1];
    *word = target_word;
    memcpy(before, backing, sizeof(before));
    MachORelocationInfo relocations[2] = {{0}, {0}};
    MachOSection macho_section = {0};
    struct SectionFormatInfo section_info = {0};
    struct ObjectCodeFormatInfo object_info = {0};
    Section section = {0};
    ObjectCode oc = {0};

    relocations[0].r_address = 0;
    relocations[0].r_symbolnum = (uint32_t)explicit_symbol;
    relocations[0].r_pcrel = 0;
    relocations[0].r_length = length;
    relocations[0].r_extern = 1;
    relocations[0].r_type = type;
    if (relocation_count > 1) {
        relocations[1].r_address = 0;
        relocations[1].r_symbolnum = 0;
        relocations[1].r_length = length;
        relocations[1].r_extern = 1;
        relocations[1].r_type = ARM64_RELOC_PAGEOFF12;
    }
    if (type == ARM64_RELOC_BRANCH26 && symbol_count != 0
        && !(symbols[0].nlist->n_type & N_EXT))
        symbols[0].addr = (ll_symbol_addr *)(void *)(word + 4);
    macho_section.nreloc = (uint32_t)relocation_count;
    section_info.macho_section = &macho_section;
    section_info.relocation_info = relocations;
    object_info.macho_symbols = symbols;
    object_info.n_macho_symbols = symbol_count;
    section.start = word;
    section.size = sizeof(*word);
    section.info = &section_info;
    oc.L = owner;
    oc.fileName = "synthetic-arm64-relocations.o";
    oc.info = &object_info;
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(owner, &oc.proddables, word, sizeof(*word));

    clear_owner_error(owner);
    int result = relocateSectionAarch64(&oc, &section);
    if (expected == LL_OK) {
        CHECK(result == 1, message);
        CHECK(owner->error_status == LL_OK, "successful relocation left error status");
    } else {
        CHECK(result == 0, message);
        check_status(owner, expected, "relocation failure had the wrong status");
        CHECK(*word == target_word, "failed relocation modified its target word");
    }
    if (expected == LL_OK)
        CHECK(*word == expected_word,
              "successful relocation wrote unexpected instruction bytes");
    for (size_t i = 0; i < sizeof(backing) / sizeof(backing[0]); ++i) {
        if (i != 1)
            CHECK(backing[i] == before[i],
                  "relocation changed target-adjacent storage or canary");
    }
    freeProddableBlocks(owner, &oc.proddables);
}

static void run_valid_subtractor_pair(ll_linker *owner)
{
    uint32_t word = UINT32_C(7);
    MachORelocationInfo relocations[2] = {{0}, {0}};
    MachOSection macho_section = {0};
    struct SectionFormatInfo section_info = {0};
    struct ObjectCodeFormatInfo object_info = {0};
    MachONList nlists[2] = {{0}, {0}};
    MachOSymbol symbols[2] = {{0}, {0}};
    Section section = {0};
    ObjectCode oc = {0};

    nlists[0].n_type = N_SECT;
    nlists[1].n_type = N_SECT;
    symbols[0].name = (ll_symbol_name *)"_subtractor_base";
    symbols[0].addr = (ll_symbol_addr *)(uintptr_t)100;
    symbols[0].nlist = &nlists[0];
    symbols[1].name = (ll_symbol_name *)"_subtractor_target";
    symbols[1].addr = (ll_symbol_addr *)(uintptr_t)150;
    symbols[1].nlist = &nlists[1];
    relocations[0].r_length = 2;
    relocations[0].r_extern = 1;
    relocations[0].r_type = ARM64_RELOC_SUBTRACTOR;
    relocations[1].r_symbolnum = 1;
    relocations[1].r_length = 2;
    relocations[1].r_extern = 1;
    relocations[1].r_type = ARM64_RELOC_UNSIGNED;
    macho_section.nreloc = 2;
    section_info.macho_section = &macho_section;
    section_info.relocation_info = relocations;
    object_info.macho_symbols = symbols;
    object_info.n_macho_symbols = 2;
    section.start = &word;
    section.size = sizeof(word);
    section.info = &section_info;
    oc.L = owner;
    oc.fileName = "synthetic-arm64-relocations.o";
    oc.info = &object_info;
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(owner, &oc.proddables, &word, sizeof(word));

    clear_owner_error(owner);
    CHECK(relocateSectionAarch64(&oc, &section) == 1,
          "valid SUBTRACTOR/UNSIGNED pair did not propagate success");
    CHECK(owner->error_status == LL_OK, "valid SUBTRACTOR pair left an error status");
    CHECK(word == UINT32_C(57), "valid SUBTRACTOR pair encoded the wrong word");
    freeProddableBlocks(owner, &oc.proddables);
}

static void test_relocation_loop(ll_linker *owner)
{
    MachONList local_nlist = {0};
    MachOSymbol local = {0};
    MachONList missing_nlist = {0};
    MachOSymbol missing = {0};
    local_nlist.n_type = N_SECT;
    local.name = (ll_symbol_name *)"_local_branch_target";
    local.addr = NULL;
    local.nlist = &local_nlist;
    run_relocation_case(owner, ARM64_RELOC_BRANCH26, 2, 0,
                        UINT32_C(0x14000000), &local, 1, 1, LL_OK,
                        UINT32_C(0x14000004),
                        "valid local BRANCH26 did not propagate success");
    local.addr = NULL;

    run_relocation_case(owner, ARM64_RELOC_SUBTRACTOR, 2, 0,
                        UINT32_C(0x11223344), &local, 1, 1, LL_E_FORMAT, 0,
                        "unpaired SUBTRACTOR did not propagate FORMAT");
    run_relocation_case(owner, ARM64_RELOC_ADDEND, 2, 1,
                        UINT32_C(0x11223344), &local, 1, 1, LL_E_FORMAT, 0,
                        "unpaired ADDEND did not propagate FORMAT");

    run_relocation_case(owner, ARM64_RELOC_ADDEND, 2, 1,
                        UINT32_C(0x00000400), &local, 1, 2, LL_E_FORMAT, 0,
                        "conflicting embedded/explicit addends did not propagate FORMAT");
    run_valid_subtractor_pair(owner);
    run_relocation_case(owner, ARM64_RELOC_ADDEND, 2, 1,
                        UINT32_C(0x91000000), &local, 1, 2, LL_OK,
                        UINT32_C(0x91000400),
                        "valid ADDEND/PAGEOFF12 pair did not propagate success");
    run_relocation_case(owner, (uint8_t)15, 2, 0,
                        UINT32_C(0x12345678), &local, 1, 1, LL_E_UNSUPPORTED, 0,
                        "unknown relocation did not propagate UNSUPPORTED");

    local.addr = (ll_symbol_addr *)(uintptr_t)1000;
    run_relocation_case(owner, ARM64_RELOC_UNSIGNED, 0, 0,
                        UINT32_C(0xA5A5A5A5), &local, 1, 1, LL_E_RELOC_RANGE, 0,
                        "byte-width overflow did not propagate RELOC_RANGE");

    missing_nlist.n_type = N_UNDF | N_EXT;
    missing.name = (ll_symbol_name *)"_wp24_missing_loop_relocation_fixture";
    missing.nlist = &missing_nlist;
    run_relocation_case(owner, ARM64_RELOC_UNSIGNED, 2, 0,
                        UINT32_C(0xA5A5A5A5), &missing, 1, 1, LL_E_UNRESOLVED, 0,
                        "missing symbol did not propagate UNRESOLVED");
    run_relocation_case(owner, ARM64_RELOC_BRANCH26, 2, 0,
                        UINT32_C(0x14000000), &missing, 1, 1, LL_E_UNRESOLVED, 0,
                        "missing external branch did not propagate UNRESOLVED");
}

int main(void)
{
    ll_linker owner = {0};
    ll_compat_host_ops owner_ops = make_ops(&owner_host);
    ll_compat_host_ops default_ops = make_ops(&default_host);
    char default_message[] = "default-owner-status-sentinel";

    ll_install_host(&default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_message, sizeof(default_message));
    ll_init_linker(&owner, 0);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);

    test_decode_encode(&owner);
    test_encode_range_matrix(&owner);
    test_unresolved_symbol(&owner);
    test_relocation_loop(&owner);

    CHECK(owner_host.logs > 0, "negative paths did not use the owner's log callback");
    CHECK(owner_host.fatals == 0, "expected status failure reached fatal callback");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && memcmp(ll_default_linker.error_message, default_message,
                        sizeof(default_message)) == 0,
          "owner operation modified default linker error state");
    CHECK(default_host.logs == 0 && default_host.fatals == 0,
          "owner operation used default host callbacks");
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    CHECK(owner_host.live == 0, "linker exit left owner allocations live");
    return failures == 0 ? 0 : 1;
}
#endif
