#define _DARWIN_C_SOURCE 1
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#if defined(OBJFORMAT_MACHO)
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <mach-o/arm64/reloc.h>
#include <mach-o/x86_64/reloc.h>
#endif

#if !defined(darwin_HOST_OS) || !defined(x86_64_HOST_ARCH) || !defined(OBJFORMAT_MACHO)
#include <stdio.h>
int main(void)
{
    (void)puts("SKIP: Mach-O x86_64 relocation status test requires x86_64 Darwin");
    return 0;
}
#else
#include "linker/ProddableBlocks.h"
#include "linker/MachO.c"

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "macho_x86_error_status:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

enum { HOST_CAPACITY = 128 };
enum { FIXTURE_BYTES = 8192 };

typedef struct {
    _Alignas(16) uint32_t words[FIXTURE_BYTES / sizeof(uint32_t)];
    SymbolExtra extras[1];
} RelocationStorage;

typedef enum {
    EXPECT_LITERAL,
    EXPECT_INTERNAL_TARGET_LOW32,
    EXPECT_GOT_SLOT_DISPLACEMENT,
    EXPECT_ISLAND_DISPLACEMENT
} ExpectedWordKind;

typedef enum {
    SYMBOL_LITERAL,
    SYMBOL_RELATIVE_TO_RELOCATION,
    SYMBOL_DEFAULT_HOST
} SymbolAddressKind;

typedef struct {
    void *owned[HOST_CAPACITY];
    size_t live;
    unsigned logs;
    unsigned fatals;
} Host;

typedef struct {
    const char *name;
    uint8_t type;
    uint8_t pcrel;
    uint8_t is_external;
    uint32_t symbol_number;
    uint8_t symbol_type;
    uintptr_t symbol_address;
    uint32_t initial_word;
    uint32_t expected_word;
    ll_status expected_status;
    uint64_t source_address;
    uint64_t target_address;
    int add_proddable_span;
    int32_t relative_symbol_offset;
    int use_symbol_extra;
    int heap_symbol_extra;
    int separate_target;
    int include_target_span;
    SymbolAddressKind symbol_address_kind;
    ExpectedWordKind expected_word_kind;
} RelocationCase;

static Host owner_host;
static Host default_host;
static ll_linker owner;
static unsigned failures;
static char default_message[] = "x86-default-owner-sentinel";

static void remember(Host *host, void *pointer)
{
    CHECK(pointer != NULL, "owner allocation returned NULL");
    CHECK(host->live < HOST_CAPACITY, "owner allocation ledger is full");
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
    CHECK(written >= 0 && (size_t)written < sizeof(message),
          "owner log formatting failed or truncated");
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

static void run_case(const RelocationCase *test)
{
    RelocationStorage storage;
    unsigned char target_backing[32];
    unsigned char original[FIXTURE_BYTES];
    unsigned char original_target[sizeof(target_backing)];
    MachORelocationInfo relocation = {0};
    MachOSection macho_sections[2] = {{0}, {0}};
    struct SectionFormatInfo section_info[2] = {{0}, {0}};
    struct ObjectCodeFormatInfo object_info = {0};
    MachONList nlist = {0};
    MachOSymbol symbol = {0};
    Section sections[2] = {{0}, {0}};
    ObjectCode oc = {0};
    unsigned char *backing = (unsigned char *)storage.words;
    unsigned char *target_start;
    SymbolExtra *heap_extra = NULL;

    memset(&storage, 0xA5, sizeof(storage));
    memset(target_backing, 0x5A, sizeof(target_backing));
    memcpy(original_target, target_backing, sizeof(target_backing));
    memset(backing, 0xA5, FIXTURE_BYTES);
    memcpy(backing + 8, &test->initial_word, sizeof(test->initial_word));
    memcpy(original, backing, sizeof(original));

    relocation.r_address = 0;
    relocation.r_symbolnum = test->symbol_number;
    relocation.r_pcrel = test->pcrel;
    relocation.r_length = 2;
    relocation.r_extern = test->is_external;
    relocation.r_type = test->type;
    macho_sections[0].nreloc = 1;
    macho_sections[0].addr = test->source_address;
    macho_sections[1].addr = test->target_address;
    (void)memcpy(macho_sections[0].sectname, "__fixture", sizeof("__fixture"));
    (void)memcpy(macho_sections[1].sectname, "__target", sizeof("__target"));

    section_info[0].macho_section = &macho_sections[0];
    section_info[0].relocation_info = &relocation;
    section_info[1].macho_section = &macho_sections[1];
    sections[0].start = backing + 8;
    sections[0].size = sizeof(test->initial_word);
    sections[0].info = &section_info[0];
    target_start = test->separate_target ? target_backing + 8 : backing + 16;
    sections[1].start = target_start;
    sections[1].size = 8;
    sections[1].info = &section_info[1];

    nlist.n_type = test->symbol_type;
    symbol.name = (ll_symbol_name *)"_wp24_x86_relocation_fixture";
    if (test->symbol_address_kind == SYMBOL_LITERAL)
        symbol.addr = (ll_symbol_addr *)test->symbol_address;
    else if (test->symbol_address_kind == SYMBOL_DEFAULT_HOST)
        symbol.addr = (ll_symbol_addr *)(void *)&default_host;
    else {
        int32_t instruction_adjustment = 0;
        if (test->type == X86_64_RELOC_SIGNED_1) instruction_adjustment = 1;
        if (test->type == X86_64_RELOC_SIGNED_2) instruction_adjustment = 2;
        if (test->type == X86_64_RELOC_SIGNED_4) instruction_adjustment = 4;
        symbol.addr = (ll_symbol_addr *)(uintptr_t)
            (backing + 8 + 4 + instruction_adjustment +
             test->relative_symbol_offset);
    }
    symbol.nlist = &nlist;
    object_info.macho_symbols = &symbol;
    object_info.n_macho_symbols = 1;
    oc.L = &owner;
    oc.info = &object_info;
    oc.sections = sections;
    oc.n_sections = 2;
    oc.fileName = "synthetic-x86-relocations.o";
    if (test->use_symbol_extra) {
        if (test->heap_symbol_extra) {
            heap_extra = host_malloc(&owner_host, sizeof(*heap_extra),
                                     "test branch island storage");
            if (heap_extra == NULL) {
                CHECK(false, "could not allocate real branch-island storage");
                return;
            }
            memset(heap_extra, 0, sizeof(*heap_extra));
            oc.symbol_extras = heap_extra;
        } else {
            oc.symbol_extras = storage.extras;
        }
        oc.first_symbol_extra = 0;
        oc.n_symbol_extras = 1;
        oc.symbol_extra_stride = sizeof(SymbolExtra);
    }
    initProddableBlockSet(&oc.proddables);
    if (test->include_target_span)
        addProddableBlock(&owner, &oc.proddables, backing, FIXTURE_BYTES);
    else if (test->add_proddable_span)
        addProddableBlock(&owner, &oc.proddables, backing + 8,
                          sizeof(test->initial_word));

    if (test->symbol_address_kind == SYMBOL_DEFAULT_HOST &&
        test->type == X86_64_RELOC_BRANCH) {
        uintptr_t base = (uintptr_t)(backing + 12);
        uintptr_t target = (uintptr_t)symbol.addr;
        uintptr_t island = (uintptr_t)&oc.symbol_extras[0].jumpIsland;
        int64_t target_delta = (int64_t)target - (int64_t)base;
        int64_t island_delta = (int64_t)island - (int64_t)base;
        int target_is_far = target_delta < INT32_MIN || target_delta > INT32_MAX;
        int island_is_far = island_delta < INT32_MIN || island_delta > INT32_MAX;
        if (!target_is_far || (test->expected_status == LL_E_RELOC_RANGE &&
                              !island_is_far)) {
            (void)fprintf(stderr,
                          "macho_x86_error_status: SKIP branch island reach case; "
                          "actual target/island deltas=%" PRId64 "/%" PRId64 "\n",
                          target_delta, island_delta);
            freeProddableBlocks(&owner, &oc.proddables);
            if (heap_extra != NULL) host_free(&owner_host, heap_extra);
            return;
        }
        (void)fprintf(stderr,
                      "macho_x86_error_status: exercised %s with target/island "
                      "deltas=%" PRId64 "/%" PRId64 "\n",
                      test->expected_status == LL_E_RELOC_RANGE
                          ? "branch-island range failure"
                          : "nearby branch island control",
                      target_delta, island_delta);
    }

    ll_error_clear(&owner);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    int result = relocateSection(&oc, 0);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);

    if (test->expected_status == LL_OK) {
        if (result != 1 || owner.error_status != LL_OK)
            (void)fprintf(stderr,
                          "macho_x86_error_status: %s returned %d, status=%d, diagnostic=%s\n",
                          test->name, result, (int)owner.error_status,
                          owner.error_message);
        CHECK(result == 1, test->name);
        CHECK(owner.error_status == LL_OK,
              "successful relocation changed owner status");
        uint32_t actual_word = 0;
        memcpy(&actual_word, backing + 8, sizeof(actual_word));
        uint32_t expected_word = test->expected_word;
        if (test->expected_word_kind == EXPECT_INTERNAL_TARGET_LOW32)
            expected_word = (uint32_t)(uintptr_t)(target_start + 4);
        else if (test->expected_word_kind == EXPECT_GOT_SLOT_DISPLACEMENT)
            expected_word = (uint32_t)(offsetof(RelocationStorage, extras) +
                                       offsetof(SymbolExtra, addr) - 12U);
        else if (test->expected_word_kind == EXPECT_ISLAND_DISPLACEMENT)
            expected_word = (uint32_t)(offsetof(RelocationStorage, extras) +
                                       offsetof(SymbolExtra, jumpIsland) - 12U);
        CHECK(actual_word == expected_word,
              "successful relocation wrote the wrong literal word");
        for (size_t i = 0; i < sizeof(original); ++i) {
            if (i < 8 || i >= 12)
                CHECK(backing[i] == original[i],
                      "successful relocation changed target canaries");
        }
        if (test->use_symbol_extra &&
            (test->expected_word_kind == EXPECT_GOT_SLOT_DISPLACEMENT ||
             test->expected_word_kind == EXPECT_ISLAND_DISPLACEMENT)) {
            static const unsigned char expected_island[8] = {
                0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF, 0x00, 0x00
            };
            CHECK(memcmp(storage.extras[0].jumpIsland, expected_island,
                         sizeof(expected_island)) == 0,
                  "SymbolExtra did not contain the literal x86 jump island");
        }
        if (test->symbol_address_kind == SYMBOL_DEFAULT_HOST)
            CHECK(storage.extras[0].addr == (unsigned long)&default_host,
                  "branch island did not retain the real target address");
    } else {
        CHECK(result == 0, test->name);
        CHECK(owner.error_status == test->expected_status,
              "failure returned the wrong owner-local status");
        CHECK(owner.error_message[0] != '\0',
              "failed relocation recorded no diagnostic");
        CHECK(memcmp(backing, original, sizeof(original)) == 0,
              "failed relocation changed the target or canaries");
        CHECK(memcmp(target_backing, original_target,
                     sizeof(target_backing)) == 0,
              "failed relocation changed separate target storage");
    }
    freeProddableBlocks(&owner, &oc.proddables);
    if (heap_extra != NULL) host_free(&owner_host, heap_extra);
}

static void test_relocation_statuses(void)
{
    const RelocationCase cases[] = {
#define CASE(name_, type_, pcrel_, ext_, symnum_, symtype_, symaddr_, initial_, expected_, status_) \
        {.name = (name_), .type = (type_), .pcrel = (pcrel_), \
         .is_external = (ext_), .symbol_number = (symnum_), \
         .symbol_type = (symtype_), .symbol_address = (uintptr_t)(symaddr_), \
         .initial_word = (initial_), .expected_word = (expected_), \
         .expected_status = (status_), .add_proddable_span = 1}
        CASE("UNSIGNED external control", X86_64_RELOC_UNSIGNED, 0, 1, 0,
             N_SECT, UINT32_C(0x12345678), UINT32_C(0x01020304),
             UINT32_C(0x1336597C), LL_OK),
        {.name = "SIGNED external control", .type = X86_64_RELOC_SIGNED,
         .pcrel = 1, .is_external = 1, .symbol_number = 0, .symbol_type = N_SECT,
         .initial_word = 0, .expected_word = UINT32_C(0x1234),
         .expected_status = LL_OK, .add_proddable_span = 1,
         .relative_symbol_offset = 0x1234,
         .symbol_address_kind = SYMBOL_RELATIVE_TO_RELOCATION},
        {.name = "SIGNED_1 external control", .type = X86_64_RELOC_SIGNED_1,
         .pcrel = 1, .is_external = 1, .symbol_number = 0, .symbol_type = N_SECT,
         .initial_word = 0, .expected_word = UINT32_C(0x1234),
         .expected_status = LL_OK, .add_proddable_span = 1,
         .relative_symbol_offset = 0x1234,
         .symbol_address_kind = SYMBOL_RELATIVE_TO_RELOCATION},
        {.name = "SIGNED_2 external control", .type = X86_64_RELOC_SIGNED_2,
         .pcrel = 1, .is_external = 1, .symbol_number = 0, .symbol_type = N_SECT,
         .initial_word = 0, .expected_word = UINT32_C(0x1234),
         .expected_status = LL_OK, .add_proddable_span = 1,
         .relative_symbol_offset = 0x1234,
         .symbol_address_kind = SYMBOL_RELATIVE_TO_RELOCATION},
        {.name = "SIGNED_4 external control", .type = X86_64_RELOC_SIGNED_4,
         .pcrel = 1, .is_external = 1, .symbol_number = 0, .symbol_type = N_SECT,
         .initial_word = 0, .expected_word = UINT32_C(0x1234),
         .expected_status = LL_OK, .add_proddable_span = 1,
         .relative_symbol_offset = 0x1234,
         .symbol_address_kind = SYMBOL_RELATIVE_TO_RELOCATION},
        {.name = "BRANCH nearby external control", .type = X86_64_RELOC_BRANCH,
         .pcrel = 1, .is_external = 1, .symbol_number = 0, .symbol_type = N_SECT,
         .initial_word = 0, .expected_word = UINT32_C(0x100),
         .expected_status = LL_OK, .add_proddable_span = 1,
         .relative_symbol_offset = 0x100,
         .symbol_address_kind = SYMBOL_RELATIVE_TO_RELOCATION},
        CASE("SUBTRACTOR external control", X86_64_RELOC_SUBTRACTOR, 0, 1, 0,
             N_SECT, UINT32_C(0x12340000), UINT32_C(0x12345678),
             UINT32_C(0x00005678), LL_OK),
        {.name = "GOT external control", .type = X86_64_RELOC_GOT,
         .pcrel = 1, .is_external = 1, .symbol_number = 0, .symbol_type = N_SECT,
         .symbol_address = UINT32_C(0x12345678), .expected_status = LL_OK,
         .add_proddable_span = 1, .use_symbol_extra = 1,
         .expected_word_kind = EXPECT_GOT_SLOT_DISPLACEMENT},
        {.name = "GOT_LOAD external control", .type = X86_64_RELOC_GOT_LOAD,
         .pcrel = 1, .is_external = 1, .symbol_number = 0, .symbol_type = N_SECT,
         .symbol_address = UINT32_C(0x12345678), .expected_status = LL_OK,
         .add_proddable_span = 1, .use_symbol_extra = 1,
         .expected_word_kind = EXPECT_GOT_SLOT_DISPLACEMENT},
        {.name = "BRANCH far symbol uses nearby real island",
         .type = X86_64_RELOC_BRANCH, .pcrel = 1, .is_external = 1,
         .symbol_number = 0, .symbol_type = N_SECT,
         .symbol_address_kind = SYMBOL_DEFAULT_HOST, .use_symbol_extra = 1,
         .expected_status = LL_OK, .add_proddable_span = 1,
         .expected_word_kind = EXPECT_ISLAND_DISPLACEMENT},
        {.name = "internal UNSIGNED control", .type = X86_64_RELOC_UNSIGNED,
         .symbol_number = 2, .initial_word = UINT32_C(0x1004),
         .expected_status = LL_OK, .source_address = 0x2000,
         .target_address = 0x1000, .add_proddable_span = 1,
         .include_target_span = 1,
         .expected_word_kind = EXPECT_INTERNAL_TARGET_LOW32},
        {.name = "internal SIGNED control", .type = X86_64_RELOC_SIGNED,
         .pcrel = 1, .symbol_number = 2, .initial_word = 0,
         .expected_word = 8, .expected_status = LL_OK,
         .source_address = 0x1000, .target_address = 0x1000,
         .add_proddable_span = 1, .include_target_span = 1},
        CASE("unsupported internal relocation", 15, 0, 0, 1, 0, 0,
             UINT32_C(0x11223344), 0, LL_E_UNSUPPORTED),
        CASE("unsupported external relocation", 15, 0, 1, 0, N_SECT,
             UINT32_C(0x12345678), UINT32_C(0x11223344), 0, LL_E_UNSUPPORTED),
        CASE("missing external symbol", X86_64_RELOC_UNSIGNED, 0, 1, 0,
             N_UNDF | N_EXT, 0, UINT32_C(0x11223344), 0, LL_E_UNRESOLVED),
        CASE("missing external GOT symbol", X86_64_RELOC_GOT, 1, 1, 0,
             N_UNDF | N_EXT, 0, UINT32_C(0x11223344), 0, LL_E_UNRESOLVED),
        CASE("unresolved local GOT section symbol", X86_64_RELOC_GOT, 1, 0, 0,
             N_SECT, 0, UINT32_C(0x11223344), 0, LL_E_UNRESOLVED),
        CASE("invalid local GOT symbol type", X86_64_RELOC_GOT, 1, 0, 0,
             N_UNDF, 0, UINT32_C(0x11223344), 0, LL_E_FORMAT),
        CASE("unresolved section symbol", X86_64_RELOC_SIGNED, 1, 1, 0,
             N_SECT, 0, UINT32_C(0x11223344), 0, LL_E_UNRESOLVED),
        CASE("UNSIGNED with PC-relative flag", X86_64_RELOC_UNSIGNED, 1, 1, 0,
             N_SECT, UINT32_C(0x12345678), 0, 0, LL_E_FORMAT),
        CASE("SIGNED without PC-relative flag", X86_64_RELOC_SIGNED, 0, 1, 0,
             N_SECT, UINT32_C(0x12345678), 0, 0, LL_E_FORMAT),
        CASE("SIGNED_1 without PC-relative flag", X86_64_RELOC_SIGNED_1, 0, 1, 0,
             N_SECT, UINT32_C(0x12345678), 0, 0, LL_E_FORMAT),
        CASE("SIGNED_2 without PC-relative flag", X86_64_RELOC_SIGNED_2, 0, 1, 0,
             N_SECT, UINT32_C(0x12345678), 0, 0, LL_E_FORMAT),
        CASE("SIGNED_4 without PC-relative flag", X86_64_RELOC_SIGNED_4, 0, 1, 0,
             N_SECT, UINT32_C(0x12345678), 0, 0, LL_E_FORMAT),
        CASE("SUBTRACTOR with PC-relative flag", X86_64_RELOC_SUBTRACTOR, 1, 1, 0,
             N_SECT, UINT32_C(0x12345678), UINT32_C(0x11223344), 0, LL_E_FORMAT),
        {.name = "internal UNSIGNED displacement before target",
         .type = X86_64_RELOC_UNSIGNED, .symbol_number = 2,
         .initial_word = UINT32_C(0x0FFF), .expected_status = LL_E_FORMAT,
         .source_address = 0x2000, .target_address = 0x1000,
         .add_proddable_span = 1},
        {.name = "internal SIGNED displacement before target",
         .type = X86_64_RELOC_SIGNED, .pcrel = 1, .symbol_number = 2,
         .initial_word = 0, .expected_status = LL_E_FORMAT,
         .source_address = 0x1000, .target_address = 0x2000,
         .add_proddable_span = 1},
        {.name = "internal relocation with zero section index",
         .type = X86_64_RELOC_UNSIGNED, .symbol_number = 0,
         .initial_word = UINT32_C(0x11223344), .expected_status = LL_E_FORMAT,
         .source_address = 0x2000, .target_address = 0x1000,
         .add_proddable_span = 1},
        {.name = "computed internal UNSIGNED target outside proddable span",
         .type = X86_64_RELOC_UNSIGNED, .symbol_number = 2,
         .initial_word = UINT32_C(0x1004), .expected_status = LL_E_FORMAT,
         .source_address = 0x2000, .target_address = 0x1000,
         .add_proddable_span = 1, .separate_target = 1},
        {.name = "computed internal SIGNED target outside proddable span",
         .type = X86_64_RELOC_SIGNED, .pcrel = 1, .symbol_number = 2,
         .initial_word = 0, .expected_status = LL_E_FORMAT,
         .source_address = 0x1000, .target_address = 0x1000,
         .add_proddable_span = 1, .separate_target = 1},
        {.name = "write outside proddable span",
         .type = X86_64_RELOC_UNSIGNED, .is_external = 1,
         .symbol_type = N_SECT, .symbol_address = UINT32_C(0x12345678),
         .initial_word = UINT32_C(0x11223344), .expected_status = LL_E_FORMAT},
        {.name = "branch-island range failure with distant real allocation",
         .type = X86_64_RELOC_BRANCH, .pcrel = 1, .is_external = 1,
         .symbol_number = 0, .symbol_type = N_SECT,
         .symbol_address_kind = SYMBOL_DEFAULT_HOST, .use_symbol_extra = 1,
         .heap_symbol_extra = 1, .expected_status = LL_E_RELOC_RANGE,
         .add_proddable_span = 1},
    };
#undef CASE
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i)
        run_case(&cases[i]);
}

int main(void)
{
    ll_compat_host_ops default_ops = make_ops(&default_host);
    ll_compat_host_ops owner_ops = make_ops(&owner_host);
    ll_install_host(&default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_message, sizeof(default_message));
    ll_init_linker(&owner, 0);

    test_relocation_statuses();
    ll_exit_linker(&owner);

    CHECK(owner_host.logs > 0, "negative cases did not log through owner services");
    CHECK(owner_host.fatals == 0, "recoverable relocation used fatal callback");
    CHECK(owner_host.live == 0, "owner linker teardown leaked host allocations");
    CHECK(default_host.logs == 0, "owner failures used default host logging");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND &&
              memcmp(ll_default_linker.error_message, default_message,
                     sizeof(default_message)) == 0,
          "owner relocation changed default linker error sentinel");
    if (failures != 0) return 1;
    (void)puts("macho_x86_error_status: PASS");
    return 0;
}
#endif
