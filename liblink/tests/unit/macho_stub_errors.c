#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#if defined(OBJFORMAT_MACHO)
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <mach-o/arm64/reloc.h>
#include <mach-o/x86_64/reloc.h>
#endif
#include "linker/MachO.h"
#include "linker/ProddableBlocks.h"
#include "linker/macho/plt.h"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(OBJFORMAT_MACHO) || !defined(aarch64_HOST_ARCH)
#error "macho_stub_errors requires the native AArch64 Mach-O configuration"
#endif

enum { MAX_OWNED = 128 };

typedef struct {
    void *owned[MAX_OWNED];
    size_t live;
    unsigned allocs;
    unsigned frees;
    unsigned fatals;
    unsigned logs;
    const char *fail_tag;
} TestHost;

static TestHost owner_host;
static TestHost default_host;
static ll_linker owner;
static bool reject_backend;
static unsigned failures;

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "macho_stub_errors:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void remember(TestHost *host, void *pointer)
{
    CHECK(pointer != NULL, "owner allocation returned NULL");
    CHECK(host->live < MAX_OWNED, "owner allocation ledger overflow");
    if (pointer != NULL && host->live < MAX_OWNED)
        host->owned[host->live++] = pointer;
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    TestHost *host = ctx;
    (void)what;
    void *pointer = malloc(size == 0 ? 1 : size);
    ++host->allocs;
    remember(host, pointer);
    return pointer;
}

static void *host_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    TestHost *host = ctx;
    if (host->fail_tag != NULL && strcmp(what, host->fail_tag) == 0)
        return NULL;
    void *pointer = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    ++host->allocs;
    remember(host, pointer);
    return pointer;
}

static void *host_realloc(void *ctx, void *pointer, size_t size, const char *what)
{
    TestHost *host = ctx;
    (void)what;
    if (pointer == NULL) {
        void *resized = realloc(NULL, size == 0 ? 1 : size);
        ++host->allocs;
        remember(host, resized);
        return resized;
    }
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == pointer) {
            void *resized = realloc(pointer, size == 0 ? 1 : size);
            if (resized != NULL) host->owned[i] = resized;
            return resized;
        }
    }
    CHECK(false, "realloc pointer was not owned by this linker");
    return NULL;
}

static void host_free(void *ctx, void *pointer)
{
    TestHost *host = ctx;
    if (pointer == NULL) return;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == pointer) {
            host->owned[i] = host->owned[--host->live];
            ++host->frees;
            free(pointer);
            return;
        }
    }
    CHECK(false, "freed pointer was not owned by this linker");
}

static void host_log(void *ctx, int kind, const char *format, va_list args)
{
    TestHost *host = ctx;
    char message[512];
    int written = vsnprintf(message, sizeof(message), format, args);
    (void)kind;
    CHECK(written >= 0 && (size_t)written < sizeof(message), "host log truncated");
    ++host->logs;
}

static _Noreturn void host_fatal(void *ctx, const char *format, va_list args)
{
    TestHost *host = ctx;
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

static ll_compat_host_ops host_ops(TestHost *host)
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

/* Keep the real architecture backend on successful calls and permit a
 * deterministic rejection to exercise makeStub's post-allocation cleanup. */
#define macho_makeStubAarch64 real_makeStubAarch64
#include "linker/macho/plt_aarch64.c"
#undef macho_makeStubAarch64

bool macho_makeStubAarch64(Stub *stub)
{
    return reject_backend ? EXIT_FAILURE : real_makeStubAarch64(stub);
}

#include "linker/MachO.c"
#include "linker/macho/plt.c"

static void clear_owner_error(void)
{
    owner.error_status = LL_OK;
    owner.error_message[0] = '\0';
}

static void test_stub_success_and_reuse(void)
{
    _Alignas(uint32_t) uint32_t stub_words[15];
    static const uint32_t expected_words[] = {
        UINT32_C(0xd2e22450), UINT32_C(0xf2c66890), UINT32_C(0xf2aaacd0),
        UINT32_C(0xf28ef110), UINT32_C(0xd61f0200),
    };
    struct SectionFormatInfo section_info = {0};
    Section section = {0};
    const uintptr_t first_target_bits = UINT64_C(0x1122334455667788);
    const uintptr_t second_target_bits = UINT64_C(0x8877665544332211);
    void *first_target = (void *)first_target_bits;
    void *second_target = (void *)second_target_bits;
    void *lookup;

    memset(stub_words, 0xA5, sizeof(stub_words));
    section_info.stub_offset = stub_words;
    section_info.stub_size = sizeof(stub_words);
    section.info = &section_info;

    clear_owner_error();
    CHECK(macho_findStub(&section, &first_target, 0),
          "findStub did not signal a missing target");
    CHECK(macho_makeStub(&owner, &section, &first_target, 0) == EXIT_SUCCESS,
          "valid makeStub call failed");
    CHECK(owner.error_status == LL_OK, "successful makeStub set an error");
    CHECK(section_info.nstubs == 1 && section_info.stubs != NULL,
          "successful makeStub did not add one list node");
    CHECK(section_info.stubs != NULL
              && section_info.stubs->target == (void *)first_target_bits,
          "stub target was not preserved");
    CHECK(section_info.stubs != NULL && section_info.stubs->addr == &stub_words[0],
          "stub was not placed in the real backing array");
    CHECK(memcmp(&stub_words[0], expected_words, sizeof(expected_words)) == 0,
          "real AArch64 backend emitted unexpected stub words");

    lookup = (void *)first_target_bits;
    if (macho_findStub(&section, &lookup, 0)) {
        CHECK(macho_makeStub(&owner, &section, &lookup, 0) == EXIT_SUCCESS,
              "missing-target path failed while checking reuse");
    }
    CHECK(section_info.nstubs == 1,
          "repeated target was not reused through findStub");
    CHECK(lookup == &stub_words[0], "findStub did not return the first stub address");

    CHECK(macho_makeStub(&owner, &section, &second_target, 0) == EXIT_SUCCESS,
          "second valid makeStub call failed");
    CHECK(section_info.nstubs == 2 && section_info.stubs != NULL
              && section_info.stubs->next != NULL,
          "second successful makeStub did not append a node");
    CHECK(section_info.stubs != NULL && section_info.stubs->next != NULL
              && section_info.stubs->next->addr == &stub_words[5],
          "second stub was not placed after the first slot");

    size_t live_before_free = owner_host.live;
    unsigned frees_before = owner_host.frees;
    macho_freeStubs(&owner, &section);
    CHECK(section_info.stubs == NULL && section_info.nstubs == 0,
          "freeStubs did not clear the list and count");
    CHECK(owner_host.live + 2 == live_before_free,
          "freeStubs did not release both metadata nodes");
    CHECK(owner_host.frees == frees_before + 2,
          "freeStubs did not free both metadata nodes");
}

static void test_failure_cleanup(void)
{
    _Alignas(uint32_t) unsigned char stub_bytes[64];
    struct SectionFormatInfo info = {0};
    Section section = {0};
    uintptr_t target_bits = UINT64_C(0x123456789abcdef0);
    void *target = (void *)target_bits;
    memset(stub_bytes, 0xA5, sizeof(stub_bytes));
    info.stub_offset = stub_bytes;
    info.stub_size = sizeof(stub_bytes);
    section.info = &info;

    clear_owner_error();
    size_t live_before_guard = owner_host.live;
    section.info = NULL;
    CHECK(macho_makeStub(&owner, &section, &target, 0) == EXIT_FAILURE &&
              owner.error_status == LL_E_RELOC_RANGE,
          "missing stub metadata was not rejected");
    section.info = &info;
    info.stub_offset = NULL;
    clear_owner_error();
    CHECK(macho_makeStub(&owner, &section, &target, 0) == EXIT_FAILURE &&
              owner.error_status == LL_E_RELOC_RANGE,
          "missing stub storage was not rejected");
    info.stub_offset = stub_bytes;
    info.stub_size = STUB_SIZE - 1;
    clear_owner_error();
    CHECK(macho_makeStub(&owner, &section, &target, 0) == EXIT_FAILURE &&
              owner.error_status == LL_E_RELOC_RANGE,
          "short stub storage was not rejected");
    info.stub_size = STUB_SIZE;
    info.nstubs = 1;
    clear_owner_error();
    CHECK(macho_makeStub(&owner, &section, &target, 0) == EXIT_FAILURE &&
              owner.error_status == LL_E_RELOC_RANGE,
          "exhausted stub storage was not rejected");
    CHECK(target == (void *)target_bits && owner_host.live == live_before_guard &&
              info.stubs == NULL, "stub guard changed caller state or allocated");
    info.nstubs = 0;
    info.stub_size = sizeof(stub_bytes);

    reject_backend = true;
    clear_owner_error();
    size_t live_before = owner_host.live;
    unsigned frees_before = owner_host.frees;
    CHECK(macho_makeStub(&owner, &section, &target, 0) == EXIT_FAILURE,
          "backend rejection did not fail makeStub");
    CHECK(owner.error_status == LL_E_RELOC_RANGE,
          "backend rejection did not report LL_E_RELOC_RANGE");
    CHECK(target == (void *)target_bits, "failed makeStub changed caller target");
    CHECK(info.stubs == NULL && info.nstubs == 0,
          "backend rejection changed the linked stub list");
    CHECK(owner_host.live == live_before && owner_host.frees == frees_before + 1,
          "backend rejection did not free its unlinked metadata node");
    for (size_t i = 0; i < sizeof(stub_bytes); ++i)
        CHECK(stub_bytes[i] == 0xA5, "rejected backend changed stub output bytes");

    reject_backend = false;
    owner_host.fail_tag = "makeStub";
    clear_owner_error();
    live_before = owner_host.live;
    frees_before = owner_host.frees;
    CHECK(macho_makeStub(&owner, &section, &target, 0) == EXIT_FAILURE,
          "allocation failure did not fail makeStub");
    CHECK(owner.error_status == LL_E_NOMEM,
          "allocation failure did not report LL_E_NOMEM");
    CHECK(target == (void *)target_bits, "allocation failure changed caller target");
    CHECK(info.stubs == NULL && info.nstubs == 0,
          "allocation failure changed the linked stub list");
    CHECK(owner_host.live == live_before && owner_host.frees == frees_before,
          "allocation failure changed allocator ownership");
    for (size_t i = 0; i < sizeof(stub_bytes); ++i)
        CHECK(stub_bytes[i] == 0xA5, "allocation failure changed stub output bytes");
    owner_host.fail_tag = NULL;
}

static void test_relocation_failure_propagation(void)
{
    _Alignas(uint32_t) unsigned char code[4] = {0};
    _Alignas(uint32_t) unsigned char stub_bytes[64];
    uint32_t instruction = UINT32_C(0x94000000);
    MachORelocationInfo relocation = {0};
    MachOSection macho_section = {0};
    MachONList nlist = {0};
    struct SectionFormatInfo section_info = {0};
    Section section = {0};
    MachOSymbol symbol = {0};
    struct ObjectCodeFormatInfo object_info = {0};
    ObjectCode oc = {0};
    uintptr_t target_bits = (uintptr_t)code + UINT64_C(0x20000000);

    memcpy(code, &instruction, sizeof(instruction));
    memset(stub_bytes, 0xA5, sizeof(stub_bytes));
    macho_section.nreloc = 1;
    section_info.stub_offset = stub_bytes;
    section_info.stub_size = sizeof(stub_bytes);
    section_info.macho_section = &macho_section;
    section_info.relocation_info = &relocation;
    section.start = code;
    section.size = sizeof(code);
    section.info = &section_info;
    relocation.r_symbolnum = 0;
    relocation.r_length = 2;
    relocation.r_type = ARM64_RELOC_BRANCH26;
    symbol.addr = (void *)target_bits;
    symbol.nlist = &nlist;
    object_info.macho_symbols = &symbol;
    oc.L = &owner;
    oc.info = &object_info;
    oc.sections = &section;
    oc.n_sections = 1;
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(&owner, &oc.proddables, code, sizeof(code));
    CHECK(containsSpan(&oc.proddables, (uintptr_t)code,
                       (uintptr_t)code + sizeof(code)),
          "real proddable-block setup did not contain the instruction span");

    size_t live_before = owner_host.live;
    unsigned logs_before = owner_host.logs;
    owner_host.fail_tag = "makeStub";
    clear_owner_error();
    CHECK(relocateSectionAarch64(&oc, &section) == 0,
          "relocation did not propagate stub allocation failure");
    CHECK(owner.error_status == LL_E_NOMEM,
          "relocation overwrote the more specific LL_E_NOMEM status");
    CHECK(owner_host.logs == logs_before + 1,
          "relocation failure did not use owner logging");
    CHECK(owner_host.fatals == 0, "recoverable relocation failure called fatal");
    CHECK(section_info.stubs == NULL && section_info.nstubs == 0,
          "failed relocation changed stub list ownership");
    CHECK(memcmp(code, &instruction, sizeof(instruction)) == 0,
          "failed relocation changed instruction bytes");
    for (size_t i = 0; i < sizeof(stub_bytes); ++i)
        CHECK(stub_bytes[i] == 0xA5,
              "failed relocation changed stub output bytes");
    CHECK(owner_host.live == live_before,
          "failed relocation changed allocator ownership");
    owner_host.fail_tag = NULL;
    size_t live_before_proddable_free = owner_host.live;
    freeProddableBlocks(&owner, &oc.proddables);
    CHECK(oc.proddables.data == NULL && oc.proddables.size == 0
              && oc.proddables.capacity == 0,
          "proddable-block teardown did not clear the set");
    CHECK(owner_host.live + 1 == live_before_proddable_free,
          "proddable-block teardown did not release its owned storage");
}

static void test_free_stubs_null_inputs(void)
{
    Section section = {0};
    macho_freeStubs(&owner, NULL);
    macho_freeStubs(&owner, &section);
    CHECK(section.info == NULL, "freeStubs changed a NULL section info pointer");
}

int main(void)
{
    static const char default_message[] = "default owner sentinel";
    ll_compat_host_ops default_ops = host_ops(&default_host);
    ll_compat_host_ops owner_ops = host_ops(&owner_host);
    ll_install_host(&default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_message, sizeof(default_message));
    ll_init_linker(&owner, 0);

#if defined(STUB_FREE_ONLY)
    test_stub_success_and_reuse();
#else
    test_stub_success_and_reuse();
    test_failure_cleanup();
    test_relocation_failure_propagation();
    test_free_stubs_null_inputs();
#endif
    ll_exit_linker(&owner);

    CHECK(owner_host.live == 0, "owner teardown left allocations live");
    CHECK(owner_host.fatals == 0, "test invoked an unexpected fatal callback");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && memcmp(ll_default_linker.error_message, default_message,
                        sizeof(default_message)) == 0,
          "owner operation changed default linker error state");
    CHECK(default_host.logs == 0 && default_host.fatals == 0,
          "owner error was routed through default host services");
    if (failures != 0) return 1;
    (void)puts("macho_stub_errors: PASS");
    return 0;
}
