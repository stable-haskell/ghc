#define _DARWIN_C_SOURCE 1
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"

#if !defined(darwin_HOST_OS) && !defined(ios_HOST_OS)
#include <stdio.h>
int main(void)
{
    (void)puts("SKIP: Mach-O indirect-import fixture is Darwin-only");
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
        (void)fprintf(stderr, "macho_import_status:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

enum { OWNER_CAPACITY = 256 };
typedef struct {
    void *owned[OWNER_CAPACITY];
    size_t live;
    unsigned log_calls;
    unsigned fatal_calls;
} Host;

static Host owner_host;
static Host default_host;
static unsigned failures;

static void remember(Host *host, void *ptr)
{
    CHECK(ptr != NULL, "host allocation failed");
    CHECK(host->live < OWNER_CAPACITY, "host allocation ledger full");
    if (ptr != NULL && host->live < OWNER_CAPACITY)
        host->owned[host->live++] = ptr;
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    void *ptr = malloc(size == 0 ? 1 : size);
    remember(host, ptr);
    return ptr;
}

static void *host_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    void *ptr = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    remember(host, ptr);
    return ptr;
}

static void *host_realloc(void *ctx, void *ptr, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    if (ptr == NULL) return host_malloc(ctx, size, what);
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == ptr) {
            void *next = realloc(ptr, size == 0 ? 1 : size);
            if (next != NULL) host->owned[i] = next;
            return next;
        }
    }
    CHECK(false, "realloc pointer was not owned by linker host");
    return NULL;
}

static void host_free(void *ctx, void *ptr)
{
    Host *host = ctx;
    if (ptr == NULL) return;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == ptr) {
            host->owned[i] = host->owned[--host->live];
            free(ptr);
            return;
        }
    }
    CHECK(false, "free pointer was not owned by linker host");
}

static void host_log(void *ctx, int kind, const char *fmt, va_list ap)
{
    Host *host = ctx;
    char message[512];
    int n = vsnprintf(message, sizeof(message), fmt, ap);
    CHECK(n >= 0 && (size_t)n < sizeof(message), "log formatting failed/truncated");
    (void)kind;
    ++host->log_calls;
}

static _Noreturn void host_fatal(void *ctx, const char *fmt, va_list ap)
{
    Host *host = ctx;
    char message[512];
    (void)vsnprintf(message, sizeof(message), fmt, ap);
    ++host->fatal_calls;
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

static void reset_target(unsigned char *storage, size_t size)
{
    memset(storage, 0xA5, size);
}

static void check_outside_target(const unsigned char *storage, size_t storage_size,
                                 size_t target_offset, size_t target_size,
                                 const char *message)
{
    for (size_t i = 0; i < storage_size; ++i) {
        if (i < target_offset || i >= target_offset + target_size)
            CHECK(storage[i] == 0xA5, message);
    }
}

static void test_two_import_slots(ll_linker *owner)
{
    unsigned char storage[2 * sizeof(void *) + 3];
    _Alignas(uint32_t) unsigned char image[1024];
    unsigned char image_before[sizeof(image)];
    uint32_t index_words[3] = {UINT32_C(0xfeedface), 0, 1};
    MachONList nlists[2] = {0};
    MachOSymbol symbols[2] = {0};
    struct ObjectCodeFormatInfo info = {0};
    MachOSection import_section = {0};
    Section target = {0};
    ObjectCode oc = {0};

    reset_target(storage, sizeof(storage));
    memset(image, 0xC3, sizeof(image));
    memcpy(image + 2 * sizeof(uint32_t), index_words, sizeof(index_words));
    memcpy(image_before, image, sizeof(image));
    symbols[0].name = (ll_symbol_name *)"first_import";
    symbols[0].nlist = &nlists[0];
    nlists[0].n_type = N_UNDF | N_EXT;
    nlists[0].n_value = UINT64_C(0x1111222233334444);
    symbols[1].name = (ll_symbol_name *)"second_import";
    symbols[1].nlist = &nlists[1];
    nlists[1].n_type = N_UNDF | N_EXT;
    nlists[1].n_value = UINT64_C(0x5555666677778888);
    info.macho_symbols = symbols;
    info.n_macho_symbols = 2;
    import_section.size = 2 * sizeof(void *);
    import_section.reserved1 = 1;
    import_section.offset = 512;
    target.start = storage + 1; /* Intentionally not pointer-aligned. */
    target.size = import_section.size;
    oc.L = owner;
    oc.fileName = "synthetic-import.o";
    oc.image = (char *)image;
    oc.info = &info;
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(owner, &oc.proddables, target.start, target.size);

    CHECK(resolveImports(&oc, &import_section, &target,
                         (const uint32_t *)(const void *)(image + 2 * sizeof(uint32_t)),
                         3) == 1,
          "two-entry indirect import resolution failed");
    ll_symbol_addr *actual[2] = {NULL, NULL};
    memcpy(&actual[0], (unsigned char *)target.start, sizeof(actual[0]));
    memcpy(&actual[1], (unsigned char *)target.start + sizeof(void *), sizeof(actual[1]));
    CHECK(actual[0] == (ll_symbol_addr *)(uintptr_t)nlists[0].n_value,
          "first loaded pointer slot has wrong resolved address");
    CHECK(actual[1] == (ll_symbol_addr *)(uintptr_t)nlists[1].n_value,
          "second loaded pointer slot has wrong resolved address");
    CHECK(memcmp(image, image_before, sizeof(image)) == 0,
          "resolution modified the source image or indirect table");
    check_outside_target(storage, sizeof(storage), 1, target.size,
                         "unaligned target write crossed a canary");
    freeProddableBlocks(owner, &oc.proddables);
}

static void test_reject_without_writes(ll_linker *owner, uint32_t second_index,
                                       ll_status expected, const char *message)
{
    unsigned char storage[2 * sizeof(void *) + 3];
    uint32_t indexes[2] = {0, second_index};
    MachONList nlists[1] = {0};
    MachOSymbol symbols[1] = {0};
    struct ObjectCodeFormatInfo info = {0};
    MachOSection import_section = {0};
    Section target = {0};
    ObjectCode oc = {0};

    reset_target(storage, sizeof(storage));
    symbols[0].name = (ll_symbol_name *)"first_import";
    symbols[0].nlist = &nlists[0];
    nlists[0].n_type = N_UNDF | N_EXT;
    nlists[0].n_value = UINT64_C(0x1111222233334444);
    info.macho_symbols = symbols;
    info.n_macho_symbols = 1;
    import_section.size = 2 * sizeof(void *);
    target.start = storage + 1;
    target.size = import_section.size;
    oc.L = owner;
    oc.fileName = "synthetic-import.o";
    oc.info = &info;
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(owner, &oc.proddables, target.start, target.size);
    ll_error_clear(owner);

    CHECK(resolveImports(&oc, &import_section, &target, indexes, 2) == 0,
          message);
    CHECK(owner->error_status == expected, "negative status mismatch");
    for (size_t i = 0; i < sizeof(storage); ++i)
        CHECK(storage[i] == 0xA5, "rejected table partially wrote target slots");
    freeProddableBlocks(owner, &oc.proddables);
}

static void test_shape_and_lookup_failures(ll_linker *owner)
{
    unsigned char storage[2 * sizeof(void *) + 3];
    uint32_t indexes[2] = {0, 0};
    MachONList nlist = {0};
    MachOSymbol symbol = {0};
    struct ObjectCodeFormatInfo info = {0};
    MachOSection import_section = {0};
    Section target = {0};
    ObjectCode oc = {0};

    reset_target(storage, sizeof(storage));
    symbol.name = (ll_symbol_name *)"_missing_import_for_fixture";
    symbol.nlist = &nlist;
    nlist.n_type = N_UNDF | N_EXT;
    info.macho_symbols = &symbol;
    info.n_macho_symbols = 1;
    import_section.size = 2 * sizeof(void *);
    target.start = storage + 1;
    target.size = import_section.size - 1;
    oc.L = owner;
    oc.fileName = "synthetic-import.o";
    oc.info = &info;
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(owner, &oc.proddables, target.start, import_section.size);
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, &target, indexes, 2) == 0
              && owner->error_status == LL_E_FORMAT,
          "target and pointer-slot size mismatch was accepted");
    for (size_t i = 0; i < sizeof(storage); ++i)
        CHECK(storage[i] == 0xA5, "shape rejection wrote to target storage");

    target.size = sizeof(void *) - 1;
    import_section.size = target.size;
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, &target, indexes, 2) == 0
              && owner->error_status == LL_E_FORMAT,
          "non-pointer-multiple slot size was accepted");
    for (size_t i = 0; i < sizeof(storage); ++i)
        CHECK(storage[i] == 0xA5, "slot-shape rejection wrote to target storage");

    import_section.size = sizeof(void *);
    target.size = import_section.size;
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, &target, indexes, 1) == 0
              && owner->error_status == LL_E_UNRESOLVED,
          "missing imported symbol did not return UNRESOLVED");
    for (size_t i = 0; i < sizeof(storage); ++i)
        CHECK(storage[i] == 0xA5, "unresolved symbol wrote target storage");
    freeProddableBlocks(owner, &oc.proddables);
}

static int run_table_boundary_case(ll_linker *owner, uint32_t file_size,
                                   uint32_t offset, uint32_t count)
{
    _Alignas(uint32_t) unsigned char image[16] = {0};
    struct ObjectCodeFormatInfo info = {0};
    MachODsymtabCommand dsym = {0};
    ObjectCode oc = {0};
    info.dsymCmd = &dsym;
    dsym.indirectsymoff = offset;
    dsym.nindirectsyms = count;
    oc.L = owner;
    oc.image = (char *)image;
    oc.fileSize = file_size;
    oc.info = &info;
    oc.n_sections = 0;
    ll_error_clear(owner);
    return ocResolve_MachO(&oc);
}

static void test_caller_table_bounds(ll_linker *owner)
{
    CHECK(run_table_boundary_case(owner, 4, 4, 1) == 0
              && owner->error_status == LL_E_FORMAT,
          "short/out-of-file indirect symbol table was accepted");
    CHECK(run_table_boundary_case(owner, 8, 1, 1) == 0
              && owner->error_status == LL_E_FORMAT,
          "misaligned indirect symbol table was accepted");
    CHECK(run_table_boundary_case(owner, 8, 9, 0) == 0
              && owner->error_status == LL_E_FORMAT,
          "offset past image end was accepted");
}

static void test_helper_range_boundaries(ll_linker *owner)
{
    unsigned char storage[sizeof(void *) + 2];
    uint32_t indexes[1] = {0};
    MachOSection import_section = {0};
    Section target = {0};
    ObjectCode oc = {0};
    oc.L = owner;
    oc.fileName = "synthetic-import.o";

    import_section.size = sizeof(void *);
    import_section.reserved1 = 0;
    target.start = storage + 1;
    target.size = import_section.size;
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, &target, indexes, 0) == 0
              && owner->error_status == LL_E_FORMAT,
          "short indirect-symbol count was accepted");

    import_section.reserved1 = 1;
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, &target, indexes, 1) == 0
              && owner->error_status == LL_E_FORMAT,
          "reserved1 equal to count with nonzero slots was accepted");

    import_section.reserved1 = UINT32_MAX;
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, &target, indexes, 1) == 0
              && owner->error_status == LL_E_FORMAT,
          "UINT32_MAX reserved1 was accepted");

    import_section.reserved1 = 0;
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, NULL, indexes, 1) == 0
              && owner->error_status == LL_E_FORMAT,
          "NULL target Section was accepted");

    target.start = NULL;
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, &target, indexes, 1) == 0
              && owner->error_status == LL_E_FORMAT,
          "NULL target storage for nonzero slots was accepted");

    import_section.size = 0;
    import_section.reserved1 = 0;
    target.size = 0;
    target.start = NULL;
    ll_error_clear(owner);
    CHECK(resolveImports(&oc, &import_section, &target, NULL, 0) == 1
              && owner->error_status == LL_OK,
          "zero-slot import section was not accepted");
}

static void test_jump_table_policy(ll_linker *owner)
{
    _Alignas(uint32_t) unsigned char image[sizeof(uint32_t)] = {0};
    MachODsymtabCommand dsym = {0};
    struct ObjectCodeFormatInfo info = {0};
    MachOSection macho_section = {0};
    Section target = {0};
    ObjectCode oc = {0};
    dsym.indirectsymoff = 0;
    dsym.nindirectsyms = 1;
    info.dsymCmd = &dsym;
    info.macho_sections = &macho_section;
    (void)snprintf(macho_section.sectname, sizeof(macho_section.sectname),
                   "__jump_table");
    oc.L = owner;
    oc.image = (char *)image;
    oc.fileSize = sizeof(image);
    oc.info = &info;
    oc.sections = &target;
    oc.n_sections = 1;
    ll_error_clear(owner);
    CHECK(ocResolve_MachO(&oc) == 0 && owner->error_status == LL_E_UNSUPPORTED,
          "__jump_table did not return UNSUPPORTED");
}

int main(int argc, char **argv)
{
    ll_linker owner = {0};
    ll_compat_host_ops owner_ops = make_ops(&owner_host);
    ll_compat_host_ops default_ops = make_ops(&default_host);
    char default_message[] = "default-owner-sentinel";

    ll_install_host(&default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_message, sizeof(default_message));
    ll_init_linker(&owner, 0);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);

    bool positive_only = argc == 2 && strcmp(argv[1], "--positive-only") == 0;
    CHECK(argc == 1 || positive_only, "unknown test mode");
    test_two_import_slots(&owner);
    if (!positive_only) {
        test_reject_without_writes(&owner, 1, LL_E_FORMAT,
                                   "invalid second symbol index was accepted");
        test_reject_without_writes(&owner, INDIRECT_SYMBOL_LOCAL, LL_E_UNSUPPORTED,
                                   "LOCAL entry was not rejected as UNSUPPORTED");
        test_reject_without_writes(&owner, INDIRECT_SYMBOL_ABS, LL_E_UNSUPPORTED,
                                   "ABS entry was not rejected as UNSUPPORTED");
        test_reject_without_writes(
            &owner, INDIRECT_SYMBOL_LOCAL | INDIRECT_SYMBOL_ABS,
            LL_E_UNSUPPORTED, "combined LOCAL|ABS entry was not unsupported");
        test_shape_and_lookup_failures(&owner);
        test_helper_range_boundaries(&owner);
        test_caller_table_bounds(&owner);
        test_jump_table_policy(&owner);
    }

    CHECK(owner_host.fatal_calls == 0, "unexpected linker fatal callback");
    if (!positive_only) {
        CHECK(owner_host.log_calls > 0,
              "negative paths did not use the selected owner's log callback");
    }
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && memcmp(ll_default_linker.error_message, default_message,
                        sizeof(default_message)) == 0,
          "owner operation modified default linker error state");
    CHECK(default_host.log_calls == 0,
          "owner error was logged through default host services");
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    CHECK(owner_host.live == 0, "linker exit left host allocations live");
    return failures == 0 ? 0 : 1;
}
#endif
