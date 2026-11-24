/* Drive actual core/ELF code with native services intercepted, never objects. */
#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "LinkerInternals.h"
#include "ll_format.h"
#include "ll_noexec.h"
#include "ll_path.h"
#include "linker/BackendMemory.h"
#include "linker/M32Alloc.h"
#include "linker/MMap.h"
#include "linker/SymbolExtras.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !LL_BUILD_ELF || !LL_TARGET_X86_64 || SIZEOF_VOID_P != 8
#error "noexec_core requires the x64 ELF backend on a 64-bit host"
#endif

enum { MAX_ALLOCATIONS = 4096 };
typedef struct {
    ll_linker *owner;
    void *bookkeeping[MAX_ALLOCATIONS];
    size_t live;
    void *heap[MAX_ALLOCATIONS];
    size_t heap_live, heap_calls, fail_heap_at;
    size_t native_calls, builtin_calls, init_calls, fini_calls, cxa_calls;
    size_t page_size;
} TestHost;

/* The readonly host adapter has no owner argument; the tested operation is
 * synchronous. This binding is only for that test interception. */
static TestHost *active_host;
static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "noexec_core:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

static void *book_malloc(void *ctx, size_t size, const char *what)
{
    TestHost *host = ctx;
    (void)what;
    void *p = malloc(size != 0 ? size : 1);
    if (p == NULL || host->live == MAX_ALLOCATIONS) abort();
    host->bookkeeping[host->live++] = p;
    return p;
}
static void *book_calloc(void *ctx, size_t n, size_t size, const char *what)
{
    if (n != 0 && size > SIZE_MAX / n) abort();
    void *p = book_malloc(ctx, n * size, what);
    memset(p, 0, n * size);
    return p;
}
static void book_free(void *ctx, void *p)
{
    if (p == NULL) return;
    TestHost *host = ctx;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->bookkeeping[i] == p) {
            host->bookkeeping[i] = host->bookkeeping[--host->live];
            free(p);
            return;
        }
    }
    CHECK(false, "bookkeeping free received section memory or an unknown owner");
}
static void *book_realloc(void *ctx, void *p, size_t size, const char *what)
{
    if (p == NULL) return book_malloc(ctx, size, what);
    TestHost *host = ctx;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->bookkeeping[i] == p) {
            void *q = realloc(p, size != 0 ? size : 1);
            if (q == NULL) abort();
            host->bookkeeping[i] = q;
            return q;
        }
    }
    abort();
}
static void *heap_calloc(size_t n, size_t size)
{
    TestHost *host = active_host;
    ++host->heap_calls;
    if (host->fail_heap_at == host->heap_calls) return NULL;
    void *p = calloc(n, size);
    if (p == NULL || host->heap_live == MAX_ALLOCATIONS) abort();
    host->heap[host->heap_live++] = p;
    return p;
}
static void heap_free(void *p)
{
    if (p == NULL) return;
    TestHost *host = active_host;
    for (size_t i = 0; i < host->heap_live; ++i) {
        if (host->heap[i] == p) {
            host->heap[i] = host->heap[--host->heap_live];
            free(p);
            return;
        }
    }
    CHECK(false, "section free received bookkeeping memory or an unknown base");
}
static void builtin_spy(void *ctx, int retain, ll_builtin_fn insert, void *arg)
{
    (void)retain; (void)insert; (void)arg;
    ++((TestHost *)ctx)->builtin_calls;
}
static void init_hook(void *ctx, ObjectCode *oc)
{
    (void)oc;
    ++((TestHost *)ctx)->init_calls;
}
static void after_hook(void *ctx, ObjectCode *oc, bool ok)
{
    (void)oc; (void)ok;
    ++((TestHost *)ctx)->init_calls;
}
static void batch_hook(void *ctx) { ++((TestHost *)ctx)->init_calls; }
static void native_hook(void *ctx, ObjectCode *oc)
{
    (void)oc;
    ++((TestHost *)ctx)->native_calls;
}
static void native_after(void *ctx, ObjectCode *oc, bool ok)
{
    (void)oc; (void)ok;
    ++((TestHost *)ctx)->native_calls;
}
static size_t page_size(void *ctx) { return ((TestHost *)ctx)->page_size; }
static void flush_spy(void *ctx, void *p, size_t n)
{
    (void)p; (void)n;
    ++((TestHost *)ctx)->native_calls;
}
static bool protect_spy(ll_linker *L, void *p, size_t n, MemoryAccess access)
{
    (void)p; (void)n; (void)access;
    ++((TestHost *)L->services.host.ctx)->native_calls;
    return false;
}
static bool readonly_spy(void *p, size_t n)
{
    (void)p; (void)n;
    ++active_host->native_calls;
    return false;
}
static void *map_spy(ll_linker *L, size_t n)
{
    (void)n;
    ++((TestHost *)L->services.host.ctx)->native_calls;
    return NULL;
}
static bool extras_spy(ObjectCode *oc)
{
    ++((TestHost *)oc->L->services.host.ctx)->native_calls;
    return false;
}
static m32_allocator *new_m32_spy(ll_linker *L, bool executable)
{
    (void)executable;
    ++((TestHost *)L->services.host.ctx)->native_calls;
    return NULL;
}
static void free_m32_spy(ll_linker *L, m32_allocator *p)
{
    (void)p;
    ++((TestHost *)L->services.host.ctx)->native_calls;
}
static bool flush_m32_spy(ll_linker *L, m32_allocator *p)
{
    (void)p;
    ++((TestHost *)L->services.host.ctx)->native_calls;
    return false;
}
static int init_spy(ObjectCode *oc)
{
    ++((TestHost *)oc->L->services.host.ctx)->init_calls;
    return 1;
}
static int fini_spy(ObjectCode *oc)
{
    ++((TestHost *)oc->L->services.host.ctx)->fini_calls;
    return 1;
}
static void cxa_spy(void *image) { (void)image; ++active_host->cxa_calls; }

/* Test the actual product bodies. A missed guard is counted and fails safely,
 * instead of permitting an executable mapping or foreign constructor call. */
#define calloc heap_calloc
#define free heap_free
#include "ll_noexec.c"
#undef calloc
#define ll_mmap_anon_for_linker map_spy
#define ll_mprotect_for_linker protect_spy
#define ll_backend_protect_readonly readonly_spy
#define ocProtectExtras extras_spy
#define m32_allocator_new_for new_m32_spy
#define m32_allocator_free_for free_m32_spy
#define m32_allocator_flush_for flush_m32_spy
#include "Linker.c"
#include "linker/Elf.c"
#include "linker/elf_got.c"
#include "linker/LoadArchive.c"
#include "linker/LoadNativeObjPosix.c"
#undef m32_allocator_flush_for
#undef m32_allocator_free_for
#undef m32_allocator_new_for
#undef ocProtectExtras
#undef ll_backend_protect_readonly
#undef ll_mprotect_for_linker
#undef ll_mmap_anon_for_linker
#undef free

static void registered(void *ctx, ObjectCode *oc)
{
    ll_linker *L = ((TestHost *)ctx)->owner;
    oc->next = L->gc.objects;
    if (oc->next != NULL) oc->next->prev = oc;
    L->gc.objects = oc;
}
static void start(ll_linker *L, TestHost *host, uintptr_t base, size_t page)
{
    memset(L, 0, sizeof(*L));
    memset(host, 0, sizeof(*host));
    host->owner = L;
    host->page_size = page;
    active_host = host;
    ll_compat_host_ops ops = {.ctx = host, .malloc_ = book_malloc,
        .calloc_ = book_calloc, .realloc_ = book_realloc, .free_ = book_free,
        .populate_builtins = builtin_spy, .page_size = page_size,
        .flush_icache = flush_spy, .before_init = init_hook,
        .after_init = after_hook, .after_init_batch = batch_hook,
        .before_native_load = native_hook, .after_native_load = native_after,
        .object_registered = registered};
    ll_compat_config cfg = {.flags = LL_CFG_NO_EXEC | LL_CFG_ALL_FORMATS,
                            .noexec_base = base};
    ll_install_host_for(L, &ops, &cfg);
    ll_init_linker(L, 0);
    ll_mutex_lock_for(L, &L->linker_mutex);
}
static void stop(ll_linker *L, TestHost *host)
{
    while (L->gc.objects != NULL) {
        ObjectCode *oc = L->gc.objects;
        L->gc.objects = oc->next;
        removeOcSymbols(oc);
        oc->status = OBJECT_UNLOADED;
        freeObjectCode(oc);
    }
    L->gc.loaded_objects = NULL;
    ll_mutex_unlock_for(L, &L->linker_mutex);
    ll_exit_linker(L);
    CHECK(host->live == 0 && host->heap_live == 0, "owner or heap allocations leaked");
    CHECK(host->native_calls == 0 && host->builtin_calls == 0 &&
          host->init_calls == 0 && host->fini_calls == 0 && host->cxa_calls == 0,
          "NO_EXEC reached native memory/loader or lifecycle services");
}
static bool define(ll_linker *L, const char *name, uintptr_t value)
{
    return ghciInsertSymbolTable(L, WSTR("noexec_core"), name, (void *)value,
                                LL_STRENGTH_STRONG, LL_SYM_CODE, 0, NULL) != 0;
}
static void check_object(ll_linker *L, ObjectCode *oc)
{
    CHECK(!oc->imageMapped && oc->rx_m32 == NULL && oc->rw_m32 == NULL,
          "NO_EXEC acquired native storage");
    for (int i = 1; i < oc->n_sections; ++i) {
        const Section *s = &oc->sections[i];
        CHECK(s->alloc == SECTION_NOEXEC && s->mapped_start != NULL &&
              s->exec_start != (uintptr_t)s->start && s->exec_start % 4096 == 0,
              "section storage/exec metadata is not NO_EXEC");
    }
    CHECK(oc->symbol_extras_origin == LL_EXTRAS_NOEXEC &&
          oc->elf_info->got_allocation != NULL, "extras or global GOT lacks heap ownership");
    struct ll_format_ops ops = *oc->format_ops;
    ops.run_init = init_spy;
    ops.run_fini = fini_spy;
    oc->format_ops = &ops;
    const uintptr_t next_address = L->noexec_next;
    CHECK(resolveObjs_(L), "NO_EXEC core resolution failed");
    CHECK(resolveObjs_(L) && L->noexec_next == next_address,
          "repeat resolution advanced layout or failed");
    CHECK(oc->status == OBJECT_READY && !oc->initializersRan && oc->cxa_finalize == NULL,
          "NO_EXEC status/lifecycle state differs");
    for (ElfSymbolTable *table = oc->elf_info->symbolTables; table; table = table->next) {
        for (size_t i = 0; i < table->n_symbols; ++i) {
            const ElfSymbol *symbol = &table->symbols[i];
            const uint16_t index = symbol->elf_sym->st_shndx;
            if (index > 0 && index < oc->n_sections && symbol->addr != NULL)
                CHECK((uintptr_t)symbol->addr == oc->sections[index].exec_start +
                       symbol->elf_sym->st_value, "defined symbol retained a work address");
            if (symbol->got_addr != NULL) {
                void *payload;
                memcpy(&payload, symbol->got_addr, sizeof(payload));
                CHECK(payload == symbol->addr, "global GOT payload differs from symbol value");
            }
        }
    }
    /* Prove the disposal guard even with stale lifecycle metadata. */
    oc->initializersRan = true;
    oc->cxa_finalize = cxa_spy;
    removeOcSymbols(oc);
    L->gc.objects = oc->next;
    L->gc.loaded_objects = oc->next_loaded_object;
    freeObjectCode(oc);
}

int main(int argc, char **argv)
{
    if (argc != 8) {
        (void)fputs("usage: noexec_core SYMBOLS.o INIT_FINI.o ARCHIVE.a GNU_LONG.a TLSGD.o ABS64.o GOTTPOFF.o\n", stderr);
        return 2;
    }
    ll_linker owner;
    TestHost host;
    size_t fixture_allocations = 0;
    for (size_t page = 4096; page <= 16384; page *= 4) {
        start(&owner, &host, UINT32_C(0x10000000), page);
        CHECK(define(&owner, "__dso_handle", 0x12340000) &&
              define(&owner, "__cxa_atexit", 0x10010000) &&
              define(&owner, "__fini_array_start", 0x12340010) &&
              define(&owner, "fixture_record_event", 0x10010020), "explicit definitions failed");
        CHECK(lookupDependentSymbol(&owner, "__dso_handle", NULL, NULL) ==
              (void *)(uintptr_t)0x12340000, "native special symbol replaced explicit definition");
        CHECK(lookupDependentSymbol(&owner, "__fini_array_start", NULL, NULL) ==
              (void *)(uintptr_t)0x12340010, "native fini symbol replaced explicit definition");
        CHECK(lookupDependentSymbol(&owner, "_malloc", NULL, NULL) == NULL,
              "missing symbol fell through to native process lookup");
        CHECK(loadObj_(&owner, argv[1]), "could not load symbols fixture");
        if (fixture_allocations == 0) fixture_allocations = host.heap_calls;
        ObjectCode *oc = owner.gc.objects;
        if (oc != NULL) check_object(&owner, oc);
        CHECK(loadObj_(&owner, argv[2]), "could not load initializer fixture");
        oc = owner.gc.objects;
        if (oc != NULL) {
            CHECK(oc->elf_info->init != NULL && oc->elf_info->fini != NULL,
                  "initializer control fixture lost its lists");
            check_object(&owner, oc);
        }
        char *error = NULL;
        CHECK(loadNativeObj_POSIX(&owner, WSTR("unused.so"), &error) == NULL &&
              owner.error_status == LL_E_UNSUPPORTED && error != NULL,
              "native load was not rejected before services");
        book_free(&host, error);
        stop(&owner, &host);
    }
    /* Exercise full-width symbol construction across the signed boundary. */
    start(&owner, &host, UINT64_C(0x7fffffffffffd000), 16384);
    CHECK(loadObj_(&owner, argv[1]), "high-base fixture did not load");
    ObjectCode *oc = owner.gc.objects;
    if (oc != NULL) {
        uintptr_t expected = 0;
        for (ElfSymbolTable *table = oc->elf_info->symbolTables; table; table = table->next)
            for (size_t i = 0; i < table->n_symbols; ++i) {
                const ElfSymbol *s = &table->symbols[i];
                if (s->name != NULL && strcmp(s->name, "fixture_high_symbol") == 0)
                    expected = oc->sections[s->elf_sym->st_shndx].exec_start + s->elf_sym->st_value;
            }
        CHECK(expected > INT64_MAX && lookupDependentSymbol(&owner,
              "fixture_high_symbol", NULL, NULL) == (void *)expected,
              "high symbol address was truncated or used signed addition");
        check_object(&owner, oc);
    }
    stop(&owner, &host);
    /* Fail each initial heap allocation in turn, then dispose partial loads. */
    for (size_t failure = 1; failure <= fixture_allocations; ++failure) {
        start(&owner, &host, UINT32_C(0x10000000), 4096);
        host.fail_heap_at = failure;
        CHECK(!loadObj_(&owner, argv[1]) && owner.error_status == LL_E_NOMEM,
              "injected partial allocation failure lost its result");
        stop(&owner, &host);
    }
    start(&owner, &host, UINT32_C(0x10000000), 4096);
    CHECK(loadArchive_(&owner, argv[3]), "NO_EXEC archive load failed");
    CHECK(owner.gc.objects != NULL && owner.gc.objects->status == OBJECT_LOADED,
          "archive did not retain lazy member state");
    CHECK(lookupDependentSymbol(&owner, "fixture_high_symbol", NULL, NULL) != NULL,
          "lazy NO_EXEC archive symbol did not resolve");
    CHECK(runPendingInitializers(&owner), "NO_EXEC archive READY transition failed");
    stop(&owner, &host);
    start(&owner, &host, UINT32_C(0x10000000), 4096);
    CHECK(loadArchive_(&owner, argv[4]) && lookupDependentSymbol(&owner,
          "fixture_long_archive_member", NULL, NULL) != NULL &&
          runPendingInitializers(&owner), "GNU filename-index/lazy member path failed");
    stop(&owner, &host);
    const int tls_arguments[] = {5, 7};
    const char *tls_errors[] = {"ELF TLSGD is unavailable in NO_EXEC mode",
                               "ELF GOTTPOFF is unavailable in NO_EXEC mode"};
    for (size_t tls = 0; tls < sizeof(tls_arguments) / sizeof(tls_arguments[0]); ++tls) {
        start(&owner, &host, UINT32_C(0x10000000), 4096);
        CHECK(loadObj_(&owner, argv[tls_arguments[tls]]), "valid TLS fixture did not load");
        oc = owner.gc.objects;
        if (oc != NULL) {
            bool rejected = false;
            for (int i = 0; i < oc->n_sections; ++i) {
                ll_elf_target_shdr *headers = oc->elf_info->sectionHeader;
                if (headers[i].sh_type != LL_ELF_SHT_RELA) continue;
                rejected = !do_Elf_Rela_relocations(oc, oc->image, headers, i) &&
                           owner.error_status == LL_E_UNSUPPORTED &&
                           strcmp(owner.error_message, tls_errors[tls]) == 0;
                if (rejected) break;
            }
            CHECK(rejected, "NO_EXEC TLS relocation reached a host TLS service");
            ll_error_clear(&owner);
            CHECK(!resolveObjs_(&owner) && owner.error_status == LL_E_UNSUPPORTED,
                  "core TLS resolution did not preserve unsupported classification");
        }
        stop(&owner, &host);
    }
    start(&owner, &host, UINT32_C(0x10000000), 4096);
    CHECK(loadObj_(&owner, argv[6]), "retry control did not load");
    oc = owner.gc.objects;
    CHECK(!resolveObjs_(&owner) && owner.error_status == LL_E_UNRESOLVED &&
          oc != NULL && !oc->resolution_failed, "missing definition became terminal");
    ll_error_clear(&owner);
    CHECK(define(&owner, "external_data", 0x12345000) && resolveObjs_(&owner) &&
          oc != NULL && oc->status == OBJECT_READY && !oc->initializersRan,
          "explicit definition did not permit ordinary retry");
    stop(&owner, &host);
    if (failures != 0) return 1;
    (void)puts("noexec_core: PASS (symbols, heap owners, lifecycle, lazy archive, failures)");
    return 0;
}
