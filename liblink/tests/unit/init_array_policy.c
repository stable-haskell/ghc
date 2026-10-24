#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)
#define ocRunInit_ELF initializer_policy_backend
int initializer_policy_backend(ObjectCode *object);
#elif defined(OBJFORMAT_PEi386)
#define ocRunInit_PEi386 initializer_policy_backend
bool initializer_policy_backend(ObjectCode *object);
#elif defined(OBJFORMAT_MACHO)
#define ocRunInit_MachO initializer_policy_backend
int initializer_policy_backend(ObjectCode *object);
#else
#error "init_array_policy requires a configured native ELF/PE/Mach-O host"
#endif
#include "Linker.c"
#undef ocRunInit_ELF
#undef ocRunInit_PEi386
#undef ocRunInit_MachO
#include "linker/InitFini.c"
#include "ll_failure.c"

#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    void *owned[256];
    size_t live;
    unsigned fatal_calls;
    unsigned log_calls;
} Host;

static Host host_a;
static Host host_default;
static unsigned failures;

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "init_array_policy:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void remember(Host *host, void *pointer)
{
    if (pointer == NULL || host->live == sizeof(host->owned) / sizeof(host->owned[0]))
        _Exit(99);
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
    if (size != 0 && count > SIZE_MAX / size) _Exit(99);
    void *pointer = calloc(count == 0 || size == 0 ? 1 : count, size == 0 ? 1 : size);
    remember(host, pointer);
    return pointer;
}

static void *host_realloc(void *ctx, void *pointer, size_t size, const char *what)
{
    Host *host = ctx;
    if (pointer == NULL) return host_malloc(ctx, size, what);
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == pointer) {
            void *result = realloc(pointer, size == 0 ? 1 : size);
            if (result == NULL) _Exit(99);
            host->owned[i] = result;
            return result;
        }
    }
    CHECK(false, "realloc used a pointer outside the owner ledger");
    _Exit(99);
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
    CHECK(false, "free used a pointer outside the owner ledger");
}

static void host_log(void *ctx, int kind, const char *format, va_list args)
{
    ++((Host *)ctx)->log_calls;
    (void)kind;
    (void)format;
    (void)args;
}

static _Noreturn void host_fatal(void *ctx, const char *format, va_list args)
{
    ++((Host *)ctx)->fatal_calls;
    (void)vfprintf(stderr, format, args);
    _Exit(98);
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
    return ops;
}

/* The backend wrapper calls the real shared initializer helper. Stack objects
 * model already-resolved objects; parsing, mappings and GC disposal are outside
 * this unit. Logical unload uses unloadObj_; reload uses a fresh same-name
 * object. Native GHC T1-T3 qualify ordinary file loading and GC lifetimes. */
typedef struct {
    ObjectCode *object;
    struct InitFiniList *head;
    Section section;
    init_t entries[3];
    unsigned backends;
    unsigned before;
    unsigned after;
    unsigned prefix;
    unsigned tail;
    bool last_ok;
} InitCase;

static InitCase cases[3];
static InitCase *active_case;

static InitCase *case_for(ObjectCode *object)
{
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i)
        if (cases[i].object == object) return &cases[i];
    CHECK(false, "unexpected object reached initializer backend or hook");
    return NULL;
}

static void prefix(int n, char **a, char **e)
{
    CHECK(n == 0 && a == NULL && e == NULL, "default initializer arguments differ");
    CHECK(active_case != NULL, "initializer has no active fixture");
    if (active_case != NULL) ++active_case->prefix;
}

static void tail(int n, char **a, char **e)
{
    CHECK(n == 0 && a == NULL && e == NULL, "default initializer arguments differ");
    CHECK(active_case != NULL, "initializer has no active fixture");
    if (active_case != NULL) ++active_case->tail;
}

static void policy_before(void *ctx, ObjectCode *object)
{
    CHECK(ctx == &host_a, "before-init hook used another owner");
    InitCase *test = case_for(object);
    if (test != NULL) ++test->before;
}

static void policy_after(void *ctx, ObjectCode *object, bool ok)
{
    CHECK(ctx == &host_a, "after-init hook used another owner");
    InitCase *test = case_for(object);
    if (test != NULL) { ++test->after; test->last_ok = ok; }
}

#if defined(OBJFORMAT_PEi386)
bool initializer_policy_backend(ObjectCode *object)
#else
int initializer_policy_backend(ObjectCode *object)
#endif
{
    InitCase *test = case_for(object);
    if (test == NULL) return false;
    ++test->backends;
    active_case = test;
    bool result = runInit(object->L, &test->head);
    active_case = NULL;
    return result;
}

static void prepare_case(InitCase *test, ObjectCode *object, bool bad)
{
    test->object = object;
    test->entries[0] = prefix;
    test->entries[1] = bad ? NULL : tail;
    test->entries[2] = tail;
    test->section.start = test->entries;
    test->section.size = (bad ? 3 : 2) * sizeof(init_t);
    addInitFini(object->L, &test->head, &test->section, INITFINI_INIT_ARRAY, 10);
}

static void register_symbol(ll_linker *L, ObjectCode *object, Symbol_t *symbol)
{
    object->symbols = LL_MALLOC_FOR(L, sizeof(*symbol), "init policy symbol");
    *object->symbols = *symbol;
    object->n_symbols = 1;
    CHECK(ghciInsertSymbolTable(L, object->fileName, symbol->name, symbol->addr,
                               LL_STRENGTH_NORMAL, symbol->type, 0, object),
          "could not register modeled object symbol");
}

#if defined(OBJFORMAT_ELF)
static unsigned disposal_init_calls;
static unsigned disposal_fini_calls;

static void disposal_init(int argc, char **argv, char **envp)
{
    CHECK(argc == 0 && argv == NULL && envp == NULL,
          "ELF disposal initializer arguments differ");
    ++disposal_init_calls;
}

static void disposal_fini(void)
{
    ++disposal_fini_calls;
}

/* Use normal physical disposal, not ll_discard_object or manual list cleanup.
 * The native archive supplies the real ELF deinitializer and finalizer path.
 * Synthetic resolved objects isolate metadata ownership from file parsing. */
static void check_elf_disposal(ll_linker *owner)
{
    for (unsigned mode = 0; mode < 3; ++mode) {
        const size_t before = host_a.live;
        const unsigned init_before = disposal_init_calls;
        const unsigned fini_before = disposal_fini_calls;
        ObjectCode *object = mkOc(owner, STATIC_OBJECT, WSTR("elf-init-disposal.o"),
                                  NULL, 0, false, NULL, 0);
        object->status = OBJECT_RESOLVED;
        object->info = LL_CALLOC_FOR(owner, 1, sizeof(*object->info),
                                     "ELF initializer disposal metadata");
        init_t entries[2] = {disposal_init, mode == 0 ? NULL : disposal_init};
        fini_t finalizer = disposal_fini;
        Section init_section = {0};
        Section fini_section = {0};
        init_section.start = entries;
        init_section.size = sizeof(entries);
        fini_section.start = &finalizer;
        fini_section.size = sizeof(finalizer);
        addInitFini(owner, &object->info->init, &init_section,
                    INITFINI_INIT_ARRAY, 10);
        addInitFini(owner, &object->info->fini, &fini_section,
                    INITFINI_FINI, 10);
        ll_error_clear(owner);
        if (mode != 2) {
            const bool ok = runInit(owner, &object->info->init);
            CHECK(ok == (mode == 1), "ELF disposal initializer result differs");
            object->initializersRan = ok;
            object->resolution_failed = !ok;
            if (ok) {
                object->status = OBJECT_READY;
                CHECK(object->info->init == NULL,
                      "successful ELF initialization retained its list");
            } else {
                CHECK(owner->error_status == LL_E_INIT && object->info->init != NULL,
                      "failed ELF initialization lost its error or pending list");
            }
        }
        CHECK(disposal_init_calls == init_before + (mode == 0 ? 1 : mode == 1 ? 2 : 0),
              "ELF disposal initializer prefix differs");
        freeObjectCode(object);
        CHECK(disposal_fini_calls == fini_before + (mode == 1 ? 1 : 0),
              "ELF disposal ran finalizers without completed initialization");
        CHECK(host_a.live == before,
              "normal ELF disposal leaked retained initializer/finalizer metadata");
        if (mode == 0)
            CHECK(owner->error_status == LL_E_INIT,
                  "normal ELF disposal lost the original initializer error");
    }
    ll_error_clear(owner);
    (void)puts("init_array_policy: ELF physical disposal controls complete");
}
#endif

int main(void)
{
    ll_linker owner = {0};
    ll_compat_host_ops ops = make_ops(&host_a);
    ll_compat_host_ops defaults = make_ops(&host_default);
    ops.before_init = policy_before;
    ops.after_init = policy_after;
    ll_install_host_for(&ll_default_linker, &defaults, NULL);
    ll_error_set(&ll_default_linker, LL_E_NOT_FOUND, "default init policy sentinel");
    ll_install_host_for(&owner, &ops, NULL);
    ll_init_linker(&owner, 0);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
#if defined(OBJFORMAT_ELF)
    check_elf_disposal(&owner);
#endif
    ObjectCode objects[5] = {{0}, {0}, {0}, {0}, {0}};
    pathchar *paths[5] = {WSTR("failed-init.o"), WSTR("init-child.o"),
                         WSTR("init-grandchild.o"), WSTR("healthy-init.o"),
                         WSTR("failed-init.o")};
    for (size_t i = 0; i < 5; ++i) {
        objects[i].L = &owner;
        objects[i].fileName = paths[i];
        objects[i].status = OBJECT_RESOLVED;
        if (i < 3) {
            objects[i].next = &objects[i + 1];
            objects[i].next_loaded_object = &objects[i + 1];
            objects[i + 1].prev = &objects[i];
        }
    }
    owner.gc.objects = &objects[0];
    owner.gc.loaded_objects = &objects[0];
    objects[1].dependencies = allocHashSet(&owner);
    objects[2].dependencies = allocHashSet(&owner);
    insertHashSet(objects[1].dependencies, (ll_word)(uintptr_t)&objects[0]);
    insertHashSet(objects[2].dependencies, (ll_word)(uintptr_t)&objects[1]);
    int values[3] = {11, 22, 33};
    Symbol_t symbols[3] = {
        {(ll_symbol_name *)"_failed_init_symbol", &values[0], LL_SYM_DATA},
        {(ll_symbol_name *)"_healthy_init_symbol", &values[1], LL_SYM_DATA},
        {(ll_symbol_name *)"_failed_init_symbol", &values[2], LL_SYM_DATA}
    };
    register_symbol(&owner, &objects[0], &symbols[0]);
    register_symbol(&owner, &objects[3], &symbols[1]);
    prepare_case(&cases[0], &objects[0], true);
    prepare_case(&cases[1], &objects[3], false);
    prepare_case(&cases[2], &objects[4], false);
    struct ll_failure_scope scope;
    ll_failure_begin(&owner, &scope, NULL);
    CHECK(lookupSymbol_(&owner, (ll_symbol_name *)"_failed_init_symbol") == NULL &&
              owner.error_status == LL_E_INIT,
          "first initialization-triggering lookup exposed an address or lost INIT");
    char first_error[LL_ERROR_CAPACITY];
    memcpy(first_error, owner.error_message, sizeof(first_error));
    ll_failure_end(&owner, &scope);
    CHECK(memcmp(first_error, owner.error_message, sizeof(first_error)) == 0,
          "scope cleanup changed the complete first diagnostic");
    CHECK(objects[0].status == OBJECT_RESOLVED && !objects[0].initializersRan,
          "failed initialization reported READY or completed initializers");
    CHECK(objects[0].resolution_failed && objects[1].resolution_failed &&
              objects[2].resolution_failed && !objects[3].resolution_failed,
          "initializer quarantine did not follow exactly the dependency closure");
    CHECK(cases[0].prefix == 1 && cases[0].tail == 0 && cases[0].backends == 1 &&
              cases[0].before == 1 && cases[0].after == 1 && !cases[0].last_ok,
          "first failure changed constructor prefix or hook outcome/count");
    ll_error_clear(&owner);
    CHECK(ocRunInit(&objects[0]) == 0 && owner.error_status == LL_E_STATE,
          "repeated initializer call did not reject quarantined object");
    CHECK(cases[0].prefix == 1 && cases[0].backends == 1 && cases[0].before == 1,
          "repeated initializer call replayed constructors, hooks or backend");
    ll_error_clear(&owner);
    CHECK(ocRunInit(&objects[1]) == 0 && owner.error_status == LL_E_STATE,
          "dependent initializer did not reject quarantine before backend");
    CHECK(lookupSymbol_(&owner, (ll_symbol_name *)"_failed_init_symbol") == NULL &&
              owner.error_status == LL_E_STATE,
          "quarantined cached symbol remained visible");
    CHECK(lookupSymbol_(&owner, (ll_symbol_name *)"_healthy_init_symbol") == &values[1],
          "independent resolved symbol became unusable");
    CHECK(resolveObjs_(&owner) == 0 && owner.error_status == LL_E_STATE,
          "global resolve accepted an active initialization quarantine");
    ll_error_clear(&owner);
    CHECK(ocRunInit(&objects[3]) != 0 && objects[3].status == OBJECT_READY &&
              objects[3].initializersRan && cases[1].prefix == 1 &&
              cases[1].tail == 1 && cases[1].last_ok,
          "independent healthy initialization did not succeed");
    for (size_t i = 0; i < 3; ++i)
        CHECK(unloadObj_(&owner, paths[i], false) != 0,
              "logical unload rejected an affected object");
    CHECK(resolveObjs_(&owner) != 0,
          "global resolve remained blocked after all affected objects unloaded");
    objects[4].next = owner.gc.objects;
    objects[4].next->prev = &objects[4];
    owner.gc.objects = &objects[4];
    objects[4].next_loaded_object = owner.gc.loaded_objects;
    owner.gc.loaded_objects = &objects[4];
    register_symbol(&owner, &objects[4], &symbols[2]);
    CHECK(ocRunInit(&objects[4]) != 0 && !objects[4].resolution_failed &&
              objects[4].initializersRan && cases[2].prefix == 1 && cases[2].tail == 1,
          "fresh same-name object inherited failure or could not initialize");
    CHECK(lookupSymbol_(&owner, (ll_symbol_name *)"_failed_init_symbol") == &values[2],
          "fresh same-name object did not expose its new symbol");
    CHECK(unloadObj_(&owner, paths[3], false) != 0 &&
              unloadObj_(&owner, paths[4], false) != 0,
          "healthy fixture cleanup failed to unload");
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i)
        freeInitFiniList(&owner, cases[i].head);
    freeHashSet(objects[1].dependencies);
    freeHashSet(objects[2].dependencies);
    owner.gc.objects = NULL;
    owner.gc.loaded_objects = NULL;
    owner.gc.n_unloaded_objects = 0;
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    CHECK(host_a.live == 0 && host_a.fatal_calls == 0,
          "policy fixture leaked owner storage or used fatal callback");
    CHECK(host_default.live == 0 && host_default.fatal_calls == 0 &&
              ll_default_linker.error_status == LL_E_NOT_FOUND &&
              strcmp(ll_default_linker.error_message,
                     "default init policy sentinel") == 0,
          "policy fixture changed default owner state");
    if (failures != 0) return 1;
    (void)puts("init_array_policy: PASS");
    return 0;
}
