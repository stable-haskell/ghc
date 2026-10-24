/* Owner-scoped POSIX native-load cleanup and retry controls.
 * Global fixture state dispatches allocator/hook observations in this single-
 * threaded unit; it is not library state or an RTS foreign-export model. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "LinkerInternals.h"
#include "linker/LoadNativeObjPosix.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if defined(HAVE_DLINFO)
#define FAILED_LOAD_ATTEMPTS 2
#else
#define FAILED_LOAD_ATTEMPTS 1
#endif

#define REQUIRE(p) do { if (!(p)) { \
    fprintf(stderr, "native probe line %u: %s\n", (unsigned)__LINE__, #p); \
    exit(99); } } while (0)

typedef struct {
    ll_linker *owner;
    void *live[128];
    const char *tags[128];
    size_t sizes[128];
    size_t allocations, frees, before, after, registered, batches, logs;
    unsigned unloaded;
    bool inject_failed_host_state;
    unsigned events[32];
    size_t n_events;
    ObjectCode *attempt;
} Host;

static Host *active;

static void remember(Host *host, void *p, const char *tag, size_t bytes)
{
    REQUIRE(host == active && p != NULL);
    for (size_t i = 0; i < 128; i++) {
        if (host->live[i] == NULL) {
            host->live[i] = p;
            host->tags[i] = tag;
            host->sizes[i] = bytes;
            host->allocations++;
            return;
        }
    }
    REQUIRE(false);
}

static void *allocate(void *ctx, size_t size, const char *tag)
{
    Host *host = ctx;
    REQUIRE(tag != NULL);
    void *p = malloc(size);
    remember(host, p, tag, size);
    return p;
}

static void *allocate_zero(void *ctx, size_t count, size_t size, const char *tag)
{
    Host *host = ctx;
    REQUIRE(tag != NULL && (size == 0 || count <= SIZE_MAX / size));
    void *p = calloc(count, size);
    remember(host, p, tag, count * size);
    return p;
}

static void *resize(void *ctx, void *old, size_t size, const char *tag)
{
    Host *host = ctx;
    REQUIRE(host == active && tag != NULL);
    if (old == NULL) {
        return allocate(ctx, size, tag);
    }
    for (size_t i = 0; i < 128; i++) {
        if (host->live[i] == old) {
            void *p = realloc(old, size);
            REQUIRE(p != NULL);
            host->live[i] = p;
            return p;
        }
    }
    REQUIRE(false);
    return NULL;
}

static void release(void *ctx, void *p)
{
    Host *host = ctx;
    REQUIRE(host == active);
    if (p == NULL) return;
    for (size_t i = 0; i < 128; i++) {
        if (host->live[i] == p) {
            host->live[i] = NULL;
            host->frees++;
            free(p);
            return;
        }
    }
    REQUIRE(false);
}

static size_t page_size(void *ctx)
{
    REQUIRE(ctx == active);
    long size = sysconf(_SC_PAGESIZE);
    REQUIRE(size > 0);
    return (size_t)size;
}

static void log_message(void *ctx, int kind, const char *fmt, va_list args)
{
    Host *host = ctx;
    REQUIRE(host == active && fmt != NULL);
    (void)kind; (void)args;
    host->logs++;
}

static _Noreturn void fatal(void *ctx, const char *fmt, va_list args)
{
    REQUIRE(ctx == active && fmt != NULL);
    vfprintf(stderr, fmt, args);
    exit(98);
}

static void event(Host *host, unsigned value)
{
    REQUIRE(host == active && host->n_events < 32);
    host->events[host->n_events++] = value;
}

static void before(void *ctx, ObjectCode *oc)
{
    Host *host = ctx;
    REQUIRE(oc->L == host->owner && oc->type == DYNAMIC_OBJECT);
    host->attempt = oc;
    host->before++;
    event(host, 1);
}

static void after(void *ctx, ObjectCode *oc, bool success)
{
    Host *host = ctx;
    REQUIRE(oc == host->attempt && oc->L == host->owner);
    REQUIRE(success == (oc->dlopen_handle != NULL));
    host->after++;
    event(host, 2);
    if (!success && host->inject_failed_host_state && oc->foreign_exports == NULL) {
        int *cookie = allocate(ctx, sizeof(*cookie), "native host cookie");
        *cookie = 73;
        /* Opaque fixture-owned state, not a modeled RTS export list. */
        oc->foreign_exports = (struct ForeignExportsList *)cookie;
    }
}

static void registered(void *ctx, ObjectCode *oc)
{
    Host *host = ctx;
    REQUIRE(oc == host->attempt && oc->L == host->owner);
    oc->next = host->owner->gc.objects;
    host->owner->gc.objects = oc;
    host->registered++;
    event(host, 3);
}

static void batch(void *ctx)
{
    Host *host = ctx;
    REQUIRE(host->owner->gc.loaded_objects == host->attempt);
    host->batches++;
    event(host, 4);
}

static void failed_native_unload(void *ctx, ObjectCode *oc)
{
    Host *host = ctx;
    REQUIRE(host == active && oc == host->attempt && oc->L == host->owner);
    REQUIRE(oc->type == DYNAMIC_OBJECT && oc->status == OBJECT_READY);
    REQUIRE(oc->dlopen_handle == NULL);
    REQUIRE(host->before == FAILED_LOAD_ATTEMPTS && host->after == FAILED_LOAD_ATTEMPTS);
    REQUIRE(host->registered == 0 && host->batches == 0 && host->unloaded == 0);
    /* Metadata must still belong to this owner when attached state is freed. */
    bool object_live = false;
    for (size_t i = 0; i < 128; ++i) object_live |= host->live[i] == oc;
    REQUIRE(object_live);
    if (host->inject_failed_host_state) {
        int *cookie = (int *)oc->foreign_exports;
        REQUIRE(cookie != NULL && *cookie == 73);
        release(ctx, cookie);
        oc->foreign_exports = NULL;
    } else {
        REQUIRE(oc->foreign_exports == NULL);
    }
    ++host->unloaded;
    event(host, 5);
    /* The caller's error copy must survive loader-error storage changing. */
    REQUIRE(dlopen("/liblink-cleanup-probe-missing-second.dylib", RTLD_LAZY) == NULL);
    REQUIRE(dlerror() != NULL);
}

static void install(ll_linker *L, Host *host)
{
    host->owner = L;
    ll_compat_host_ops ops = {
        .ctx = host, .malloc_ = allocate, .calloc_ = allocate_zero,
        .realloc_ = resize, .free_ = release, .page_size = page_size,
        .log = log_message, .fatal = fatal, .before_native_load = before,
        .after_native_load = after, .object_registered = registered,
        .after_init_batch = batch, .on_unload = failed_native_unload
    };
    ll_compat_config cfg = {.debug_mask = LL_DBG_LINKER};
    ll_install_host_for(L, &ops, &cfg);
}

static void *load(Host *host, char *path, char **error)
{
    active = host;
    ll_mutex_lock_for(host->owner, &host->owner->linker_mutex);
    void *handle = loadNativeObj_POSIX(host->owner, path, error);
    ll_mutex_unlock_for(host->owner, &host->owner->linker_mutex);
    return handle;
}

static void dispose(Host *host)
{
    active = host;
    ObjectCode *oc = host->owner->gc.objects;
    REQUIRE(oc != NULL && oc == host->owner->gc.loaded_objects && oc->L == host->owner);
    host->owner->gc.objects = oc->next;
    host->owner->gc.loaded_objects = oc->next_loaded_object;
    freeObjectCode(oc);
    REQUIRE(host->owner->gc.objects == NULL && host->owner->gc.loaded_objects == NULL);
    REQUIRE(host->allocations == host->frees);
    for (size_t i = 0; i < 128; i++) REQUIRE(host->live[i] == NULL);
}

int main(int argc, char **argv)
{
    REQUIRE(argc == 3);
    ll_linker owners[2] = {{0}};
    Host hosts[3] = {{0}};
    for (size_t i = 0; i < 3; i++) {
        active = &hosts[i];
        install(i < 2 ? &owners[i] : &ll_default_linker, &hosts[i]);
        if (i < 2) ll_mutex_init_for(&owners[i].services, &owners[i].linker_mutex);
    }
    char *error = NULL;
    if (strcmp(argv[1], "missing") == 0 ||
        strcmp(argv[1], "missing-host-state") == 0 ||
        strcmp(argv[1], "missing-no-unload-hook") == 0) {
        Host *host = &hosts[1];
        const unsigned cleanup_calls =
            strcmp(argv[1], "missing-no-unload-hook") == 0 ? 0u : 1u;
        if (cleanup_calls == 0) owners[1].services.host.on_unload = NULL;
        host->inject_failed_host_state = strcmp(argv[1], "missing-host-state") == 0;
        for (unsigned attempt = 0; attempt < 2; ++attempt) {
            host->before = host->after = host->registered = host->batches = 0;
            host->unloaded = 0;
            host->n_events = 0;
            host->attempt = NULL;
            error = NULL;
            REQUIRE(load(host, argv[2], &error) == NULL);
            REQUIRE(error != NULL && strstr(error, argv[2]) != NULL);
            REQUIRE(host->before == FAILED_LOAD_ATTEMPTS && host->after == FAILED_LOAD_ATTEMPTS);
            REQUIRE(host->registered == 0 && host->batches == 0);
            REQUIRE(owners[1].gc.objects == NULL && owners[1].gc.loaded_objects == NULL);
            REQUIRE(hosts[0].allocations == 0 && hosts[2].allocations == 0);
            release(host, error);
            size_t retained = 0;
            for (size_t i = 0; i < 128; ++i) retained += host->live[i] != NULL;
            printf("attempt=%u retained=%zu cleanup=%u host_state=%d\n",
                   attempt, retained, host->unloaded, host->inject_failed_host_state);
            /* Diagnostic fallback cleanup follows observation of any residue.
             * Do not dereference host->attempt: the candidate already freed it. */
            for (size_t i = 0; i < 128; ++i) {
                if (host->live[i] != NULL) release(host, host->live[i]);
            }
            REQUIRE(host->allocations == host->frees);
            REQUIRE(retained == 0 && host->unloaded == cleanup_calls);
            REQUIRE(host->n_events == 2 * FAILED_LOAD_ATTEMPTS + cleanup_calls);
            for (unsigned i = 0; i < FAILED_LOAD_ATTEMPTS; ++i) {
                REQUIRE(host->events[2 * i] == 1 && host->events[2 * i + 1] == 2);
            }
            if (cleanup_calls != 0) {
                REQUIRE(host->events[2 * FAILED_LOAD_ATTEMPTS] == 5);
            }
        }
        return 0;
    }
    if (strcmp(argv[1], "collision") == 0) {
        ObjectCode oc = {.L = &owners[0], .type = STATIC_OBJECT,
                         .status = OBJECT_READY, .fileName = argv[2]};
        owners[0].gc.objects = &oc;
        REQUIRE(load(&hosts[0], argv[2], &error) == NULL);
        REQUIRE(error != NULL && strcmp(error,
            "loadNativeObj_POSIX: already loaded as non-dynamic object") == 0);
        REQUIRE(hosts[0].before == 0 && hosts[0].registered == 0);
        release(&hosts[0], error);
        REQUIRE(hosts[0].allocations == hosts[0].frees);
        REQUIRE(hosts[1].allocations == 0 && hosts[2].allocations == 0);
        return 0;
    }
    if (strcmp(argv[1], "lazy") == 0) {
        void *handle = load(&hosts[0], argv[2], &error);
        REQUIRE(handle != NULL && error == NULL && hosts[0].unloaded == 0);
#if defined(HAVE_DLINFO)
        REQUIRE(hosts[0].before == 2 && hosts[0].after == 2);
        const unsigned expected[] = {1, 2, 1, 2, 3, 4};
#else
        REQUIRE(hosts[0].before == 1 && hosts[0].after == 1);
        const unsigned expected[] = {1, 2, 3, 4};
#endif
        REQUIRE(hosts[0].registered == 1 && hosts[0].batches == 1);
        REQUIRE(hosts[0].n_events == sizeof(expected) / sizeof(expected[0]));
        REQUIRE(memcmp(hosts[0].events, expected, sizeof(expected)) == 0);
        int *value = dlsym(handle, "ll_owner_fixture_value");
        REQUIRE(value != NULL && *value == 73);
        REQUIRE(!hosts[0].attempt->unloadable && hosts[0].attempt->nc_ranges == NULL);
        dispose(&hosts[0]);
        REQUIRE(hosts[1].allocations == 0 && hosts[2].allocations == 0);
        return 0;
    }
    REQUIRE(strcmp(argv[1], "positive") == 0);
    void *handles[2];
    for (size_t i = 0; i < 2; i++) {
        handles[i] = load(&hosts[i], argv[2], &error);
        REQUIRE(handles[i] != NULL && error == NULL);
#if defined(HAVE_DLINFO)
        REQUIRE(hosts[i].attempt->unloadable && hosts[i].attempt->nc_ranges != NULL);
#else
        REQUIRE(!hosts[i].attempt->unloadable && hosts[i].attempt->nc_ranges == NULL);
#endif
        REQUIRE(hosts[i].before == 1 && hosts[i].after == 1 &&
                hosts[i].registered == 1 && hosts[i].batches == 1);
        unsigned expected[4] = {1, 2, 3, 4};
        REQUIRE(hosts[i].n_events == 4 && memcmp(hosts[i].events, expected, sizeof(expected)) == 0);
        size_t allocations = hosts[i].allocations;
        REQUIRE(load(&hosts[i], argv[2], &error) == handles[i]);
        REQUIRE(hosts[i].allocations == allocations && hosts[i].n_events == 4);
        int *value = dlsym(handles[i], "ll_owner_fixture_value");
        REQUIRE(value != NULL && *value == 73);
    }
    REQUIRE(owners[0].gc.objects != owners[1].gc.objects);
    dispose(&hosts[0]);
    int *remaining = dlsym(handles[1], "ll_owner_fixture_value");
    REQUIRE(remaining != NULL && *remaining == 73);
    dispose(&hosts[1]);
    for (size_t i = 0; i < 2; i++) {
        active = &hosts[i];
        ll_mutex_close_for(&owners[i].services, &owners[i].linker_mutex);
        REQUIRE(hosts[i].logs > 0);
    }
    REQUIRE(hosts[2].allocations == 0 && hosts[2].frees == 0 && hosts[2].logs == 0);
    return 0;
}
