#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/MMap.h"
#include <fcntl.h>

static void *core_error_status_mmap(ll_linker *L, size_t bytes,
                                   MemoryAccess access, uint32_t flags,
                                   int fd, int offset);
#define ll_mmap_for_linker core_error_status_mmap
#include "Linker.c"
#undef ll_mmap_for_linker

#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

enum { MAX_OWNED = 256 };
typedef struct {
    const char *name;
    void *owned[MAX_OWNED];
    size_t live;
    unsigned alloc_calls;
    unsigned free_calls;
    unsigned log_calls;
    unsigned init_calls;
    unsigned exit_calls;
    unsigned fatal_calls;
} Host;

typedef struct {
    unsigned alloc_calls;
    unsigned free_calls;
    size_t live;
    StrHashTable *symhash;
    ObjectCode *objects;
    ObjectCode *loaded;
    uint64_t unloaded;
} Snapshot;

static Host host_a = {.name = "A"};
static Host host_b = {.name = "B"};
static Host host_default = {.name = "default"};
static unsigned failures;
static bool inject_map_failure;
static ll_linker *inject_map_owner;
static ll_linker *last_map_owner;
static int last_map_fd = -1;
static unsigned map_calls;
static bool injected_fd_was_live;

static void *core_error_status_mmap(ll_linker *L, size_t bytes,
                                   MemoryAccess access, uint32_t flags,
                                   int fd, int offset)
{
    ++map_calls;
    last_map_owner = L;
    last_map_fd = fd;
    if (inject_map_failure && L == inject_map_owner) {
        injected_fd_was_live = fcntl(fd, F_GETFD) >= 0;
        inject_map_failure = false;
        errno = ENOMEM;
        return NULL;
    }
    return ll_mmap_for_linker(L, bytes, access, flags, fd, offset);
}

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "core_error_status:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void remember(Host *host, void *ptr)
{
    CHECK(ptr != NULL, "allocator returned NULL unexpectedly");
    CHECK(host->live < MAX_OWNED, "allocator ledger overflow");
    if (ptr != NULL && host->live < MAX_OWNED) host->owned[host->live++] = ptr;
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    void *ptr = malloc(size == 0 ? 1 : size);
    ++host->alloc_calls;
    remember(host, ptr);
    return ptr;
}

static void *host_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    void *ptr = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    ++host->alloc_calls;
    remember(host, ptr);
    return ptr;
}

static void *host_realloc(void *ctx, void *ptr, size_t size, const char *what)
{
    Host *host = ctx;
    (void)what;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == ptr) {
            void *next = realloc(ptr, size == 0 ? 1 : size);
            if (next != NULL) host->owned[i] = next;
            return next;
        }
    }
    CHECK(false, "realloc pointer was not owned by caller");
    return NULL;
}

static void host_free(void *ctx, void *ptr)
{
    Host *host = ctx;
    if (ptr == NULL) return;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == ptr) {
            host->owned[i] = host->owned[--host->live];
            ++host->free_calls;
            free(ptr);
            return;
        }
    }
    CHECK(false, "free pointer was not owned by caller");
}

static void host_log(void *ctx, int kind, const char *fmt, va_list ap)
{
    Host *host = ctx;
    char message[1024];
    int n = vsnprintf(message, sizeof(message), fmt, ap);
    CHECK(n >= 0 && (size_t)n < sizeof(message), "log message truncated");
    (void)kind;
    ++host->log_calls;
}

static _Noreturn void host_fatal(void *ctx, const char *fmt, va_list ap)
{
    Host *host = ctx;
    char message[1024];
    (void)vsnprintf(message, sizeof(message), fmt, ap);
    ++host->fatal_calls;
    (void)fprintf(stderr, "unexpected fatal from %s: %s\n", host->name, message);
    _Exit(98);
}

static size_t host_page_size(void *ctx)
{
    (void)ctx;
    long size = sysconf(_SC_PAGESIZE);
    return size > 0 ? (size_t)size : 4096u;
}

static void host_mem_failure(void *ctx) { (void)ctx; }

static void on_init(void *ctx) { ++((Host *)ctx)->init_calls; }
static void on_exit(void *ctx) { ++((Host *)ctx)->exit_calls; }

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
    ops.on_mem_failure = host_mem_failure;
    ops.init_unload_check = on_init;
    ops.exit_unload_check = on_exit;
    return ops;
}

static Snapshot snapshot(ll_linker *L, Host *host)
{
    return (Snapshot){host->alloc_calls, host->free_calls, host->live,
                      L->symhash, L->gc.objects, L->gc.loaded_objects,
                      (uint64_t)L->gc.n_unloaded_objects};
}

static void check_snapshot(ll_linker *L, Host *host, Snapshot before,
                           const char *what)
{
    Snapshot after = snapshot(L, host);
    CHECK(after.alloc_calls == before.alloc_calls, what);
    CHECK(after.free_calls == before.free_calls, what);
    CHECK(after.live == before.live, what);
    CHECK(after.symhash == before.symhash, what);
    CHECK(after.objects == before.objects, what);
    CHECK(after.loaded == before.loaded, what);
    CHECK(after.unloaded == before.unloaded, what);
}

static void load_failure(ll_linker *L, Host *host, pathchar *path,
                         ll_status expected, const char *label)
{
    ll_mutex_lock_for(L, &L->linker_mutex);
    ll_error_clear(L);
    Snapshot before = snapshot(L, host);
    int result = loadObj_(L, path);
    ll_mutex_unlock_for(L, &L->linker_mutex);
    CHECK(result == 0, label);
    CHECK(L->error_status == expected, label);
    check_snapshot(L, host, before, label);
}

static void make_path(char *out, size_t capacity, const char *dir,
                      const char *leaf)
{
    int n = snprintf(out, capacity, "%s/%s", dir, leaf);
    CHECK(n >= 0 && (size_t)n < capacity, "temporary path overflow");
}

static void create_file(const char *path, bool nonempty)
{
    static const char contents[] = "liblink-map-failure-input\n";
    int fd = open(path, O_CREAT | O_EXCL | O_RDWR, 0600);
    CHECK(fd >= 0, "could not create temporary file");
    if (fd >= 0 && nonempty) {
        CHECK(write(fd, contents, sizeof(contents) - 1)
                  == (ssize_t)(sizeof(contents) - 1),
              "file write failed");
    }
    if (fd >= 0) CHECK(close(fd) == 0, "file close failed");
}

static void positive_mapping(ll_linker *L, const char *path)
{
    static const char contents[] = "liblink-map-failure-input\n";
    int fd = open(path, O_RDWR);
    CHECK(fd >= 0, "could not open positive mapping input");
    if (fd < 0) return;
    unsigned calls_before = map_calls;
    void *image = core_error_status_mmap(L, sizeof(contents) - 1,
                                        MEM_READ_WRITE, MAP_PRIVATE, fd, 0);
    CHECK(image != NULL && image != MAP_FAILED,
          "positive wrapper did not forward to the real mapper");
    CHECK(map_calls == calls_before + 1 && last_map_owner == L
              && last_map_fd == fd,
          "positive wrapper did not record the selected owner and fd");
    if (image != NULL && image != MAP_FAILED) {
        CHECK(memcmp(image, contents, sizeof(contents) - 1) == 0,
              "real mapping did not contain the file bytes");
        ll_munmap_for_linker(L, image, sizeof(contents) - 1,
                             "core_error_status positive mapping");
    }
    CHECK(close(fd) == 0, "positive mapping fd close failed");
}

static void mapping_failure(ll_linker *L, Host *host, const char *path)
{
    ll_mutex_lock_for(L, &L->linker_mutex);
    ll_error_clear(L);
    Snapshot before = snapshot(L, host);
    unsigned calls_before = map_calls;
    inject_map_owner = L;
    inject_map_failure = true;
    int result = loadObj_(L, (pathchar *)path);
    bool injected = !inject_map_failure;
    ll_linker *seen_owner = last_map_owner;
    int seen_fd = last_map_fd;
    bool seen_fd_was_live = injected_fd_was_live;
    Snapshot after = snapshot(L, host);
    ll_mutex_unlock_for(L, &L->linker_mutex);

    CHECK(result == 0 && L->error_status == LL_E_IO,
          "NULL mapping result did not return LL_E_IO");
    CHECK(injected && map_calls == calls_before + 1 && seen_owner == L
              && seen_fd >= 0 && seen_fd_was_live,
          "injected mapping did not capture the selected owner and live fd");
    CHECK(after.alloc_calls == before.alloc_calls
              && after.free_calls == before.free_calls
              && after.live == before.live
              && after.symhash == before.symhash
              && after.objects == before.objects
              && after.loaded == before.loaded
              && after.unloaded == before.unloaded,
          "mapping failure allocated an ObjectCode or changed owner lists");
    if (seen_fd >= 0) {
        errno = 0;
        CHECK(fcntl(seen_fd, F_GETFD) == -1 && errno == EBADF,
              "mapping failure did not close the captured input fd");
    }
    CHECK(!inject_map_failure, "mapping failure injection was not consumed");
}

static void cleanup_inputs(const char *empty_path, const char *loaded_path,
                           const char *large_path, const char *dir)
{
    CHECK(unlink(empty_path) == 0 && unlink(loaded_path) == 0
              && unlink(large_path) == 0 && rmdir(dir) == 0,
          "temporary input cleanup failed");
}

static void archive_dispatch(ll_linker *L, Host *host, const char *dir)
{
    static const char magic[] = "!<arch>\n";
    static const char incomplete_member[] = "short";
    char path[256];
    make_path(path, sizeof(path), dir, "archive.o");
    int fd = open(path, O_CREAT | O_EXCL | O_RDWR, 0600);
    CHECK(fd >= 0, "archive dispatch fixture create failed");
    if (fd < 0) return;
    CHECK(write(fd, magic, sizeof(magic) - 1) == (ssize_t)(sizeof(magic) - 1),
          "archive dispatch magic write failed");

    for (unsigned malformed = 0; malformed < 2; ++malformed) {
        if (malformed) {
            CHECK(write(fd, incomplete_member, sizeof(incomplete_member) - 1)
                      == (ssize_t)(sizeof(incomplete_member) - 1),
                  "archive dispatch truncated member write failed");
        }
        ll_mutex_lock_for(L, &L->linker_mutex);
        ll_error_clear(L);
        Snapshot before = snapshot(L, host);
        unsigned maps_before = map_calls;
        int result = loadObj_(L, path);
        Snapshot after = snapshot(L, host);
        ll_mutex_unlock_for(L, &L->linker_mutex);
        CHECK(result == (malformed ? 0 : 1)
                  && L->error_status == (malformed ? LL_E_FORMAT : LL_OK),
              "loadObj did not preserve archive success or failure");
        CHECK(map_calls == maps_before,
              "recognized archive was retried as an ordinary object");
        CHECK(after.live == before.live && after.symhash == before.symhash
                  && after.objects == before.objects && after.loaded == before.loaded
                  && after.unloaded == before.unloaded
                  && after.alloc_calls - before.alloc_calls
                      == after.free_calls - before.free_calls,
              "archive dispatch changed owner state or leaked reader allocations");
        if (malformed) {
            CHECK(strncmp(L->error_message, "loadArchive:", 12) == 0,
                  "ordinary object fallback replaced the archive diagnostic");
        }
    }
    CHECK(close(fd) == 0 && unlink(path) == 0,
          "archive dispatch fixture cleanup failed");
}

int main(int argc, char **argv)
{
    ll_linker owner_a = {0};
    ll_linker owner_b = {0};
    ll_compat_host_ops ops_a = make_ops(&host_a);
    ll_compat_host_ops ops_b = make_ops(&host_b);
    ll_compat_host_ops ops_default = make_ops(&host_default);
    char default_message[] = "default-owner-sentinel";
    char dir_template[128];
    char empty_path[256], loaded_path[256], large_path[256], missing_path[256];
    char duplicate_path_a[] = "duplicate-owner-a.o";
    char duplicate_path_b[] = "duplicate-owner-b.o";
    bool mapping_only = argc == 2 && strcmp(argv[1], "--mapping-only") == 0;
    int value_a = 11, value_b = 22;
    ObjectCode duplicate_a = {0}, duplicate_b = {0};
    int ok;

    ll_install_host_for(&ll_default_linker, &ops_default, NULL);
    ll_install_host_for(&owner_a, &ops_a, NULL);
    ll_install_host_for(&owner_b, &ops_b, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_message,
           sizeof(default_message));

    ll_init_linker(&owner_a, 0);
    ll_init_linker(&owner_b, 0);
    CHECK(owner_a.linker_init_done == 1 && owner_a.symhash != NULL,
          "owner A product initializer did not initialize state");
    CHECK(owner_b.linker_init_done == 1 && owner_b.symhash != NULL,
          "owner B product initializer did not initialize state");
    CHECK(host_a.init_calls == 1 && host_b.init_calls == 1,
          "product init hooks did not run once per owner");
    /* Keep the real positive map out of the low-address linker region. */
    owner_a.services.cfg.always_pic = true;

    int n = snprintf(dir_template, sizeof(dir_template),
                     "/tmp/liblink-status-%ld-XXXXXX", (long)getpid());
    CHECK(n >= 0 && (size_t)n < sizeof(dir_template), "temp directory path overflow");
    char *dir = mkdtemp(dir_template);
    CHECK(dir != NULL, "mkdtemp failed");
    if (dir == NULL) return 2;
    make_path(empty_path, sizeof(empty_path), dir, "empty.o");
    make_path(loaded_path, sizeof(loaded_path), dir, "already-loaded.o");
    make_path(large_path, sizeof(large_path), dir, "sparse-large.o");
    make_path(missing_path, sizeof(missing_path), dir, "missing.o");
    create_file(empty_path, false);
    create_file(loaded_path, true);
    positive_mapping(&owner_a, loaded_path);
    mapping_failure(&owner_a, &host_a, loaded_path);
    int large_fd = open(large_path, O_CREAT | O_EXCL | O_RDWR, 0600);
    CHECK(large_fd >= 0, "large sparse file create failed");
    if (large_fd >= 0) {
        CHECK(ftruncate(large_fd, (off_t)INT_MAX + 1) == 0,
              "could not create sparse > INT_MAX input");
        CHECK(close(large_fd) == 0, "large sparse file close failed");
    }
    struct stat large_stat;
    CHECK(stat(large_path, &large_stat) == 0 && large_stat.st_size > INT_MAX,
          "sparse test input is not larger than INT_MAX");
    if (mapping_only) {
        CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
                  && strcmp(ll_default_linker.error_message,
                            "default-owner-sentinel") == 0,
              "mapping operation modified default error sentinel");
        ll_exit_linker(&owner_a);
        ll_exit_linker(&owner_b);
        CHECK(host_a.live == 0 && host_b.live == 0,
              "mapping-only product exit left owner allocations live");
        cleanup_inputs(empty_path, loaded_path, large_path, dir);
        return failures == 0 ? 0 : 1;
    }

    load_failure(&owner_a, &host_a, missing_path, LL_E_IO,
                 "missing input did not return IO without state changes");
    load_failure(&owner_b, &host_b, empty_path, LL_E_FORMAT,
                 "empty input did not return FORMAT without state changes");
    load_failure(&owner_a, &host_a, NULL, LL_E_STATE,
                 "NULL path did not return STATE without state changes");
    load_failure(&owner_b, &host_b, large_path, LL_E_UNSUPPORTED,
                 "large input did not return UNSUPPORTED without state changes");
    archive_dispatch(&owner_b, &host_b, dir);

    ObjectCode loaded = {.L = &owner_a, .fileName = loaded_path,
                         .status = OBJECT_READY};
    owner_a.gc.objects = &loaded;
    owner_a.gc.loaded_objects = &loaded;
    ll_mutex_lock_for(&owner_a, &owner_a.linker_mutex);
    ll_error_clear(&owner_a);
    Snapshot loaded_before = snapshot(&owner_a, &host_a);
    int loaded_result = loadObj_(&owner_a, loaded_path);
    ll_mutex_unlock_for(&owner_a, &owner_a.linker_mutex);
    CHECK(loaded_result == 1 && owner_a.error_status == LL_OK,
          "already-loaded existing path did not return success");
    check_snapshot(&owner_a, &host_a, loaded_before,
                   "already-loaded path changed allocation or list state");
    owner_a.gc.objects = NULL;
    owner_a.gc.loaded_objects = NULL;

    duplicate_a = (ObjectCode){.L = &owner_a, .fileName = duplicate_path_a,
                               .status = OBJECT_READY};
    duplicate_b = (ObjectCode){.L = &owner_a, .fileName = duplicate_path_b,
                               .status = OBJECT_READY};
    ll_mutex_lock_for(&owner_a, &owner_a.linker_mutex);
    ll_error_clear(&owner_a);
    ok = ghciInsertSymbolTable(&owner_a, duplicate_path_a, "wp24_ready_duplicate",
                               (ll_symbol_addr *)&value_a, LL_STRENGTH_NORMAL,
                               LL_SYM_DATA, sizeof(value_a), &duplicate_a);
    RtsSymbolInfo *original = lookupStrHashTable(owner_a.symhash,
                                                  "wp24_ready_duplicate");
    Snapshot duplicate_before = snapshot(&owner_a, &host_a);
    int duplicate_result = ghciInsertSymbolTable(
        &owner_a, duplicate_path_b, "wp24_ready_duplicate",
        (ll_symbol_addr *)&value_b, LL_STRENGTH_NORMAL, LL_SYM_DATA,
        sizeof(value_b), &duplicate_b);
    RtsSymbolInfo *after = lookupStrHashTable(owner_a.symhash,
                                               "wp24_ready_duplicate");
    ll_mutex_unlock_for(&owner_a, &owner_a.linker_mutex);
    CHECK(ok != 0 && original != NULL, "initial ready-owner symbol insertion failed");
    CHECK(duplicate_result == 0 && owner_a.error_status == LL_E_DUPSYM,
          "ready-owner duplicate did not return DUPSYM");
    CHECK(after != NULL, "duplicate removed the original symbol entry");
    if (after != NULL) {
        CHECK(after == original && after->owner == &duplicate_a
                  && after->value == (ll_symbol_addr *)&value_a
                  && after->strength == LL_STRENGTH_NORMAL
                  && after->type == LL_SYM_DATA
                  && after->size == sizeof(value_a),
              "duplicate changed the original symbol entry");
    }
    check_snapshot(&owner_a, &host_a, duplicate_before,
                   "duplicate failure changed allocator or object-list state");

    ll_mutex_lock_for(&owner_a, &owner_a.linker_mutex);
    ll_error_clear(&owner_a);
    ghciRemoveSymbolTable(&owner_a, "wp24_ready_duplicate", &duplicate_a);
    ll_mutex_unlock_for(&owner_a, &owner_a.linker_mutex);
    CHECK(lookupStrHashTable(owner_a.symhash, "wp24_ready_duplicate") == NULL,
          "duplicate fixture cleanup did not remove its symbol");

    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && strcmp(ll_default_linker.error_message,
                        "default-owner-sentinel") == 0,
          "explicit-owner operations modified default error sentinel");
    CHECK(host_default.alloc_calls == 0 && host_default.log_calls == 0
              && host_default.fatal_calls == 0,
          "explicit-owner operations used default host services");

    ll_exit_linker(&owner_a);
    ll_exit_linker(&owner_b);
    CHECK(host_a.exit_calls == 1 && host_b.exit_calls == 1,
          "product exit hooks did not run once per owner");
    CHECK(host_a.live == 0 && host_b.live == 0,
          "product exit left owner allocations live");
    CHECK(host_a.alloc_calls == host_a.free_calls
              && host_b.alloc_calls == host_b.free_calls,
          "product exit did not balance owner allocation ledgers");
    CHECK(host_a.fatal_calls == 0 && host_b.fatal_calls == 0,
          "unexpected product fatal callback");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && strcmp(ll_default_linker.error_message,
                        "default-owner-sentinel") == 0,
          "product exit modified default error sentinel");

    cleanup_inputs(empty_path, loaded_path, large_path, dir);
    printf("core_error_status: PASS failures=%u owners=2 default-sentinel=preserved\n",
           failures);
    return failures == 0 ? 0 : 1;
}
