#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/MMap.h"
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

enum { MAX_OWNED = 512, ARCHIVE_CAPACITY = 4096 };
#if (defined(darwin_HOST_OS) || defined(ios_HOST_OS)) && defined(aarch64_HOST_ARCH)
/* lipo -info verifies arm64 and x86_64 slices; SHA-256 of the 64-byte fixture:
   7ee823d55d1c6442f22d6866aad8465ddd93ab13cbff5e609a0bf5ad44d49304 */
static const unsigned char fat_empty_archives[64] = {
    0xca, 0xfe, 0xba, 0xbe, 0x00, 0x00, 0x00, 0x02,
    0x01, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x08,
    0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x07,
    0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x38,
    0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00,
    '!', '<', 'a', 'r', 'c', 'h', '>', '\n',
    '!', '<', 'a', 'r', 'c', 'h', '>', '\n'
};
#endif
typedef struct {
    const char *name;
    void *owned[MAX_OWNED];
    size_t live;
    unsigned alloc_calls;
    unsigned free_calls;
    unsigned log_calls;
    unsigned fatal_calls;
    unsigned registrations;
} Host;

typedef struct {
    unsigned alloc_calls;
    unsigned free_calls;
    size_t live;
    StrHashTable *symhash;
    ObjectCode *objects;
    ObjectCode *loaded;
    uint64_t unloaded;
    unsigned registrations;
} Snapshot;

static Host host_a = {.name = "A"};
static Host host_b = {.name = "B"};
static Host host_default = {.name = "default"};
static unsigned failures;
static unsigned registration_calls;
#if RTS_LINKER_USE_MMAP
static bool inject_map_failure;
static unsigned map_calls;
static unsigned unmap_calls;
static size_t last_map_bytes;
static size_t last_unmap_bytes;
static ll_linker *last_map_owner;
static ll_linker *last_unmap_owner;

static void *archive_test_mmap(ll_linker *L, size_t bytes)
{
    ++map_calls;
    last_map_owner = L;
    last_map_bytes = bytes;
    if (inject_map_failure) {
        inject_map_failure = false;
        errno = ENOMEM;
        return NULL;
    }
    return ll_mmap_anon_for_linker(L, bytes);
}

static void archive_test_munmap(ll_linker *L, void *addr, size_t bytes,
                                const char *caller)
{
    ++unmap_calls;
    last_unmap_owner = L;
    last_unmap_bytes = bytes;
    ll_munmap_for_linker(L, addr, bytes, caller);
}

#define ll_mmap_anon_for_linker archive_test_mmap
#define ll_munmap_for_linker archive_test_munmap
#include "linker/LoadArchive.c"
#undef ll_munmap_for_linker
#undef ll_mmap_anon_for_linker

#endif

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "archive_error_status:%d: %s\n", __LINE__, message); \
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
    if (ptr == NULL) return host_malloc(ctx, size, what);
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

static void on_mem_failure(void *ctx) { (void)ctx; }
static void object_registered(void *ctx, ObjectCode *oc)
{
    Host *host = ctx;
    (void)oc;
    ++host->registrations;
    ++registration_calls;
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
    ops.on_mem_failure = on_mem_failure;
    ops.object_registered = object_registered;
    return ops;
}

static Snapshot snapshot(ll_linker *L, Host *host)
{
    return (Snapshot){host->alloc_calls, host->free_calls, host->live,
                      L->symhash, L->gc.objects, L->gc.loaded_objects,
                      (uint64_t)L->gc.n_unloaded_objects, host->registrations};
}

static void check_clean_delta(ll_linker *L, Host *host, Snapshot before,
                              const char *label)
{
    Snapshot after = snapshot(L, host);
    CHECK(after.live == before.live, label);
    CHECK(after.alloc_calls - before.alloc_calls
              == after.free_calls - before.free_calls,
          label);
    CHECK(after.symhash == before.symhash && after.objects == before.objects
              && after.loaded == before.loaded
              && after.unloaded == before.unloaded,
          label);
    CHECK(after.registrations == before.registrations, label);
}

static void fill_field(char *dst, size_t width, const char *text)
{
    size_t n = strlen(text);
    if (n > width) n = width;
    memset(dst, ' ', width);
    memcpy(dst, text, n);
}

static void append_header(char *buf, size_t capacity, size_t *used,
                          const char *name, const char *size_text)
{
    static const char zero_time[] = "0";
    char header[60];
    if (*used > capacity || capacity - *used < sizeof(header)) {
        CHECK(false, "archive fixture buffer overflow");
        return;
    }
    fill_field(header, 16, name);
    fill_field(header + 16, 12, zero_time);
    fill_field(header + 28, 6, "0");
    fill_field(header + 34, 6, "0");
    fill_field(header + 40, 8, "100644");
    fill_field(header + 48, 10, size_text);
    header[58] = '`';
    header[59] = '\n';
    memcpy(buf + *used, header, sizeof(header));
    *used += sizeof(header);
}

static size_t make_archive(char *buf, size_t capacity)
{
    static const char magic[] = "!<arch>\n";
    CHECK(capacity >= sizeof(magic) - 1, "archive fixture buffer too small");
    memcpy(buf, magic, sizeof(magic) - 1);
    return sizeof(magic) - 1;
}

static size_t append_member(char *buf, size_t capacity, size_t used,
                            const char *name, const char *size_text,
                            const void *payload, size_t payload_size,
                            bool pad)
{
    append_header(buf, capacity, &used, name, size_text);
    if (payload_size > capacity - used) {
        CHECK(false, "archive fixture payload overflow");
        return used;
    }
    if (payload_size != 0 && payload != NULL)
        memcpy(buf + used, payload, payload_size);
    used += payload_size;
    if (pad && (payload_size & 1u) != 0) {
        if (used == capacity) {
            CHECK(false, "archive fixture pad overflow");
            return used;
        }
        buf[used++] = '\n';
    }
    return used;
}

static void write_fixture(const char *path, const void *bytes, size_t size)
{
    int fd = open(path, O_CREAT | O_EXCL | O_WRONLY, 0600);
    CHECK(fd >= 0, "could not create archive fixture");
    if (fd < 0) return;
    const unsigned char *p = bytes;
    size_t written = 0;
    while (written < size) {
        ssize_t n = write(fd, p + written, size - written);
        if (n <= 0) {
            CHECK(false, "could not write archive fixture");
            break;
        }
        written += (size_t)n;
    }
    CHECK(close(fd) == 0, "could not close archive fixture");
}

static void make_path(char *out, size_t capacity, const char *dir,
                      const char *leaf)
{
    int n = snprintf(out, capacity, "%s/%s", dir, leaf);
    CHECK(n >= 0 && (size_t)n < capacity, "temporary path overflow");
}

static void check_archive(ll_linker *L, Host *host, const char *path,
                          ll_status expected, ll_hs_int expected_result,
                          const char *label)
{
    ll_mutex_lock_for(L, &L->linker_mutex);
    ll_error_clear(L);
    Snapshot before = snapshot(L, host);
    ll_hs_int result = loadArchive_(L, (pathchar *)path);
    ll_mutex_unlock_for(L, &L->linker_mutex);
    CHECK(result == expected_result, label);
    CHECK(L->error_status == expected, label);
    CHECK(L->error_message[0] != '\0' || expected == LL_OK, label);
    check_clean_delta(L, host, before, label);
}

static void check_null_path(ll_linker *L, Host *host)
{
    ll_mutex_lock_for(L, &L->linker_mutex);
    ll_error_clear(L);
    Snapshot before = snapshot(L, host);
    ll_hs_int result = loadArchive_(L, NULL);
    ll_mutex_unlock_for(L, &L->linker_mutex);
    CHECK(result == 0 && L->error_status == LL_E_STATE,
          "NULL archive path did not return STATE");
    check_clean_delta(L, host, before, "NULL archive path changed owner resources");
}

static void cleanup(const char **paths, size_t count, const char *dir)
{
    for (size_t i = 0; i < count; ++i) {
        if (paths[i] != NULL) CHECK(unlink(paths[i]) == 0, "fixture unlink failed");
    }
    CHECK(rmdir(dir) == 0, "fixture directory removal failed");
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
    char empty_path[256], nonobject_path[256], missing_path[256];
    char truncated_header_path[256], truncated_payload_path[256];
    char malformed_size_path[256], large_size_path[256];
    char bsd_name_path[256], bsd_valid_path[256], gnu_missing_path[256];
    char gnu_range_path[256], gnu_unterminated_path[256], gnu_valid_path[256];
    char gnu_truncated_path[256], gnu_map_failure_path[256], thin_missing_path[256];
#if (defined(darwin_HOST_OS) || defined(ios_HOST_OS)) && defined(aarch64_HOST_ARCH)
    char fat_empty_path[256];
#endif
#if RTS_LINKER_USE_MMAP && (defined(darwin_HOST_OS) || defined(ios_HOST_OS))
    char map_failure_path[256];
#endif
    const char *paths[20];
    char bytes[ARCHIVE_CAPACITY];
    size_t used;
    size_t path_count;
    bool thin_only = argc == 2 && strcmp(argv[1], "--thin-missing-only") == 0;
    int n;

    ll_install_host_for(&ll_default_linker, &ops_default, NULL);
    ll_install_host_for(&owner_a, &ops_a, NULL);
    ll_install_host_for(&owner_b, &ops_b, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_message, sizeof(default_message));
    ll_init_linker(&owner_a, 0);
    ll_init_linker(&owner_b, 0);
    CHECK(owner_a.linker_init_done == 1 && owner_a.symhash != NULL,
          "owner A product initializer did not initialize state");
    CHECK(owner_b.linker_init_done == 1 && owner_b.symhash != NULL,
          "owner B product initializer did not initialize state");
    owner_a.services.cfg.always_pic = true;
    owner_b.services.cfg.always_pic = true;

    n = snprintf(dir_template, sizeof(dir_template),
                 "/tmp/liblink-archive-status-%ld-XXXXXX", (long)getpid());
    CHECK(n >= 0 && (size_t)n < sizeof(dir_template), "temp directory path overflow");
    char *dir = mkdtemp(dir_template);
    CHECK(dir != NULL, "mkdtemp failed");
    if (dir == NULL) return 2;
    make_path(empty_path, sizeof(empty_path), dir, "empty.a");
    make_path(nonobject_path, sizeof(nonobject_path), dir, "nonobject.a");
    make_path(missing_path, sizeof(missing_path), dir, "missing.a");
    make_path(truncated_header_path, sizeof(truncated_header_path), dir, "header.a");
    make_path(truncated_payload_path, sizeof(truncated_payload_path), dir, "payload.a");
    make_path(malformed_size_path, sizeof(malformed_size_path), dir, "size.a");
    make_path(large_size_path, sizeof(large_size_path), dir, "large-size.a");
    make_path(bsd_name_path, sizeof(bsd_name_path), dir, "bsd-name.a");
    make_path(bsd_valid_path, sizeof(bsd_valid_path), dir, "bsd-valid.a");
    make_path(gnu_missing_path, sizeof(gnu_missing_path), dir, "gnu-missing.a");
    make_path(gnu_range_path, sizeof(gnu_range_path), dir, "gnu-range.a");
    make_path(gnu_unterminated_path, sizeof(gnu_unterminated_path), dir, "gnu-unterminated.a");
    make_path(gnu_valid_path, sizeof(gnu_valid_path), dir, "gnu-valid.a");
    make_path(gnu_truncated_path, sizeof(gnu_truncated_path), dir, "gnu-truncated.a");
    make_path(gnu_map_failure_path, sizeof(gnu_map_failure_path), dir, "gnu-map-failure.a");
    make_path(thin_missing_path, sizeof(thin_missing_path), dir, "missing-thin.a");
#if (defined(darwin_HOST_OS) || defined(ios_HOST_OS)) && defined(aarch64_HOST_ARCH)
    make_path(fat_empty_path, sizeof(fat_empty_path), dir, "fat-empty.a");
#endif
#if RTS_LINKER_USE_MMAP && (defined(darwin_HOST_OS) || defined(ios_HOST_OS))
    make_path(map_failure_path, sizeof(map_failure_path), dir, "map-failure.a");
#endif
    if (argc > 2 || (argc == 2 && !thin_only)) {
        (void)fprintf(stderr, "usage: archive_error_status [--thin-missing-only]\n");
        return 2;
    }
    path_count = 0;
    if (!thin_only) {
        paths[path_count++] = empty_path;
        paths[path_count++] = nonobject_path;
        paths[path_count++] = truncated_header_path;
        paths[path_count++] = truncated_payload_path;
        paths[path_count++] = malformed_size_path;
        paths[path_count++] = large_size_path;
        paths[path_count++] = bsd_name_path;
        paths[path_count++] = bsd_valid_path;
        paths[path_count++] = gnu_missing_path;
        paths[path_count++] = gnu_range_path;
        paths[path_count++] = gnu_unterminated_path;
        paths[path_count++] = gnu_valid_path;
        paths[path_count++] = gnu_truncated_path;
#if (defined(darwin_HOST_OS) || defined(ios_HOST_OS)) && defined(aarch64_HOST_ARCH)
        paths[path_count++] = fat_empty_path;
#endif
#if RTS_LINKER_USE_MMAP
        paths[path_count++] = gnu_map_failure_path;
#endif
#if RTS_LINKER_USE_MMAP && (defined(darwin_HOST_OS) || defined(ios_HOST_OS))
        paths[path_count++] = map_failure_path;
#endif
    }
    paths[path_count++] = thin_missing_path;

    if (!thin_only) {
        used = make_archive(bytes, sizeof(bytes));
        write_fixture(empty_path, bytes, used);
        check_archive(&owner_a, &host_a, empty_path, LL_OK, 1,
                      "valid empty archive did not succeed cleanly");
        CHECK(isArchive((pathchar *)empty_path),
              "empty standard archive was not recognized");

#if (defined(darwin_HOST_OS) || defined(ios_HOST_OS)) && defined(aarch64_HOST_ARCH)
        write_fixture(fat_empty_path, (const char *)fat_empty_archives,
                      sizeof(fat_empty_archives));
        check_archive(&owner_b, &host_b, fat_empty_path, LL_OK, 1,
                      "valid fat archive with empty slices did not succeed cleanly");
#endif

        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "payload/", "5",
                             "hello", 5, true);
        write_fixture(nonobject_path, bytes, used);
        check_archive(&owner_b, &host_b, nonobject_path, LL_OK, 1,
                      "valid non-object archive member was not skipped");

        check_archive(&owner_a, &host_a, missing_path, LL_E_IO, 0,
                      "missing archive path did not return IO");
        check_null_path(&owner_b, &host_b);

        used = make_archive(bytes, sizeof(bytes));
        memcpy(bytes + used, "short", 5); used += 5;
        write_fixture(truncated_header_path, bytes, used);
        check_archive(&owner_a, &host_a, truncated_header_path, LL_E_FORMAT, 0,
                      "truncated archive member header did not return FORMAT");

        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "payload/", "8",
                             "xy", 2, false);
        write_fixture(truncated_payload_path, bytes, used);
        check_archive(&owner_b, &host_b, truncated_payload_path, LL_E_FORMAT, 0,
                      "declared payload beyond EOF did not return FORMAT");

        used = make_archive(bytes, sizeof(bytes));
        append_header(bytes, sizeof(bytes), &used, "payload/", "not-a-size");
        write_fixture(malformed_size_path, bytes, used);
        check_archive(&owner_a, &host_a, malformed_size_path, LL_E_FORMAT, 0,
                      "malformed decimal member size did not return FORMAT");

        used = make_archive(bytes, sizeof(bytes));
        append_header(bytes, sizeof(bytes), &used, "large/", "2147483648");
        write_fixture(large_size_path, bytes, used);
        check_archive(&owner_b, &host_b, large_size_path, LL_E_UNSUPPORTED, 0,
                      "member size beyond INT_MAX did not return UNSUPPORTED");

        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "#1/9", "3", NULL, 0, false);
        write_fixture(bsd_name_path, bytes, used);
        check_archive(&owner_b, &host_b, bsd_name_path, LL_E_FORMAT, 0,
                      "BSD extended name longer than payload did not return FORMAT");

        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "#1/3", "5",
                             "x.oab", 5, true);
        used = append_member(bytes, sizeof(bytes), used, "tail/", "0", NULL, 0, false);
        write_fixture(bsd_valid_path, bytes, used);
        check_archive(&owner_a, &host_a, bsd_valid_path, LL_OK, 1,
                      "valid BSD extended name with odd stored size did not parse");

        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "/0", "0", NULL, 0, false);
        write_fixture(gnu_missing_path, bytes, used);
        check_archive(&owner_a, &host_a, gnu_missing_path, LL_E_FORMAT, 0,
                      "GNU filename reference without table did not return FORMAT");

        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "//", "6", "name/\n", 6, false);
        used = append_member(bytes, sizeof(bytes), used, "/99", "0", NULL, 0, false);
        write_fixture(gnu_range_path, bytes, used);
        check_archive(&owner_b, &host_b, gnu_range_path, LL_E_FORMAT, 0,
                      "GNU filename offset outside table did not return FORMAT");

        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "//", "6", "name//", 6, false);
        used = append_member(bytes, sizeof(bytes), used, "/0", "0", NULL, 0, false);
        write_fixture(gnu_unterminated_path, bytes, used);
        check_archive(&owner_a, &host_a, gnu_unterminated_path, LL_E_FORMAT, 0,
                      "unterminated GNU filename table did not return FORMAT");

#if RTS_LINKER_USE_MMAP
        unsigned map_before = map_calls;
        unsigned unmap_before = unmap_calls;
#endif
        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "//", "6", "name/\n", 6, false);
        used = append_member(bytes, sizeof(bytes), used, "/0", "5", "hello", 5, true);
        write_fixture(gnu_valid_path, bytes, used);
        check_archive(&owner_b, &host_b, gnu_valid_path, LL_OK, 1,
                      "valid GNU name table and referenced payload did not parse");
#if RTS_LINKER_USE_MMAP
        CHECK(map_calls == map_before + 1 && unmap_calls == unmap_before + 1
                  && last_map_owner == &owner_b && last_unmap_owner == &owner_b
                  && last_map_bytes == last_unmap_bytes,
              "successful GNU name table mapping was not released by its owner");
#endif

#if RTS_LINKER_USE_MMAP
        map_before = map_calls;
        unmap_before = unmap_calls;
#endif
        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "//", "8", "xy", 2, false);
        write_fixture(gnu_truncated_path, bytes, used);
        check_archive(&owner_a, &host_a, gnu_truncated_path, LL_E_FORMAT, 0,
                      "truncated GNU name table body did not return FORMAT");
#if RTS_LINKER_USE_MMAP
        CHECK(map_calls == map_before + 1 && unmap_calls == unmap_before + 1
                  && last_map_owner == &owner_a && last_unmap_owner == &owner_a
                  && last_map_bytes == last_unmap_bytes,
              "truncated GNU name table mapping was not fully released");

        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "//", "6", "name/\n", 6, false);
        write_fixture(gnu_map_failure_path, bytes, used);
        map_before = map_calls;
        unmap_before = unmap_calls;
        inject_map_failure = true;
        check_archive(&owner_b, &host_b, gnu_map_failure_path, LL_E_NOMEM, 0,
                      "GNU index mapping failure did not return NOMEM");
        CHECK(!inject_map_failure && map_calls == map_before + 1
                  && unmap_calls == unmap_before,
              "failed GNU index mapping was mishandled");
#endif
    }

    memcpy(bytes, "!<thin>\n", 8);
    used = 8;
    used = append_member(bytes, sizeof(bytes), used, "missing.o/", "4", NULL, 0, false);
    write_fixture(thin_missing_path, bytes, used);
    check_archive(&owner_b, &host_b, thin_missing_path, LL_E_IO, 0,
                  "missing thin member did not return IO cleanly");
    if (!thin_only) {
        CHECK(isArchive((pathchar *)thin_missing_path),
              "thin archive magic was not recognized");
    }

#if RTS_LINKER_USE_MMAP && (defined(darwin_HOST_OS) || defined(ios_HOST_OS))
    if (!thin_only) {
        static const unsigned char macho_magic[] = {0xcf, 0xfa, 0xed, 0xfe,
                                                     0, 0, 0, 0, 0, 0, 0, 0};
        used = make_archive(bytes, sizeof(bytes));
        used = append_member(bytes, sizeof(bytes), used, "input.o/", "12",
                             macho_magic, sizeof(macho_magic), false);
        write_fixture(map_failure_path, bytes, used);
        unsigned map_before = map_calls;
        inject_map_failure = true;
        check_archive(&owner_a, &host_a, map_failure_path, LL_E_NOMEM, 0,
                      "failed pending-image map did not return NOMEM cleanly");
        CHECK(!inject_map_failure && map_calls == map_before + 1,
              "pending-image map failure was not consumed exactly once");
    }
#endif

    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && strcmp(ll_default_linker.error_message, default_message) == 0,
          "explicit archive operations modified default error sentinel");
    CHECK(host_default.alloc_calls == 0 && host_default.free_calls == 0
              && host_default.log_calls == 0 && host_default.fatal_calls == 0
              && host_default.registrations == 0,
          "explicit archive operations used default host services");
    CHECK(host_a.fatal_calls == 0 && host_b.fatal_calls == 0,
          "archive input failures called a host fatal callback");
    CHECK(registration_calls == 0 && host_a.registrations == 0
              && host_b.registrations == 0,
          "early archive failures registered an object");

    ll_exit_linker(&owner_a);
    ll_exit_linker(&owner_b);
    CHECK(host_a.live == 0 && host_b.live == 0,
          "product exit left owner allocations live");
    CHECK(host_a.alloc_calls == host_a.free_calls
              && host_b.alloc_calls == host_b.free_calls,
          "product exit did not balance owner allocation ledgers");
    CHECK(host_a.fatal_calls == 0 && host_b.fatal_calls == 0,
          "unexpected fatal callback during linker exit");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && strcmp(ll_default_linker.error_message, default_message) == 0,
          "product exit modified default error sentinel");

    cleanup(paths, path_count, dir);
    printf("archive_error_status: %s failures=%u owners=2 default-sentinel=preserved\n",
           failures == 0 ? "PASS" : "FAIL", failures);
    return failures == 0 ? 0 : 1;
}
