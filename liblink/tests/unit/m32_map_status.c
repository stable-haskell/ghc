#define _DARWIN_C_SOURCE 1
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/MMap.h"

#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

enum { MAX_MAPS = 64, MAX_HOST_ALLOCS = 64 };

typedef enum {
    MAP_REAL,
    MAP_FAIL,
    MAP_OUT_OF_RANGE_BATCH,
    MAP_OUT_OF_RANGE_LARGE
} MapMode;

typedef struct {
    void *base;
    size_t pages;
    bool returned_to_allocator;
    bool released[64];
} MapRecord;

typedef struct {
    void *allocations[MAX_HOST_ALLOCS];
    size_t allocation_count;
    MapRecord maps[MAX_MAPS];
    size_t map_count;
    size_t map_requests;
    size_t raw_map_calls;
    size_t unmap_calls;
    size_t allocator_unmap_calls;
    size_t whole_allocator_unmaps;
    size_t log_calls;
    size_t fatal_calls;
    size_t memory_failure_calls;
    MapMode mode;
} Host;

static ll_linker owner;
static Host host;
static unsigned failures;
static size_t page_bytes;
static char default_error_sentinel[] = "m32-default-error-sentinel";
#if defined(aarch64_HOST_ARCH)
static char near_anchor;
#endif

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "m32_map_status:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void *fixture_mmap(ll_linker *L, size_t bytes);
static void fixture_munmap(ll_linker *L, void *address, size_t bytes,
                           const char *caller);

#define ll_mmap_anon_for_linker fixture_mmap
#define ll_munmap_for_linker fixture_munmap
#include "linker/M32Alloc.c"
#undef ll_mmap_anon_for_linker
#undef ll_munmap_for_linker

static void remember_allocation(void *pointer)
{
    CHECK(pointer != NULL, "host bookkeeping allocation returned NULL");
    CHECK(host.allocation_count < MAX_HOST_ALLOCS,
          "host allocation ledger is full");
    if (pointer != NULL && host.allocation_count < MAX_HOST_ALLOCS)
        host.allocations[host.allocation_count++] = pointer;
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    (void)ctx;
    (void)what;
    void *pointer = malloc(size == 0 ? 1 : size);
    remember_allocation(pointer);
    if (pointer == NULL) exit(98);
    return pointer;
}

static void *host_calloc(void *ctx, size_t count, size_t size,
                         const char *what)
{
    (void)ctx;
    (void)what;
    void *pointer = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    remember_allocation(pointer);
    if (pointer == NULL) exit(98);
    return pointer;
}

static void *host_realloc(void *ctx, void *pointer, size_t size,
                          const char *what)
{
    (void)ctx;
    (void)what;
    if (pointer == NULL) return host_malloc(ctx, size, what);
    for (size_t i = 0; i < host.allocation_count; ++i) {
        if (host.allocations[i] == pointer) {
            void *resized = realloc(pointer, size == 0 ? 1 : size);
            if (resized != NULL) host.allocations[i] = resized;
            if (resized == NULL) exit(98);
            return resized;
        }
    }
    CHECK(false, "realloc pointer is outside the owner ledger");
    exit(98);
}

static void host_free(void *ctx, void *pointer)
{
    (void)ctx;
    if (pointer == NULL) return;
    for (size_t i = 0; i < host.allocation_count; ++i) {
        if (host.allocations[i] == pointer) {
            host.allocations[i] = host.allocations[--host.allocation_count];
            free(pointer);
            return;
        }
    }
    CHECK(false, "free pointer is outside the owner ledger");
}

static void host_log(void *ctx, int kind, const char *format, va_list args)
{
    char message[512];
    (void)ctx;
    (void)kind;
    int written = vsnprintf(message, sizeof(message), format, args);
    CHECK(written >= 0 && (size_t)written < sizeof(message),
          "owner log formatting failed or truncated");
    ++host.log_calls;
}

static void host_fatal(void *ctx, const char *format, va_list args)
{
    char message[512];
    (void)ctx;
    (void)vsnprintf(message, sizeof(message), format, args);
    ++host.fatal_calls;
    (void)fprintf(stderr, "unexpected fatal callback: %s\n", message);
    exit(97);
}

static void host_mem_failure(void *ctx)
{
    (void)ctx;
    ++host.memory_failure_calls;
}

static size_t host_page_size(void *ctx)
{
    (void)ctx;
    return page_bytes;
}

static uintptr_t distance_between(uintptr_t a, uintptr_t b)
{
    return a >= b ? a - b : b - a;
}

static bool is_far_enough(uintptr_t address, uintptr_t base)
{
    return distance_between(address, base) > UINT32_C(0x7fffffff);
}

static bool record_mapping(void *base, size_t bytes, bool returned_to_allocator)
{
    if (host.map_count == MAX_MAPS) {
        CHECK(false, "mapping ledger is full");
        return false;
    }
    size_t pages = (bytes + page_bytes - 1) / page_bytes;
    if (pages == 0 || pages > 64) {
        CHECK(false, "mapping extent exceeds fixture ledger capacity");
        return false;
    }
    MapRecord *record = &host.maps[host.map_count++];
    memset(record, 0, sizeof(*record));
    record->base = base;
    record->pages = pages;
    record->returned_to_allocator = returned_to_allocator;
    return true;
}

static void unmap_recorded(void *address, size_t bytes, bool allocator_call)
{
    uintptr_t start = (uintptr_t)address;
    size_t pages = (bytes + page_bytes - 1) / page_bytes;
    size_t offset_page = 0;
    MapRecord *found = NULL;
    for (size_t i = 0; i < host.map_count; ++i) {
        uintptr_t base = (uintptr_t)host.maps[i].base;
        uintptr_t extent = host.maps[i].pages * page_bytes;
        if (start >= base && start - base <= extent) {
            size_t offset = (size_t)(start - base);
            if ((offset % page_bytes) == 0 && pages <= host.maps[i].pages - offset / page_bytes) {
                found = &host.maps[i];
                offset_page = offset / page_bytes;
                break;
            }
        }
    }
    CHECK(found != NULL, "munmap range does not belong to a recorded mapping");
    if (found != NULL) {
        for (size_t i = 0; i < pages; ++i)
            CHECK(!found->released[offset_page + i], "mapping page released twice");
    }
    size_t rounded = pages * page_bytes;
    CHECK(munmap(address, rounded) == 0, "OS munmap failed");
    ++host.unmap_calls;
    if (allocator_call) ++host.allocator_unmap_calls;
    if (found != NULL) {
        for (size_t i = 0; i < pages; ++i)
            found->released[offset_page + i] = true;
        if (allocator_call && start == (uintptr_t)found->base && pages == found->pages)
            ++host.whole_allocator_unmaps;
    }
}

static void *try_real_mapping(size_t bytes, uintptr_t hint_value)
{
    void *hint = hint_value == 0 ? NULL : (void *)hint_value;
    void *mapping = mmap(hint, bytes, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANON, -1, 0);
    ++host.raw_map_calls;
    if (mapping == MAP_FAILED) return NULL;
    return mapping;
}

static void *fixture_mmap(ll_linker *L, size_t bytes)
{
    CHECK(L == &owner, "M32 map used a different linker owner");
    ++host.map_requests;
    if (host.mode == MAP_FAIL) return NULL;

    const bool batch = host.mode == MAP_OUT_OF_RANGE_BATCH;
    const bool require_far = batch || host.mode == MAP_OUT_OF_RANGE_LARGE;
    uintptr_t base = (uintptr_t)LINKER_LOAD_BASE(L);
    if (!require_far) {
        void *mapping = try_real_mapping(bytes, 0);
        if (mapping != NULL && record_mapping(mapping, bytes, true)) return mapping;
        if (mapping != NULL) (void)munmap(mapping, (bytes + page_bytes - 1) & ~(page_bytes - 1));
        return NULL;
    }

    const uintptr_t offsets[] = {
        UINT64_C(0x100000000), UINT64_C(0x400000000),
        UINT64_C(0x1000000000), UINT64_C(0x2000000000)
    };
    for (size_t i = 0; i < sizeof(offsets) / sizeof(offsets[0]); ++i) {
        for (unsigned direction = 0; direction < 2; ++direction) {
            uintptr_t hint;
            if (direction == 0) {
                if (base > UINTPTR_MAX - offsets[i]) continue;
                hint = base + offsets[i];
            } else {
                if (base < offsets[i] + page_bytes) continue;
                hint = base - offsets[i];
            }
            hint &= ~(uintptr_t)(page_bytes - 1);
            void *mapping = try_real_mapping(bytes, hint);
            if (mapping == NULL) continue;
            uintptr_t probe = (uintptr_t)mapping;
            if (batch) {
                if (probe > UINTPTR_MAX - bytes) {
                    unmap_recorded(mapping, bytes, false);
                    continue;
                }
                probe += bytes;
            }
            if (is_far_enough(probe, base)) {
                if (record_mapping(mapping, bytes, true)) return mapping;
                unmap_recorded(mapping, bytes, false);
                return NULL;
            }
            CHECK(record_mapping(mapping, bytes, false), "could not record hint mapping");
            unmap_recorded(mapping, bytes, false);
        }
    }

    void *mapping = try_real_mapping(bytes, 0);
    if (mapping == NULL) return NULL;
    uintptr_t probe = (uintptr_t)mapping;
    if (batch && probe <= UINTPTR_MAX - bytes) probe += bytes;
    if (!is_far_enough(probe, base)) {
        CHECK(record_mapping(mapping, bytes, false), "could not record fallback mapping");
        unmap_recorded(mapping, bytes, false);
        return NULL;
    }
    if (record_mapping(mapping, bytes, true)) return mapping;
    unmap_recorded(mapping, bytes, false);
    return NULL;
}

static void fixture_munmap(ll_linker *L, void *address, size_t bytes,
                           const char *caller)
{
    CHECK(L == &owner, "M32 munmap used a different linker owner");
    (void)caller;
    unmap_recorded(address, bytes, true);
}

static void drain_free_page_pool(void)
{
#if defined(NEED_M32)
    while (owner.m32_free_page_pool != NULL) {
        struct m32_page_t *page = owner.m32_free_page_pool;
        owner.m32_free_page_pool = page->free_page.next;
        CHECK(owner.m32_free_page_pool_size > 0,
              "free-page pool count underflow during fixture drain");
        if (owner.m32_free_page_pool_size > 0)
            --owner.m32_free_page_pool_size;
        unmap_recorded(page, page_bytes, false);
    }
    CHECK(owner.m32_free_page_pool_size == 0,
          "free-page pool count remains after fixture drain");
#endif
}

static void check_all_mappings_released(void)
{
    for (size_t i = 0; i < host.map_count; ++i) {
        for (size_t j = 0; j < host.maps[i].pages; ++j)
            CHECK(host.maps[i].released[j], "real mapping page leaked");
    }
}

static void check_rejected_mapping_distance(bool batch)
{
    size_t found = 0;
    uintptr_t base = (uintptr_t)LINKER_LOAD_BASE(&owner);
    for (size_t i = 0; i < host.map_count; ++i) {
        MapRecord *record = &host.maps[i];
        if (!record->returned_to_allocator) continue;
        ++found;
        uintptr_t address = (uintptr_t)record->base;
        if (batch) address += record->pages * page_bytes;
        CHECK(is_far_enough(address, base),
              "range-rejected test mapping was not actually outside the M32 range");
    }
    CHECK(found == 1, "range-rejected case did not return exactly one real mapping");
}

static void begin_case(MapMode mode, bool always_pic)
{
    memset(&owner, 0, sizeof(owner));
    memset(&host, 0, sizeof(host));
    host.mode = mode;
    ll_compat_host_ops ops = {0};
    ll_compat_config cfg = {0};
    cfg.always_pic = always_pic;
#if defined(aarch64_HOST_ARCH)
    cfg.near_hint = &near_anchor;
#endif
    ops.ctx = &host;
    ops.malloc_ = host_malloc;
    ops.calloc_ = host_calloc;
    ops.realloc_ = host_realloc;
    ops.free_ = host_free;
    ops.log = host_log;
    ops.fatal = host_fatal;
    ops.on_mem_failure = host_mem_failure;
    ops.page_size = host_page_size;
    ll_install_host_for(&owner, &ops, &cfg);
}

static void end_case(m32_allocator *allocator)
{
    m32_allocator_free_for(&owner, allocator);
    drain_free_page_pool();
    check_all_mappings_released();
    CHECK(host.allocation_count == 0, "owner bookkeeping allocation leaked");
    CHECK(host.fatal_calls == 0, "recoverable map failure called fatal");
}

static void check_owner_error(const char *message)
{
    CHECK(owner.error_status == LL_E_NOMEM, "owner status is not LL_E_NOMEM");
    CHECK(owner.error_message[0] != '\0', "owner diagnostic is empty");
    CHECK(strstr(owner.error_message, message) != NULL,
          "owner diagnostic does not identify the failed allocation");
}

static void test_small_batch_map_failure(void)
{
    begin_case(MAP_FAIL, true);
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    void *result = m32_alloc_for(&owner, allocator, 64, 16);
    CHECK(result == NULL, "small request succeeded after injected NULL map");
    check_owner_error("failed to map pages");
    CHECK(host.map_requests == 1 && host.raw_map_calls == 0,
          "small NULL case did not use exactly one injected map request");
    CHECK(host.unmap_calls == 0, "NULL map should not be unmapped");
    CHECK(host.log_calls == 0, "small NULL map unexpectedly logged an error");
    CHECK(owner.m32_free_page_pool == NULL && owner.m32_free_page_pool_size == 0,
          "small NULL case modified the free-page pool");
    end_case(allocator);
}

static void test_small_batch_range_failure(void)
{
    begin_case(MAP_OUT_OF_RANGE_BATCH, false);
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    void *result = m32_alloc_for(&owner, allocator, 64, 16);
    CHECK(result == NULL, "small request succeeded with an out-of-range batch");
    check_owner_error("within 4GB");
    CHECK(host.map_requests == 1 && host.allocator_unmap_calls == 1,
          "range-rejected batch did not produce one map and one allocator unmap");
    CHECK(host.whole_allocator_unmaps == 1,
          "range-rejected batch was not unmapped at its original full base");
    check_rejected_mapping_distance(true);
    CHECK(host.memory_failure_calls == 1,
          "range-rejected batch did not notify the memory-failure callback");
    CHECK(host.log_calls == 1, "range-rejected batch did not log once");
    CHECK(owner.m32_free_page_pool == NULL && owner.m32_free_page_pool_size == 0,
          "range-rejected batch modified the free-page pool");
    end_case(allocator);
}

static void test_large_map_failure(void)
{
    begin_case(MAP_FAIL, true);
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    void *result = m32_alloc_for(&owner, allocator, page_bytes * 2, 16);
    CHECK(result == NULL, "large request succeeded after injected NULL map");
    check_owner_error("Failed to map pages");
    CHECK(host.map_requests == 1 && host.raw_map_calls == 0,
          "large NULL case did not use exactly one injected map request");
    CHECK(host.unmap_calls == 0, "NULL large map should not be unmapped");
    CHECK(host.log_calls == 1, "large NULL map did not log once");
    end_case(allocator);
}

static void test_large_range_failure(void)
{
    begin_case(MAP_OUT_OF_RANGE_LARGE, false);
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    void *result = m32_alloc_for(&owner, allocator, page_bytes * 2, 16);
    CHECK(result == NULL, "large request succeeded with an out-of-range mapping");
    check_owner_error("above 4GB");
    CHECK(host.map_requests == 1 && host.allocator_unmap_calls == 1,
          "large range rejection did not produce one map and one allocator unmap");
    CHECK(host.whole_allocator_unmaps == 1,
          "large rejection did not release the original complete mapping");
    check_rejected_mapping_distance(false);
    CHECK(host.memory_failure_calls == 1,
          "large range rejection did not notify the memory-failure callback");
    CHECK(host.log_calls == 1, "large range rejection did not log once");
    end_case(allocator);
}

static void test_small_pool_reuse(void)
{
    begin_case(MAP_REAL, true);
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    void *first = m32_alloc_for(&owner, allocator, 64, 16);
    void *second = m32_alloc_for(&owner, allocator, 64, 16);
    CHECK(first != NULL && second != NULL && first != second,
          "small positive allocations did not return distinct storage");
    CHECK(first != NULL && (uintptr_t)first % 16 == 0,
          "first small allocation is not aligned");
    CHECK(second != NULL && (uintptr_t)second % 16 == 0,
          "second small allocation is not aligned");
    CHECK(host.map_requests == 1,
          "small pool control mapped again instead of reusing a page");
    CHECK(owner.error_status == LL_OK, "small positive control set an error");
    end_case(allocator);
    CHECK(host.unmap_calls == 32,
          "small positive pool drain did not release all 32 pages");
}

static void test_large_allocation(void)
{
    begin_case(MAP_REAL, true);
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    const size_t bytes = page_bytes * 2;
    void *result = m32_alloc_for(&owner, allocator, bytes, 16);
    CHECK(result != NULL, "large positive allocation failed");
    CHECK(result != NULL && (uintptr_t)result % 16 == 0,
          "large positive allocation is not aligned");
    CHECK(host.map_requests == 1, "large positive control did not map once");
    CHECK(owner.error_status == LL_OK, "large positive control set an error");
    end_case(allocator);
    CHECK(owner.m32_free_page_pool_size == 0,
          "large positive pool drain left pages behind");
}

int main(void)
{
#if !defined(darwin_HOST_OS) || !defined(NEED_M32) || !defined(OBJFORMAT_MACHO)
    (void)puts("SKIP: M32 mapping status test requires Darwin mmap linker");
    return 0;
#else
    long detected_page_size = sysconf(_SC_PAGESIZE);
    if (detected_page_size <= 0) {
        (void)fprintf(stderr, "m32_map_status: could not query native page size\n");
        return 1;
    }
    page_bytes = (size_t)detected_page_size;
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    (void)memcpy(ll_default_linker.error_message, default_error_sentinel,
                 sizeof(default_error_sentinel));

    test_small_batch_map_failure();
    test_small_batch_range_failure();
    test_large_map_failure();
    test_large_range_failure();
    test_small_pool_reuse();
    test_large_allocation();

    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND &&
              memcmp(ll_default_linker.error_message, default_error_sentinel,
                     sizeof(default_error_sentinel)) == 0,
          "owner M32 failures changed default linker error sentinel");
    CHECK(ll_default_linker.m32_free_page_pool == NULL &&
              ll_default_linker.m32_free_page_pool_size == 0,
          "owner M32 tests changed default linker page pool");
    if (failures != 0) return 1;
    (void)puts("m32_map_status: PASS");
    return 0;
#endif
}
