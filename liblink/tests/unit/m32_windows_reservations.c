#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/M32Reservations.h"
#include "linker/MMap.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <windows.h>

enum { MAX_HOST_ALLOCS = 128, MAX_WIN_CALLS = 2048, MAX_BASES = 32 };

typedef struct {
    LPVOID address;
    SIZE_T bytes;
    DWORD kind;
    BOOL result;
} FreeCall;

typedef struct {
    LPVOID address;
    SIZE_T bytes;
    DWORD protection;
    BOOL result;
} ProtectCall;

typedef struct {
    void *allocations[MAX_HOST_ALLOCS];
    size_t allocation_count;
    FreeCall free_calls[MAX_WIN_CALLS];
    size_t free_call_count;
    ProtectCall protect_calls[MAX_WIN_CALLS];
    size_t protect_call_count;
    void *bases[MAX_BASES];
    size_t base_count;
    size_t page_size;
    unsigned log_calls;
    unsigned fatal_calls;
    unsigned memory_failure_calls;
    unsigned fail_decommit;
    unsigned fail_release;
    unsigned fail_rw_protect;
    unsigned query_calls;
    unsigned alloc_calls;
    unsigned fail_query;
    unsigned fail_alloc;
    bool clobber_host_free_errors;
    DWORD injected_query_error;
    DWORD injected_alloc_error;
    DWORD host_free_last_error;
    int injected_query_errno;
    int injected_alloc_errno;
    int host_free_errno;
    void *last_mapping;
} Fixture;

static ll_linker owner;
static Fixture fixture;
static unsigned failures;
static char default_error_sentinel[] = "m32-windows-default-sentinel";

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "m32_windows_reservations:%d: %s\n", \
                      __LINE__, message); \
        ++failures; \
    } \
} while (0)

static BOOL WINAPI fixture_virtual_free(LPVOID address, SIZE_T bytes,
                                        DWORD kind);
static BOOL WINAPI fixture_virtual_protect(LPVOID address, SIZE_T bytes,
                                           DWORD protection,
                                           PDWORD old_protection);
static SIZE_T WINAPI fixture_virtual_query(LPCVOID address,
                                           PMEMORY_BASIC_INFORMATION info,
                                           SIZE_T length);
static LPVOID WINAPI fixture_virtual_alloc(LPVOID address, SIZE_T bytes,
                                           DWORD allocation_type,
                                           DWORD protection);

#define VirtualQuery fixture_virtual_query
#define VirtualFree fixture_virtual_free
#include "linker/M32Reservations.c"
#undef VirtualQuery
#undef VirtualFree

#define VirtualProtect fixture_virtual_protect
#define VirtualAlloc fixture_virtual_alloc
#include "linker/MMap.c"
#undef VirtualProtect
#undef VirtualAlloc

#include "linker/M32Alloc.c"

static void remember_allocation(void *pointer)
{
    CHECK(pointer != NULL, "host bookkeeping allocation returned NULL");
    CHECK(fixture.allocation_count < MAX_HOST_ALLOCS,
          "host allocation ledger is full");
    if (pointer != NULL && fixture.allocation_count < MAX_HOST_ALLOCS)
        fixture.allocations[fixture.allocation_count++] = pointer;
}

static void *host_malloc(void *ctx, size_t bytes, const char *what)
{
    (void)ctx;
    (void)what;
    void *pointer = malloc(bytes == 0 ? 1 : bytes);
    remember_allocation(pointer);
    if (pointer == NULL) ExitProcess(98);
    return pointer;
}

static void *host_calloc(void *ctx, size_t count, size_t bytes,
                         const char *what)
{
    (void)ctx;
    (void)what;
    void *pointer = calloc(count == 0 ? 1 : count, bytes == 0 ? 1 : bytes);
    remember_allocation(pointer);
    if (pointer == NULL) ExitProcess(98);
    return pointer;
}

static void *host_realloc(void *ctx, void *pointer, size_t bytes,
                          const char *what)
{
    (void)ctx;
    (void)what;
    if (pointer == NULL) return host_malloc(ctx, bytes, what);
    for (size_t i = 0; i < fixture.allocation_count; ++i) {
        if (fixture.allocations[i] == pointer) {
            void *resized = realloc(pointer, bytes == 0 ? 1 : bytes);
            if (resized != NULL) fixture.allocations[i] = resized;
            if (resized == NULL) ExitProcess(98);
            return resized;
        }
    }
    CHECK(false, "realloc pointer is outside the owner allocation ledger");
    ExitProcess(98);
    return NULL;
}

static void host_free(void *ctx, void *pointer)
{
    (void)ctx;
    if (pointer == NULL) return;
    for (size_t i = 0; i < fixture.allocation_count; ++i) {
        if (fixture.allocations[i] == pointer) {
            fixture.allocations[i] = fixture.allocations[--fixture.allocation_count];
            free(pointer);
            if (fixture.clobber_host_free_errors) {
                SetLastError(fixture.host_free_last_error);
                errno = fixture.host_free_errno;
            }
            return;
        }
    }
    CHECK(false, "free pointer is outside the owner allocation ledger");
}

static void host_log(void *ctx, int kind, const char *format, va_list args)
{
    char message[512];
    (void)ctx;
    (void)kind;
    int written = vsnprintf(message, sizeof(message), format, args);
    CHECK(written >= 0 && (size_t)written < sizeof(message),
          "host log formatting failed or truncated");
    ++fixture.log_calls;
}

static void host_fatal(void *ctx, const char *format, va_list args)
{
    char message[512];
    (void)ctx;
    (void)vsnprintf(message, sizeof(message), format, args);
    ++fixture.fatal_calls;
    (void)fprintf(stderr, "unexpected fatal callback: %s\n", message);
    ExitProcess(97);
}

static void host_memory_failure(void *ctx)
{
    (void)ctx;
    ++fixture.memory_failure_calls;
}

static size_t host_page_size(void *ctx)
{
    (void)ctx;
    return fixture.page_size;
}

static BOOL WINAPI fixture_virtual_free(LPVOID address, SIZE_T bytes,
                                        DWORD kind)
{
    BOOL result;
    size_t slot = fixture.free_call_count;
    if (slot < MAX_WIN_CALLS) {
        FreeCall *call = &fixture.free_calls[slot];
        ++fixture.free_call_count;
        call->address = address;
        call->bytes = bytes;
        call->kind = kind;
    } else {
        CHECK(false, "VirtualFree call ledger is full");
    }
    if (kind == MEM_DECOMMIT && fixture.fail_decommit != 0) {
        --fixture.fail_decommit;
        SetLastError(ERROR_ACCESS_DENIED);
        result = FALSE;
    } else if (kind == MEM_RELEASE && fixture.fail_release != 0) {
        --fixture.fail_release;
        SetLastError(ERROR_ACCESS_DENIED);
        result = FALSE;
    } else {
        result = VirtualFree(address, bytes, kind);
    }
    if (slot < MAX_WIN_CALLS) fixture.free_calls[slot].result = result;
    return result;
}

static SIZE_T WINAPI fixture_virtual_query(
    LPCVOID address, PMEMORY_BASIC_INFORMATION info, SIZE_T length)
{
    ++fixture.query_calls;
    if (fixture.fail_query != 0) {
        --fixture.fail_query;
        SetLastError(fixture.injected_query_error);
        errno = fixture.injected_query_errno;
        return 0;
    }
    return VirtualQuery(address, info, length);
}

static LPVOID WINAPI fixture_virtual_alloc(LPVOID address, SIZE_T bytes,
                                           DWORD allocation_type,
                                           DWORD protection)
{
    ++fixture.alloc_calls;
    if (fixture.fail_alloc != 0) {
        --fixture.fail_alloc;
        SetLastError(fixture.injected_alloc_error);
        errno = fixture.injected_alloc_errno;
        return NULL;
    }
    LPVOID result = VirtualAlloc(address, bytes, allocation_type, protection);
    if (result != NULL) fixture.last_mapping = result;
    return result;
}

static BOOL WINAPI fixture_virtual_protect(LPVOID address, SIZE_T bytes,
                                           DWORD protection,
                                           PDWORD old_protection)
{
    BOOL result;
    size_t slot = fixture.protect_call_count;
    if (slot < MAX_WIN_CALLS) {
        ProtectCall *call = &fixture.protect_calls[slot];
        ++fixture.protect_call_count;
        call->address = address;
        call->bytes = bytes;
        call->protection = protection;
    } else {
        CHECK(false, "VirtualProtect call ledger is full");
    }
    if (protection == PAGE_READWRITE && fixture.fail_rw_protect != 0) {
        --fixture.fail_rw_protect;
        SetLastError(ERROR_ACCESS_DENIED);
        result = FALSE;
    } else {
        result = VirtualProtect(address, bytes, protection, old_protection);
    }
    if (slot < MAX_WIN_CALLS) fixture.protect_calls[slot].result = result;
    return result;
}

static void begin_case(void)
{
    SYSTEM_INFO system_info;
    memset(&owner, 0, sizeof(owner));
    memset(&fixture, 0, sizeof(fixture));
    GetSystemInfo(&system_info);
    fixture.page_size = (size_t)system_info.dwPageSize;
    CHECK(fixture.page_size != 0, "Windows reported an empty system page size");
    CHECK((fixture.page_size & (fixture.page_size - 1)) == 0,
          "system page size is not a power of two");

    ll_compat_host_ops ops = {0};
    ll_compat_config config = {0};
    ops.ctx = &fixture;
    ops.malloc_ = host_malloc;
    ops.calloc_ = host_calloc;
    ops.realloc_ = host_realloc;
    ops.free_ = host_free;
    ops.log = host_log;
    ops.fatal = host_fatal;
    ops.on_mem_failure = host_memory_failure;
    ops.page_size = host_page_size;
    config.near_hint = GetModuleHandleW(NULL);
    ll_install_host_for(&owner, &ops, &config);
}

static MEMORY_BASIC_INFORMATION query_region(const void *address)
{
    MEMORY_BASIC_INFORMATION info;
    memset(&info, 0, sizeof(info));
    SIZE_T queried = VirtualQuery(address, &info, sizeof(info));
    CHECK(queried == sizeof(info), "VirtualQuery failed for M32 reservation address");
    return info;
}

static void expect_region(const void *address, DWORD state, const void *base,
                          const char *message)
{
    MEMORY_BASIC_INFORMATION info = query_region(address);
    CHECK(info.State == state, message);
    if (base != NULL)
        CHECK(info.AllocationBase == base,
              "VirtualQuery reported a different AllocationBase");
}

static void remember_base(void *address)
{
    MEMORY_BASIC_INFORMATION info = query_region(address);
    void *base = info.AllocationBase;
    CHECK(base != NULL, "mapped address has no VirtualAlloc AllocationBase");
    for (size_t i = 0; i < fixture.base_count; ++i)
        if (fixture.bases[i] == base) return;
    CHECK(fixture.base_count < MAX_BASES, "reservation base ledger is full");
    if (base != NULL && fixture.base_count < MAX_BASES)
        fixture.bases[fixture.base_count++] = base;
}

static void expect_all_bases_free(void)
{
    for (size_t i = 0; i < fixture.base_count; ++i)
        expect_region(fixture.bases[i], MEM_FREE, NULL,
                      "reservation remained after shutdown");
}

static size_t count_free_calls(DWORD kind, const void *address)
{
    size_t count = 0;
    for (size_t i = 0; i < fixture.free_call_count; ++i) {
        FreeCall *call = &fixture.free_calls[i];
        if (call->kind == kind && (address == NULL || call->address == address))
            ++count;
    }
    return count;
}

static bool reservation_free_call_shape_is_valid(void)
{
    for (size_t i = 0; i < fixture.free_call_count; ++i) {
        FreeCall *call = &fixture.free_calls[i];
        if (call->kind == MEM_RELEASE && call->bytes != 0) return false;
        if (call->kind != MEM_RELEASE && call->kind != MEM_DECOMMIT) return false;
    }
    return true;
}

static void finish_case(bool release_all)
{
    if (release_all) CHECK(ll_m32_shutdown_for(&owner),
                           "M32 reservation shutdown did not complete");
    CHECK(reservation_free_call_shape_is_valid(),
          "VirtualFree used an invalid M32 release shape");
    CHECK(fixture.fatal_calls == 0, "recoverable M32 failure called fatal");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND &&
              memcmp(ll_default_linker.error_message, default_error_sentinel,
                     sizeof(default_error_sentinel)) == 0,
          "owner M32 operation changed default linker error state");
    if (release_all) {
        expect_all_bases_free();
        CHECK(owner.m32_free_page_pool == NULL &&
                  owner.m32_free_page_pool_size == 0,
              "M32 shutdown left free-page pool entries");
        CHECK(fixture.allocation_count == 0,
              "M32 shutdown leaked owner bookkeeping allocations");
    }
}

static void test_query_failure_preserves_io(bool large)
{
    begin_case();
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    fixture.fail_query = 1;
    fixture.fail_release = large ? 1U : 0U;
    fixture.injected_query_error = ERROR_ACCESS_DENIED;
    fixture.injected_query_errno = ERANGE;
    fixture.clobber_host_free_errors = true;
    fixture.host_free_last_error = ERROR_INVALID_HANDLE;
    fixture.host_free_errno = EBUSY;
    ll_error_set(&owner, LL_E_FORMAT, "stale M32 status");
    errno = EDOM;
    SetLastError(ERROR_SUCCESS);

    void *allocation = m32_alloc_for(
        &owner, allocator, large ? fixture.page_size * 2 : 64, 16);
    CHECK(allocation == NULL, "query failure unexpectedly returned an allocation");
    CHECK(owner.error_status == LL_E_IO &&
              strstr(owner.error_message, "VirtualQuery") != NULL,
          "small/large caller overwrote query failure IO status");
    CHECK(GetLastError() == fixture.injected_query_error,
          "query failure did not restore the captured Win32 error");
    CHECK(errno == fixture.injected_query_errno,
          "query failure did not restore errno across host free callback");
    CHECK(fixture.query_calls == 1 && fixture.alloc_calls == 1,
          "query failure did not reach the intended post-map query seam");
    CHECK(fixture.last_mapping != NULL,
          "query failure did not begin with a real OS mapping");
    CHECK(count_free_calls(MEM_RELEASE, fixture.last_mapping) == 1,
          "query failure cleanup did not release its real mapping exactly once");
    if (large) {
        CHECK(owner.m32_reservations != NULL,
              "failed cleanup release discarded its retry record");
        CHECK(count_free_calls(MEM_RELEASE, fixture.last_mapping) == 1,
              "query cleanup did not attempt one injected base release");
    } else {
        CHECK(owner.m32_reservations == NULL,
              "query failure cleanup retained an idle reservation record");
    }

    m32_allocator_free_for(&owner, allocator);
    finish_case(true);
    if (large)
        CHECK(count_free_calls(MEM_RELEASE, fixture.last_mapping) == 2,
              "shutdown did not retry the retained query-cleanup release once");
}

static void test_mapper_failure_replaces_stale_error(bool large)
{
    begin_case();
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    fixture.fail_alloc = 1;
    fixture.injected_alloc_error = ERROR_NOT_ENOUGH_MEMORY;
    fixture.injected_alloc_errno = EFAULT;
    fixture.clobber_host_free_errors = true;
    fixture.host_free_last_error = ERROR_INVALID_HANDLE;
    fixture.host_free_errno = EBUSY;
    ll_error_set(&owner, LL_E_FORMAT, "stale M32 status");
    errno = EDOM;
    SetLastError(ERROR_SUCCESS);

    void *allocation = m32_alloc_for(
        &owner, allocator, large ? fixture.page_size * 2 : 64, 16);
    CHECK(allocation == NULL, "mapper failure unexpectedly returned an allocation");
    CHECK(owner.error_status == LL_E_NOMEM &&
              strstr(owner.error_message, "M32 mapping failed") != NULL,
          "small/large caller failed to replace stale status with mapper NOMEM");
    CHECK(GetLastError() == fixture.injected_alloc_error,
          "mapper failure did not restore the captured Win32 error");
    CHECK(errno == fixture.injected_alloc_errno,
          "mapper failure did not restore errno across host free callback");
    CHECK(fixture.alloc_calls == 1 && fixture.last_mapping == NULL,
          "mapper failure did not fail at the VirtualAlloc seam");
    CHECK(fixture.free_call_count == 0 && owner.m32_reservations == NULL,
          "failed mapper left an OS reservation or reservation record");

    m32_allocator_free_for(&owner, allocator);
    finish_case(true);
}

static void test_shared_batch_subranges(void)
{
    begin_case();
    void *base = ll_m32_map_for(&owner, fixture.page_size * M32_MAP_PAGES);
    CHECK(base != NULL, "could not create shared M32 reservation");
    if (base == NULL) return;
    remember_base(base);
    expect_region(base, MEM_COMMIT, base, "reservation base is not committed");
    CHECK(ll_m32_unmap_for(&owner, base, fixture.page_size),
          "could not decommit reservation base page");
    expect_region(base, MEM_RESERVE, base,
                  "decommitted reservation base page was not retained");
    size_t decommit_calls = count_free_calls(MEM_DECOMMIT, base);
    CHECK(!ll_m32_unmap_for(&owner, base, fixture.page_size),
          "duplicate discard of an already released page was accepted");
    CHECK(owner.error_status == LL_E_STATE && owner.error_message[0] != '\0',
          "duplicate discard did not preserve a state diagnostic");
    CHECK(count_free_calls(MEM_DECOMMIT, base) == decommit_calls,
          "duplicate discard reached VirtualFree a second time");
    expect_region(base, MEM_RESERVE, base,
                  "duplicate discard changed the retained base page");
    decommit_calls = fixture.free_call_count;
    CHECK(!ll_m32_unmap_for(&owner, base,
                            fixture.page_size * (M32_MAP_PAGES + 1)),
          "discard extending beyond the usable reservation was accepted");
    CHECK(owner.error_status == LL_E_STATE && owner.error_message[0] != '\0',
          "out-of-range discard did not preserve a state diagnostic");
    CHECK(fixture.free_call_count == decommit_calls,
          "out-of-range discard reached VirtualFree");
    void *sibling = (uint8_t *)base + fixture.page_size;
    expect_region(sibling, MEM_COMMIT, base,
                  "decommitting base page invalidated a sibling page");
    void *interior = (uint8_t *)base + fixture.page_size * 7;
    CHECK(ll_m32_unmap_for(&owner, interior, fixture.page_size),
          "could not decommit an interior page");
    expect_region(interior, MEM_RESERVE, base,
                  "interior page did not remain reserved after decommit");
    expect_region(sibling, MEM_COMMIT, base,
                  "interior decommit invalidated a separate sibling");
    CHECK(count_free_calls(MEM_RELEASE, NULL) == 0,
          "subrange discard attempted MEM_RELEASE");
    CHECK(ll_m32_unmap_for(&owner, sibling, fixture.page_size * 6),
          "could not decommit first remaining sibling range");
    void *last = (uint8_t *)base + fixture.page_size * 8;
    CHECK(ll_m32_unmap_for(&owner, last, fixture.page_size * (M32_MAP_PAGES - 8)),
          "could not decommit final sibling range");
    finish_case(true);
    CHECK(count_free_calls(MEM_RELEASE, base) == 1,
          "reservation base was not released exactly once");
}

static void test_failed_decommit_residue(void)
{
    begin_case();
    void *base = ll_m32_map_for(&owner, fixture.page_size * M32_MAP_PAGES);
    CHECK(base != NULL, "could not create decommit-failure reservation");
    if (base == NULL) return;
    remember_base(base);
    void *residue = (uint8_t *)base + fixture.page_size * 3;
    fixture.fail_decommit = 1;
    CHECK(!ll_m32_unmap_for(&owner, residue, fixture.page_size),
          "injected MEM_DECOMMIT failure was reported as success");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "decommit failure did not preserve an IO status and diagnostic");
    expect_region(residue, MEM_COMMIT, base,
                  "failed decommit unexpectedly changed native page state");
    CHECK(count_free_calls(MEM_DECOMMIT, residue) == 1,
          "decommit failure was not injected at the requested page");
    CHECK(ll_m32_unmap_for(&owner, base, fixture.page_size * 3),
          "could not discard pages preceding failed-decommit residue");
    void *suffix = (uint8_t *)residue + fixture.page_size;
    CHECK(ll_m32_unmap_for(&owner, suffix, fixture.page_size * (M32_MAP_PAGES - 4)),
          "could not discard pages following failed-decommit residue");
    finish_case(true);
    CHECK(count_free_calls(MEM_RELEASE, base) == 1,
          "ownerless decommit residue prevented final base release");
}

static void test_release_failure_retry(void)
{
    begin_case();
    void *base = ll_m32_map_for(&owner, fixture.page_size * M32_MAP_PAGES);
    CHECK(base != NULL, "could not create release-retry reservation");
    if (base == NULL) return;
    remember_base(base);
    CHECK(ll_m32_unmap_for(&owner, base, fixture.page_size * M32_MAP_PAGES),
          "could not decommit retry reservation pages");
    fixture.fail_release = 1;
    CHECK(!ll_m32_shutdown_for(&owner),
          "injected MEM_RELEASE failure was reported as success");
    expect_region(base, MEM_RESERVE, base,
                  "failed base release did not retain reservation record");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "base release failure did not preserve an IO status and diagnostic");
    CHECK(count_free_calls(MEM_RELEASE, base) == 1,
          "base release failure did not reach VirtualFree once");
    CHECK(ll_m32_shutdown_for(&owner),
          "retry did not release the retained reservation");
    expect_region(base, MEM_FREE, NULL,
                  "successful retry left the reservation allocated");
    CHECK(count_free_calls(MEM_RELEASE, base) == 2,
          "base release was not retried exactly once");
    finish_case(false);
    CHECK(fixture.allocation_count == 0,
          "successful reservation retry leaked owner bookkeeping");
}

static void test_shutdown_preserves_live_allocation(void)
{
    begin_case();
    m32_allocator *allocator = m32_allocator_new_for(&owner, false);
    void *allocation = m32_alloc_for(&owner, allocator, 64, 16);
    CHECK(allocation != NULL, "could not allocate a live small M32 object");
    if (allocation == NULL) {
        m32_allocator_free_for(&owner, allocator);
        finish_case(true);
        return;
    }
    void *page = (uint8_t *)allocation - ROUND_UP(sizeof(struct m32_page_t), 16);
    remember_base(page);
    CHECK(!ll_m32_shutdown_for(&owner),
          "shutdown reported complete while an allocator still owns pages");
    expect_region(page, MEM_COMMIT, NULL,
                  "shutdown released a reservation with a live allocator page");
    CHECK(count_free_calls(MEM_RELEASE, NULL) == 0,
          "shutdown attempted base release while a live allocation remained");
    m32_allocator_free_for(&owner, allocator);
    finish_case(true);
    CHECK(count_free_calls(MEM_RELEASE, NULL) > 0,
          "shutdown after allocator disposal did not release eligible bases");
}

static void test_large_prefix_and_decommitted_tail(void)
{
    begin_case();
    for (unsigned i = 0; i < 8; ++i) {
        m32_allocator *batch = m32_allocator_new_for(&owner, false);
        void *allocation = m32_alloc_for(
            &owner, batch, fixture.page_size * (M32_MAP_PAGES - 1), 16);
        CHECK(allocation != NULL, "could not fill free pool with large allocation");
        if (allocation != NULL) remember_base(allocation);
        m32_allocator_free_for(&owner, batch);
    }
    CHECK(owner.m32_free_page_pool_size == M32_MAX_FREE_PAGE_POOL_SIZE,
          "fixture did not fill the actual 256-page M32 pool");

    m32_allocator *small = m32_allocator_new_for(&owner, false);
    void *small_object = m32_alloc_for(&owner, small, 64, 16);
    CHECK(small_object != NULL, "could not reserve a live page beside full pool");
    CHECK(owner.m32_free_page_pool_size == M32_MAX_FREE_PAGE_POOL_SIZE - 1,
          "small control did not leave exactly one free-pool slot");

    m32_allocator *large = m32_allocator_new_for(&owner, false);
    void *large_object = m32_alloc_for(&owner, large, fixture.page_size * 2, 16);
    CHECK(large_object != NULL, "could not create large reservation with tail");
    if (large_object == NULL) {
        m32_allocator_free_for(&owner, large);
        m32_allocator_free_for(&owner, small);
        finish_case(true);
        return;
    }
    void *large_page = (uint8_t *)large_object - ROUND_UP(sizeof(struct m32_page_t), 16);
    MEMORY_BASIC_INFORMATION reservation = query_region(large_page);
    remember_base(large_page);
    CHECK(reservation.AllocationBase == large_page,
          "large M32 object did not begin at reservation base");
    m32_allocator_free_for(&owner, large);
    CHECK(owner.m32_free_page_pool_size == M32_MAX_FREE_PAGE_POOL_SIZE,
          "large prefix did not enter the final free-pool slot");
    expect_region(large_page, MEM_COMMIT, reservation.AllocationBase,
                  "pooled large prefix was unexpectedly decommitted");
    void *tail = (uint8_t *)large_page + fixture.page_size;
    expect_region(tail, MEM_RESERVE, reservation.AllocationBase,
                  "large-allocation suffix was not decommitted");
    CHECK(count_free_calls(MEM_DECOMMIT, tail) == 1,
          "large suffix did not use page-range MEM_DECOMMIT");
    CHECK(count_free_calls(MEM_RELEASE, large_page) == 0,
          "large suffix discard attempted MEM_RELEASE at an interior address");
    m32_allocator_free_for(&owner, small);
    finish_case(true);
    CHECK(count_free_calls(MEM_RELEASE, reservation.AllocationBase) == 1,
          "large reservation base was not released once after pool drain");
}

static void test_failed_rw_and_decommit_preserve_header(void)
{
    begin_case();
    m32_allocator *allocator = m32_allocator_new_for(&owner, true);
    void *allocation = m32_alloc_for(&owner, allocator, 64, 16);
    CHECK(allocation != NULL, "could not allocate executable M32 page");
    if (allocation == NULL) {
        m32_allocator_free_for(&owner, allocator);
        finish_case(true);
        return;
    }
    memset(allocation, 0x5a, 64);
    m32_allocator_flush_for(&owner, allocator);
    void *page = (uint8_t *)allocation - ROUND_UP(sizeof(struct m32_page_t), 16);
    remember_base(page);
    MEMORY_BASIC_INFORMATION info = query_region(page);
    CHECK((info.Protect & PAGE_EXECUTE_READ) != 0,
          "flush did not make the executable page read/execute");
    unsigned char header_before[sizeof(struct m32_page_t)];
    (void)memcpy(header_before, page, sizeof(header_before));
    unsigned pool_before = owner.m32_free_page_pool_size;
    fixture.fail_rw_protect = 1;
    fixture.fail_decommit = 1;
    m32_allocator_free_for(&owner, allocator);
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "failed RW protection did not preserve its primary IO status");
    CHECK(memcmp(header_before, page, sizeof(header_before)) == 0,
          "failed RW/decommit path wrote into the protected page header");
    CHECK(owner.m32_free_page_pool_size == pool_before,
          "failed RW/decommit path inserted the rejected page into the free pool");
    CHECK(fixture.fail_rw_protect == 0 && fixture.fail_decommit == 0,
          "protection/decommit failure injection did not reach both OS calls");
    expect_region(page, MEM_COMMIT, info.AllocationBase,
                  "failed decommit unexpectedly invalidated the retained residue");
    finish_case(true);
    expect_region(page, MEM_FREE, NULL,
                  "final ownerless-residue release left page committed");
}

int main(void)
{
#if !defined(mingw32_HOST_OS) || !defined(NEED_M32)
    (void)puts("SKIP: M32 reservation test requires generated Windows mmap config");
    return 0;
#else
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    (void)memcpy(ll_default_linker.error_message, default_error_sentinel,
                 sizeof(default_error_sentinel));
    test_shared_batch_subranges();
    test_failed_decommit_residue();
    test_release_failure_retry();
    test_shutdown_preserves_live_allocation();
    test_large_prefix_and_decommitted_tail();
    test_failed_rw_and_decommit_preserve_header();
    test_query_failure_preserves_io(false);
    test_query_failure_preserves_io(true);
    test_mapper_failure_replaces_stale_error(false);
    test_mapper_failure_replaces_stale_error(true);
    if (failures != 0) return 1;
    (void)puts("m32_windows_reservations: PASS");
    return 0;
#endif
}
