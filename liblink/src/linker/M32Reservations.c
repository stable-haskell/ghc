/*
 * Windows reservations can contain independently owned M32 pages. Metadata
 * lives outside those pages, which may be RX or decommitted during cleanup.
 */
#define LL_INTERNAL_SOURCE 1
#include "ll_linker.h"
#include "linker/M32Reservations.h"
#include "linker/MMap.h"

#if defined(mingw32_HOST_OS) && defined(NEED_M32)
#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <windows.h>

struct ll_m32_reservation {
    struct ll_m32_reservation *next;
    void *base;
    size_t reservation_bytes;
    size_t usable_bytes;
    size_t active_pages;
    /* One byte per usable page detects duplicate logical discards without
     * reading or writing a page header. Granularity padding has no owner. */
    unsigned char active[];
};

static void reservation_error(ll_linker *L, const char *operation,
                              void *address, size_t bytes, DWORD error)
{
    const int operation_errno = errno;
    if (L->error_status == LL_OK)
        ll_error_set(L, LL_E_IO,
                     "M32 %s failed for %zu bytes at %p (Win32 error %lu)",
                     operation, bytes, address, (unsigned long)error);
    const ll_status status = L->error_status;
    char message[LL_ERROR_CAPACITY];
    memcpy(message, L->error_message, sizeof(message));
    SetLastError(error);
    LL_LOG_SYSERROR_FOR(L, "M32 %s failed for %zu bytes at %p",
                       operation, bytes, address);
    L->error_status = status;
    memcpy(L->error_message, message, sizeof(message));
    errno = operation_errno;
    SetLastError(error);
}

void *ll_m32_map_for(ll_linker *L, size_t bytes)
{
    const size_t page_size = ll_page_size_for(L);
    if (bytes == 0 || page_size == 0 || bytes > SIZE_MAX - (page_size - 1)) {
        ll_error_set(L, LL_E_NOMEM, "M32 reservation size cannot be represented");
        return NULL;
    }
    const size_t usable = ((bytes + page_size - 1) / page_size) * page_size;
    const size_t pages = usable / page_size;
    if (pages > SIZE_MAX - sizeof(struct ll_m32_reservation)) {
        ll_error_set(L, LL_E_NOMEM, "M32 reservation bookkeeping is too large");
        return NULL;
    }
    /* Host bookkeeping allocators are specified to return non-NULL. Allocate
     * before mapping so no unrecorded OS reservation can escape. */
    struct ll_m32_reservation *record = LL_MALLOC_FOR(
        L, sizeof(*record) + pages, "M32 reservation");
    void *base = ll_mmap_anon_for_linker(L, bytes);
    if (base == NULL) {
        const DWORD error = GetLastError();
        const int mapper_errno = errno;
        LL_FREE_FOR(L, record);
        ll_error_set(L, LL_E_NOMEM,
                     "M32 mapping failed for %zu bytes (Win32 error %lu)",
                     bytes, (unsigned long)error);
        errno = mapper_errno;
        SetLastError(error);
        return NULL;
    }
    record->base = base;
    record->usable_bytes = usable;
    record->active_pages = 0;
    record->reservation_bytes = 0;
    memset(record->active, 0, pages);
    record->next = L->m32_reservations;
    L->m32_reservations = record;

    /* A fresh VirtualAlloc reservation is uniformly committed/read-write.
     * RegionSize includes its allocation-granularity padding, while only
     * the requested, page-rounded M32 extent has logical owners. */
    MEMORY_BASIC_INFORMATION info;
    if (VirtualQuery(base, &info, sizeof(info)) != sizeof(info)) {
        const DWORD error = GetLastError();
        const int query_errno = errno;
        /* This is the mapping attempt's primary error, even if an earlier
         * operation left a diagnostic in the owner. Cleanup preserves it. */
        ll_error_clear(L);
        reservation_error(L, "VirtualQuery", base, usable, error);
        (void)ll_m32_release_idle_for(L, false);
        errno = query_errno;
        SetLastError(error);
        return NULL;
    }
    if (info.AllocationBase != base || info.BaseAddress != base ||
        info.State != MEM_COMMIT || info.RegionSize < usable) {
        const int query_errno = errno;
        ll_error_clear(L);
        reservation_error(L, "reservation extent check", base, usable,
                          ERROR_INVALID_ADDRESS);
        (void)ll_m32_release_idle_for(L, false);
        errno = query_errno;
        SetLastError(ERROR_INVALID_ADDRESS);
        return NULL;
    }
    record->reservation_bytes = info.RegionSize;
    record->active_pages = pages;
    memset(record->active, 1, pages);
    return base;
}

bool ll_m32_unmap_for(ll_linker *L, void *address, size_t bytes)
{
    const size_t page_size = ll_page_size_for(L);
    const uintptr_t start = (uintptr_t)address;
    if (bytes == 0 || page_size == 0 || start % page_size != 0 ||
        bytes > SIZE_MAX - (page_size - 1)) {
        ll_error_set(L, LL_E_STATE, "invalid M32 discard range");
        return false;
    }
    const size_t rounded = ((bytes + page_size - 1) / page_size) * page_size;
    for (struct ll_m32_reservation *record = L->m32_reservations;
         record != NULL; record = record->next) {
        const uintptr_t base = (uintptr_t)record->base;
        if (start < base || start - base >= record->usable_bytes) continue;
        const size_t offset = (size_t)(start - base);
        if (rounded > record->usable_bytes - offset) break;
        const size_t first = offset / page_size;
        const size_t count = rounded / page_size;
        for (size_t i = first; i < first + count; ++i) {
            if (record->active[i] == 0) {
                ll_error_set(L, LL_E_STATE, "M32 page owner was already discarded");
                return false;
            }
        }
        /* Logical ownership ends once, even if Windows retains committed
         * residue. The record then owns that residue until base release. */
        memset(record->active + first, 0, count);
        record->active_pages -= count;
        if (VirtualFree(address, rounded, MEM_DECOMMIT) == 0) {
            const DWORD error = GetLastError();
            reservation_error(L, "MEM_DECOMMIT", address, rounded, error);
            return false;
        }
        return true;
    }
    ll_error_set(L, LL_E_STATE, "M32 discard is outside its reservation");
    return false;
}

bool ll_m32_release_idle_for(ll_linker *L, bool require_empty)
{
    bool success = true;
    bool live = false;
    struct ll_m32_reservation **link = &L->m32_reservations;
    while (*link != NULL) {
        struct ll_m32_reservation *record = *link;
        if (record->active_pages != 0) {
            live = true;
            link = &record->next;
        } else if (VirtualFree(record->base, 0, MEM_RELEASE) == 0) {
            const DWORD error = GetLastError();
            reservation_error(L, "MEM_RELEASE", record->base,
                              record->reservation_bytes, error);
            success = false;
            /* Keep the exact base and descriptor for a later retry. */
            link = &record->next;
        } else {
            *link = record->next;
            LL_FREE_FOR(L, record);
        }
    }
    if (require_empty && live) {
        if (L->error_status == LL_OK)
            ll_error_set(L, LL_E_STATE, "M32 reservations still have live allocator owners");
        success = false;
    }
    return success;
}
#endif
