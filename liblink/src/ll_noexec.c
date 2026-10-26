#define LL_INTERNAL_SOURCE 1
#include "ll_noexec.h"

#include <stdlib.h>
#include <string.h>

void ll_noexec_native_error(ll_linker *L, char **message)
{
    ll_error_set(L, LL_E_UNSUPPORTED, "native loading is unavailable in NO_EXEC mode");
    if (message != NULL) {
        const size_t bytes = strlen(L->error_message) + 1;
        *message = L->services.host.malloc_(L->services.host.ctx, bytes,
                                           "NO_EXEC native diagnostic");
        memcpy(*message, L->error_message, bytes);
    }
}

int ll_noexec_allocate(ll_linker *L, size_t size, size_t alignment,
                       ll_noexec_block *out)
{
    if (!ll_noexec_enabled(L) || L->noexec_next == 0 || out == NULL) {
        ll_error_set(L, LL_E_STATE, "NO_EXEC requires a nonzero execution base");
        return 0;
    }
    if (alignment == 0 || (alignment & (alignment - 1)) != 0) {
        ll_error_set(L, LL_E_FORMAT, "NO_EXEC block alignment is not a power of two");
        return 0;
    }
    const size_t extent = size != 0 ? size : 1;
    const size_t exec_alignment = alignment < LL_NOEXEC_PAGE_SIZE
                               ? LL_NOEXEC_PAGE_SIZE : alignment;
    if (extent > SIZE_MAX - (alignment - 1) ||
        L->noexec_next > UINTPTR_MAX - (exec_alignment - 1)) goto exhausted;
    const uintptr_t exec = (L->noexec_next + exec_alignment - 1) &
                           ~(uintptr_t)(exec_alignment - 1);
    if (extent > UINTPTR_MAX - exec) goto exhausted;
    const uintptr_t end = exec + extent;
    if (end > UINTPTR_MAX - (LL_NOEXEC_PAGE_SIZE - 1)) goto exhausted;
    const uintptr_t next = (end + LL_NOEXEC_PAGE_SIZE - 1) &
                           ~(uintptr_t)(LL_NOEXEC_PAGE_SIZE - 1);
    /* Section memory is distinct from the host's infallible bookkeeping
     * allocator. Ordinary zeroed heap storage is never made executable. */
    void *allocation = calloc(1, extent + alignment - 1);
    if (allocation == NULL) goto exhausted;
    const size_t residue = (uintptr_t)allocation & (alignment - 1);
    const size_t padding = residue != 0 ? alignment - residue : 0;
    *out = (ll_noexec_block){.allocation = allocation,
        .site = {.work = (uint8_t *)allocation + padding, .exec = exec}};
    L->noexec_next = next;
    return 1;
exhausted:
    ll_error_set(L, LL_E_NOMEM, "NO_EXEC storage or execution address space exhausted");
    return 0;
}

int ll_noexec_allocate_tail(ll_linker *L, size_t payload_size, size_t alignment,
                            size_t tail_size, size_t tail_alignment,
                            ll_noexec_block *out, size_t *tail_offset)
{
    if (L == NULL || out == NULL || tail_offset == NULL) {
        ll_error_set(L, LL_E_STATE, "NO_EXEC tail owner and outputs must be non-NULL");
        return 0;
    }
    if (alignment == 0 || (alignment & (alignment - 1)) != 0) {
        ll_error_set(L, LL_E_FORMAT, "NO_EXEC block alignment is not a power of two");
        return 0;
    }
    size_t offset = payload_size;
    size_t extent = payload_size;
    if (tail_size != 0) {
        if (tail_alignment == 0 || (tail_alignment & (tail_alignment - 1)) != 0) {
            ll_error_set(L, LL_E_FORMAT, "NO_EXEC tail alignment is not a power of two");
            return 0;
        }
        if (payload_size > SIZE_MAX - (tail_alignment - 1)) goto exhausted;
        offset = (payload_size + tail_alignment - 1) & ~(tail_alignment - 1);
        if (tail_size > SIZE_MAX - offset) goto exhausted;
        extent = offset + tail_size;
        if (alignment < tail_alignment) alignment = tail_alignment;
    }
    ll_noexec_block block;
    if (!ll_noexec_allocate(L, extent, alignment, &block)) return 0;
    *out = block;
    *tail_offset = offset;
    return 1;
exhausted:
    ll_error_set(L, LL_E_NOMEM, "NO_EXEC storage or execution address space exhausted");
    return 0;
}
