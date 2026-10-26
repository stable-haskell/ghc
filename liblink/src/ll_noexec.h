/* Private NO_EXEC storage and deterministic execution-address placement. */
#pragma once

#include "ll_linker.h"
#include "ll_backend.h"

#include "ll_begin_private.h"

static inline bool ll_noexec_enabled(const ll_linker *L)
{
    return (L->services.cfg.flags & LL_CFG_NO_EXEC) != 0;
}

typedef struct {
    void *allocation;          /* free this base, not a possibly aligned work */
    ll_site site;
} ll_noexec_block;

/* Fixed synthetic page size, independent of the running host's page size.
 * Each block receives at least one page, including zero-sized definitions.
 * Successful reservations are monotonic for an owner's lifetime; disposal
 * does not recycle them. A failed call changes neither next address nor output. */
enum { LL_NOEXEC_PAGE_SIZE = 4096 };
int ll_noexec_allocate(ll_linker *L, size_t size, size_t alignment,
                       ll_noexec_block *out);
/* Reserve a payload and an aligned tail in one owner allocation. Zero tail
 * preserves payload-only layout. Failure changes neither output nor address
 * state; tail_offset is published with out only after allocation succeeds.
 * Owner and both output pointers must be non-NULL; otherwise return failure. */
int ll_noexec_allocate_tail(ll_linker *L, size_t payload_size, size_t alignment,
                            size_t tail_size, size_t tail_alignment,
                            ll_noexec_block *out, size_t *tail_offset);
/* Legacy native-load interfaces return an owned bookkeeping diagnostic. */
void ll_noexec_native_error(ll_linker *L, char **message);

#include "ll_end_private.h"
