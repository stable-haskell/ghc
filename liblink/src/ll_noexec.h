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

/* Fixed synthetic x64 page size, independent of the running host's page size.
 * Each block receives at least one page, including zero-sized definitions.
 * Successful reservations are monotonic for an owner's lifetime; disposal
 * does not recycle them. A failed call changes neither next address nor output. */
enum { LL_NOEXEC_PAGE_SIZE = 4096 };
int ll_noexec_allocate(ll_linker *L, size_t size, size_t alignment,
                       ll_noexec_block *out);
/* Legacy native-load interfaces return an owned bookkeeping diagnostic. */
void ll_noexec_native_error(ll_linker *L, char **message);

#include "ll_end_private.h"
