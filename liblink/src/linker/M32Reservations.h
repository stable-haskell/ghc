/* Private Windows reservation ownership for M32 pages. */
#pragma once

#include "liblink.h"
#include "ll_begin_private.h"

#if defined(mingw32_HOST_OS) && defined(NEED_M32)
void *ll_m32_map_for(ll_linker *L, size_t bytes);
/* Drop logical page owners even if decommit fails. Base release is separate. */
bool ll_m32_unmap_for(ll_linker *L, void *address, size_t bytes);
/* Retry ownerless reservations; require_empty also rejects live allocators. */
bool ll_m32_release_idle_for(ll_linker *L, bool require_empty);
/* Drain pooled owners, then release eligible reservations. Keep live owners. */
bool ll_m32_shutdown_for(ll_linker *L);
#endif

#include "ll_end_private.h"
