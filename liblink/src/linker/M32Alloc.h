/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ll_compat.h"

/*
 * We use the m32 allocator for symbol extras on Windows and other mmap-using
 * platforms.
 */
#if RTS_LINKER_USE_MMAP || defined(mingw32_HOST_OS)
#define NEED_M32 1
#endif

#include "ll_begin_private.h"

#if defined(NEED_M32)
#define M32_NO_RETURN_PREFIX    /* Nothing */
#else
#define M32_NO_RETURN_PREFIX    LL_NORETURN
#endif

struct m32_allocator_t;
typedef struct m32_allocator_t m32_allocator;

M32_NO_RETURN_PREFIX m32_allocator *m32_allocator_new_for(ll_linker *L,
                                                         bool executable);
M32_NO_RETURN_PREFIX void m32_allocator_free_for(ll_linker *L,
                                                 m32_allocator *alloc);
M32_NO_RETURN_PREFIX void m32_allocator_flush_for(ll_linker *L,
                                                  m32_allocator *alloc);
M32_NO_RETURN_PREFIX void *m32_alloc_for(ll_linker *L, m32_allocator *alloc,
                                         size_t size, size_t alignment);

#include "ll_end_private.h"
