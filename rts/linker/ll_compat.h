/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Compatibility diagnostics for the RTS linker.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ll_host.h"

#include <stdlib.h>

#if defined(__GNUC__) || defined(__clang__)
#define LL_COMPAT_PRINTF(fmt_index, first_arg) \
    __attribute__((format(printf, fmt_index, first_arg)))
#else
#define LL_COMPAT_PRINTF(fmt_index, first_arg)
#endif

static inline void ll_compat_log(int kind, const char *fmt, ...)
    LL_COMPAT_PRINTF(2, 3);

static inline void ll_compat_log(int kind, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    ll_host.log(ll_host.ctx, kind, fmt, ap);
    va_end(ap);
}

static inline _Noreturn void ll_compat_fatal(const char *fmt, ...)
    LL_COMPAT_PRINTF(1, 2);

static inline _Noreturn void ll_compat_fatal(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    ll_host.fatal(ll_host.ctx, fmt, ap);
    va_end(ap);
    abort();
}

#undef LL_COMPAT_PRINTF

#define LL_LOG_ERROR(...) ll_compat_log(LL_LOG_ERROR, __VA_ARGS__)
#define LL_LOG_SYSERROR(...) ll_compat_log(LL_LOG_SYSERROR, __VA_ARGS__)
#define LL_DEBUG(...) ll_compat_log(LL_LOG_DEBUG, __VA_ARGS__)
#define LL_FATAL(...) ll_compat_fatal(__VA_ARGS__)

#define LL_DEBUG_MASK_linker LL_DBG_LINKER
#define LL_DEBUG_MASK_linker_verbose LL_DBG_LINKER_VERBOSE
#define LL_DEBUG_MASK_sanity LL_DBG_SANITY
#define LL_DEBUG_MASK_zero_on_gc LL_DBG_ZERO_ON_FREE

#if defined(DEBUG)
#define LL_IF_DEBUG(category, statement)                                      \
    if ((ll_cfg.debug_mask & LL_DEBUG_MASK_##category) != 0u) {               \
        statement;                                                            \
    }                                                                         \
    do { } while (0)
#else
#define LL_IF_DEBUG(category, statement) do { } while (0)
#endif
