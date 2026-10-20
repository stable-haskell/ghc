/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Compatibility diagnostics for the RTS linker.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ll_autoconf.h"
#include "ll_platform.h"
#include "ll_host.h"

#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <wchar.h>

/* Local scalar spellings mirror the RTS types used by the linker. */
typedef int8_t ll_int8;
typedef uint8_t ll_word8;
typedef int16_t ll_int16;
typedef uint16_t ll_word16;
typedef int32_t ll_int32;
typedef uint32_t ll_word32;
typedef int64_t ll_int64;
typedef uint64_t ll_word64;

#if SIZEOF_VOID_P == 8
typedef int64_t ll_int;
typedef uint64_t ll_word;
#elif SIZEOF_VOID_P == 4
typedef int32_t ll_int;
typedef uint32_t ll_word;
#else
#error "The RTS linker requires a 32-bit or 64-bit pointer width"
#endif

typedef ll_int ll_hs_int;
typedef ll_int ll_hs_bool;
typedef void *ll_hs_ptr;
typedef void *ll_hs_stable_ptr;
typedef ll_word *ll_stg_ptr;
typedef int ll_stg_bool;
typedef ll_word ll_w;

typedef void ll_symbol_addr;
typedef char ll_symbol_name;

typedef enum {
    LL_SYM_CODE = 1 << 0,
    LL_SYM_DATA = 1 << 1,
    LL_SYM_INDIRECT_DATA = 1 << 2,
    LL_SYM_DUP_DISCARD = 1 << 3,
    LL_SYM_HIDDEN = 1 << 4
} ll_sym_type;

typedef enum {
    LL_STRENGTH_NORMAL = 0,
    LL_STRENGTH_WEAK = 1,
    LL_STRENGTH_STRONG = 2
} ll_strength;

#ifndef MAYBE_LEADING_UNDERSCORE_STR
#if defined(LEADING_UNDERSCORE)
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif
#endif

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

#if defined(__GNUC__) || defined(__clang__)
#define LL_LIKELY(predicate) __builtin_expect(!!(predicate), 1)
#else
#define LL_LIKELY(predicate) (!!(predicate))
#endif

#if defined(LL_ASSERTS) || defined(DEBUG) || defined(USE_ASSERTS_ALL_WAYS)
#define LL_ASSERTS_ENABLED 1
#else
#define LL_ASSERTS_ENABLED 0
#endif

#if defined(LL_ASSERTS) || defined(DEBUG)
#define LL_DEBUG_CODE 1
#else
#define LL_DEBUG_CODE 0
#endif

#define LL_CHECK(predicate)                                                   \
    do {                                                                      \
        if (!LL_LIKELY(predicate)) {                                          \
            LL_FATAL("ASSERTION FAILED: file %s, line %u\n",                \
                     __FILE__, (unsigned int)__LINE__);                       \
        }                                                                     \
    } while (0)

#define LL_CHECKM(predicate, ...)                                             \
    do {                                                                      \
        if (!LL_LIKELY(predicate)) {                                          \
            LL_FATAL(__VA_ARGS__);                                            \
        }                                                                     \
    } while (0)

#if LL_ASSERTS_ENABLED
#define LL_ASSERT(predicate) LL_CHECK(predicate)
#else
#define LL_ASSERT(predicate) do { (void)sizeof(predicate); } while (0)
#endif

#define LL_DEBUG_MASK_linker LL_DBG_LINKER
#define LL_DEBUG_MASK_linker_verbose LL_DBG_LINKER_VERBOSE
#define LL_DEBUG_MASK_sanity LL_DBG_SANITY
#define LL_DEBUG_MASK_zero_on_gc LL_DBG_ZERO_ON_FREE

#define LL_IF_DEBUG(category, statement)                                      \
    if ((ll_cfg.debug_mask & LL_DEBUG_MASK_##category) != 0u) {               \
        statement;                                                            \
    }                                                                         \
    do { } while (0)

/* Linker-local adapters keep allocation and memory queries on the host ABI. */
#define LL_MALLOC(size, what) \
    ll_host.malloc_(ll_host.ctx, (size), (what))
#define LL_CALLOC(count, size, what) \
    ll_host.calloc_(ll_host.ctx, (count), (size), (what))
#define LL_REALLOC(ptr, size, what) \
    ll_host.realloc_(ll_host.ctx, (ptr), (size), (what))
#define LL_FREE(ptr) ll_host.free_(ll_host.ctx, (ptr))

static inline size_t ll_page_size(void)
{
    return ll_host.page_size(ll_host.ctx);
}

static inline void ll_report_mem_failure(void)
{
    ll_host.on_mem_failure(ll_host.ctx);
}

static inline size_t ll_round_down_to_page(size_t x)
{
    size_t size = ll_page_size();
    return (x & ~(size - 1));
}

static inline size_t ll_round_up_to_page(size_t x)
{
    size_t size = ll_page_size();
    return (x + size - 1) & ~(size - 1);
}
