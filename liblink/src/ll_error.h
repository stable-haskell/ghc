/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Private diagnostics for recoverable linker failures.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "liblink.h"
#include "ll_visibility.h"

/* Error reporting must work while unwinding a failed allocation or mapping. */
#define LL_ERROR_CAPACITY 1024

#include "ll_begin_private.h"

/* The caller holds the owner's lock. Clear only at an operation boundary;
 * internal helpers preserve their caller's diagnostic until they fail. */
void ll_error_clear(ll_linker *L);

/* Store a status and diagnostic without logging or allocating. Long messages
 * end with "..." and remain NUL-terminated. Return the supplied status so a
 * status-returning caller can propagate it directly. LL_OK clears the error.
 * A NULL owner returns LL_E_STATE without accessing an instance. */
ll_status ll_error_set(ll_linker *L, ll_status status, const char *fmt, ...)
#if defined(__GNUC__) || defined(__clang__)
    __attribute__((format(printf, 3, 4)))
#endif
    ;

#include "ll_end_private.h"
