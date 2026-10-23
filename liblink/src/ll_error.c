/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Recoverable errors belong to the linker that encountered them.
 *
 * ---------------------------------------------------------------------------*/

#include "ll_error.h"
#include "ll_linker.h"

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

void ll_error_clear(ll_linker *L)
{
    if (L != NULL) {
        L->error_status = LL_OK;
        L->error_message[0] = '\0';
        L->error_from_pre_backend = false;
    }
}

ll_status ll_error_set(ll_linker *L, ll_status status, const char *fmt, ...)
{
    if (L == NULL) {
        return LL_E_STATE;
    }
    if (status == LL_OK) {
        ll_error_clear(L);
        return LL_OK;
    }

    const int saved_errno = errno;
    L->error_status = status;
    L->error_from_pre_backend = false;
    if (fmt == NULL) {
        fmt = "linker operation failed";
    }

    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(L->error_message, sizeof(L->error_message), fmt, ap);
    va_end(ap);
    if (n < 0) {
        const char fallback[] = "linker diagnostic could not be formatted";
        _Static_assert(sizeof(fallback) <= LL_ERROR_CAPACITY,
                       "fallback diagnostic must fit");
        memcpy(L->error_message, fallback, sizeof(fallback));
    } else if ((size_t)n >= sizeof(L->error_message)) {
        memcpy(L->error_message + sizeof(L->error_message) - 4, "...", 4);
    }
    errno = saved_errno;
    return status;
}
