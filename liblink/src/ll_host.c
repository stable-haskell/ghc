/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Host installation and libc defaults for the runtime linker.
 *
 * ---------------------------------------------------------------------------*/

#include "ll_linker.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static _Noreturn void allocation_failed(const char *what)
{
    fprintf(stderr, "liblink: allocation failed: %s\n",
            what != NULL ? what : "unknown allocation");
    abort();
}

static void *default_malloc(void *ctx, size_t n, const char *what)
{
    (void)ctx;
    void *p = malloc(n);
    if (p == NULL && n != 0) {
        allocation_failed(what);
    }
    return p;
}

static void *default_calloc(void *ctx, size_t n, size_t size, const char *what)
{
    (void)ctx;
    void *p = calloc(n, size);
    if (p == NULL) {
        allocation_failed(what);
    }
    return p;
}

static void *default_realloc(void *ctx, void *p, size_t n, const char *what)
{
    (void)ctx;
    void *q = realloc(p, n);
    if (q == NULL) {
        allocation_failed(what);
    }
    return q;
}

static void default_free(void *ctx, void *p)
{
    (void)ctx;
    free(p);
}

static void default_log(void *ctx, int kind, const char *fmt, va_list ap)
{
    const int saved_errno = errno;
    (void)ctx;
    vfprintf(stderr, fmt, ap);
    if (kind == LL_LOG_SYSERROR) {
        fprintf(stderr, ": %s", strerror(saved_errno));
    }
    if (kind != LL_LOG_DEBUG) {
        fputc('\n', stderr);
    }
}

static void default_fatal(void *ctx, const char *fmt, va_list ap)
{
    default_log(ctx, LL_LOG_ERROR, fmt, ap);
    abort();
}

#define DEFAULT_HOST { .malloc_ = default_malloc, .calloc_ = default_calloc, \
    .realloc_ = default_realloc, .free_ = default_free, \
    .log = default_log, .fatal = default_fatal }

ll_linker ll_default_linker = {
    .services = { .host = DEFAULT_HOST },
    .gc = { .mutex = &ll_default_linker.linker_mutex },
};

ll_compat_services *const ll_default_services = &ll_default_linker.services;

ll_gc_state *ll_compat_default_gc_state(void)
{
    return &ll_default_linker.gc;
}

void ll_install_host_for(ll_linker *L, const ll_compat_host_ops *host,
                         const ll_compat_config *cfg)
{
    ll_compat_host_ops installed = DEFAULT_HOST;
    if (host != NULL) {
        installed = *host;
        if (installed.malloc_ == NULL) installed.malloc_ = default_malloc;
        if (installed.calloc_ == NULL) installed.calloc_ = default_calloc;
        if (installed.realloc_ == NULL) installed.realloc_ = default_realloc;
        if (installed.free_ == NULL) installed.free_ = default_free;
        if (installed.log == NULL) installed.log = default_log;
        if (installed.fatal == NULL) installed.fatal = default_fatal;
    }
    L->services.host = installed;
    L->services.cfg = cfg != NULL ? *cfg : (ll_compat_config){0};
    L->noexec_next = L->services.cfg.noexec_base;
}

void ll_install_host(const ll_compat_host_ops *host, const ll_compat_config *cfg)
{
    ll_install_host_for(&ll_default_linker, host, cfg);
}
