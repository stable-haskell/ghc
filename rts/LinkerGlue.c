/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * RTS services for the extracted runtime linker.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "LinkerGlue.h"
#include "RtsUtils.h"
#include "ReportMemoryMap.h"
#include "sm/OSMem.h"
#include "linker/ll_host.h"

static void *host_malloc(void *ctx STG_UNUSED, size_t n, const char *what)
{
    return stgMallocBytes(n, (char *)what);
}

static void *host_calloc(void *ctx STG_UNUSED, size_t n, size_t size,
                         const char *what)
{
    return stgCallocBytes(n, size, (char *)what);
}

static void *host_realloc(void *ctx STG_UNUSED, void *p, size_t n,
                          const char *what)
{
    return stgReallocBytes(p, n, (char *)what);
}

static void host_free(void *ctx STG_UNUSED, void *p)
{
    stgFree(p);
}

static void host_log(void *ctx STG_UNUSED, int kind, const char *fmt, va_list ap)
{
    switch (kind) {
    case LL_LOG_DEBUG:    vdebugBelch(fmt, ap); break;
    case LL_LOG_SYSERROR: vsysErrorBelch(fmt, ap); break;
    default:             verrorBelch(fmt, ap); break;
    }
}

static void host_fatal(void *ctx STG_UNUSED, const char *fmt, va_list ap)
{
    vbarf(fmt, ap);
}

static void host_mem_failure(void *ctx STG_UNUSED)
{
    reportMemoryMap();
}

static size_t host_page_size(void *ctx STG_UNUSED)
{
    return getPageSize();
}

static void host_flush_icache(void *ctx STG_UNUSED, void *p, size_t len)
{
    flushExec(len, p);
}

void ll_rts_install_host(void)
{
    const ll_host_ops host = {
        .malloc_ = host_malloc,
        .calloc_ = host_calloc,
        .realloc_ = host_realloc,
        .free_ = host_free,
        .log = host_log,
        .fatal = host_fatal,
        .on_mem_failure = host_mem_failure,
        .page_size = host_page_size,
        .flush_icache = host_flush_icache,
    };
    const ll_config cfg = {
        .near_hint = &stg_upd_frame_info,
        .mem_base = RtsFlags.MiscFlags.linkerMemBase,
        .always_pic = RtsFlags.MiscFlags.linkerAlwaysPic,
        .optimistic = RtsFlags.MiscFlags.linkerOptimistic,
        .debug_mask =
            (RtsFlags.DebugFlags.linker ? LL_DBG_LINKER : 0) |
            (RtsFlags.DebugFlags.linker_verbose ? LL_DBG_LINKER_VERBOSE : 0) |
            (RtsFlags.DebugFlags.sanity ? LL_DBG_SANITY : 0) |
            (RtsFlags.DebugFlags.zero_on_gc ? LL_DBG_ZERO_ON_FREE : 0),
    };
    ll_install_host(&host, &cfg);
}
