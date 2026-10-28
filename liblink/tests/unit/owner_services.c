/* A focused check of per-linker host service routing. */

#include "ll_compat.h"
#include "ll_linker.h"

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    unsigned id;
    unsigned malloc_calls;
    unsigned calloc_calls;
    unsigned realloc_calls;
    unsigned free_calls;
    unsigned log_calls;
    unsigned mem_failure_calls;
    size_t page_size;
    unsigned expected_debug_mask;
    int errno_seen;
    int last_log_kind;
    int log_format_ok;
    int last_what_ok;
    char last_message[96];
} TestHost;

static unsigned failures;
static unsigned owner_evaluations;
static size_t value_evaluations;
static unsigned message_evaluations;
static unsigned debug_a;
static unsigned debug_b;

static void verify(int condition, const char *what)
{
    if (!condition) {
        (void)fprintf(stderr, "owner_services: %s\n", what);
        ++failures;
    }
}

static ll_linker *owner_arg(ll_linker *owner)
{
    ++owner_evaluations;
    return owner;
}

static size_t value_arg(size_t *counter, size_t value)
{
    ++*counter;
    return value;
}

static int message_arg(int value)
{
    ++message_evaluations;
    return value;
}

static void *host_malloc(void *opaque, size_t size, const char *what)
{
    TestHost *host = opaque;
    const char *expected = host->id == 3 ? "default allocation" : "owner allocation";
    void *pointer = malloc(size == 0 ? 1 : size);
    ++host->malloc_calls;
    host->last_what_ok = what != NULL && strcmp(what, expected) == 0;
    return pointer;
}

static void *host_calloc(void *opaque, size_t count, size_t size,
                         const char *what)
{
    TestHost *host = opaque;
    void *pointer = calloc(count == 0 || size == 0 ? 1 : count, size == 0 ? 1 : size);
    ++host->calloc_calls;
    host->last_what_ok = what != NULL && strcmp(what, "owner calloc") == 0;
    return pointer;
}

static void *host_realloc(void *opaque, void *pointer, size_t size,
                          const char *what)
{
    TestHost *host = opaque;
    void *result = realloc(pointer, size == 0 ? 1 : size);
    ++host->realloc_calls;
    host->last_what_ok = what != NULL && strcmp(what, "owner realloc") == 0;
    return result;
}

static void host_free(void *opaque, void *pointer)
{
    TestHost *host = opaque;
    ++host->free_calls;
    free(pointer);
}

static void host_log(void *opaque, int kind, const char *format, va_list args)
{
    TestHost *host = opaque;
    int written;
    ++host->log_calls;
    host->last_log_kind = kind;
    host->errno_seen = errno;
    written = vsnprintf(host->last_message, sizeof(host->last_message), format, args);
    host->log_format_ok = written >= 0 && (size_t)written < sizeof(host->last_message);
}

static _Noreturn void host_fatal(void *opaque, const char *format, va_list args)
{
    const TestHost *host = opaque;
    (void)format;
    (void)args;
    _Exit(100 + (int)host->id);
}

static void host_mem_failure(void *opaque)
{
    TestHost *host = opaque;
    ++host->mem_failure_calls;
}

static size_t host_page_size(void *opaque)
{
    TestHost *host = opaque;
    return host->page_size;
}

static ll_compat_host_ops host_ops(TestHost *host)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = host;
    ops.malloc_ = host_malloc;
    ops.calloc_ = host_calloc;
    ops.realloc_ = host_realloc;
    ops.free_ = host_free;
    ops.log = host_log;
    ops.fatal = host_fatal;
    ops.on_mem_failure = host_mem_failure;
    ops.page_size = host_page_size;
    return ops;
}

static ll_compat_config host_config(const TestHost *host)
{
    ll_compat_config config = {0};
    config.debug_mask = host->expected_debug_mask;
    return config;
}

int main(int argc, char **argv)
{
    ll_linker first = {0};
    ll_linker second = {0};
    TestHost first_host = { .id = 1, .page_size = 4096,
                            .expected_debug_mask = LL_DBG_LINKER };
    TestHost second_host = { .id = 2, .page_size = 8192,
                             .expected_debug_mask = 0 };
    TestHost default_host = { .id = 3, .page_size = 16384,
                              .expected_debug_mask = LL_DBG_LINKER };
    ll_compat_host_ops first_ops = host_ops(&first_host);
    ll_compat_host_ops second_ops = host_ops(&second_host);
    ll_compat_host_ops default_ops = host_ops(&default_host);
    ll_compat_config first_config = host_config(&first_host);
    ll_compat_config second_config = host_config(&second_host);
    ll_compat_config default_config = host_config(&default_host);
    void *first_pointer;
    void *second_pointer;
    int predicate_calls;
    size_t rounded;

    ll_install_host(&default_ops, &default_config);
    ll_install_host_for(&first, &first_ops, &first_config);
    ll_install_host_for(&second, &second_ops, &second_config);

    if (argc == 2) {
        if (strcmp(argv[1], "fatal") == 0) {
            LL_FATAL_FOR(&second, "owner fatal probe");
        } else if (strcmp(argv[1], "check") == 0) {
            LL_CHECK_FOR(&second, false);
        } else if (strcmp(argv[1], "checkm") == 0) {
            LL_CHECKM_FOR(&second, false, "owner check-message probe");
        } else if (strcmp(argv[1], "assert") == 0) {
            LL_ASSERT_FOR(&second, false);
            return 0;
        } else {
            (void)fprintf(stderr, "owner_services: unknown mode\n");
            return 2;
        }
    } else if (argc != 1) {
        (void)fprintf(stderr, "owner_services: expected at most one mode\n");
        return 2;
    }

    owner_evaluations = 0;
    value_evaluations = 0;
    first_pointer = LL_MALLOC_FOR(owner_arg(&first),
                                  value_arg(&value_evaluations, 16),
                                  "owner allocation");
    verify(first_pointer != NULL, "first owner allocation failed");
    verify(owner_evaluations == 1 && value_evaluations == 1,
           "allocation arguments were not evaluated once");
    verify(first_host.malloc_calls == 1 && first_host.last_what_ok,
           "allocation did not use the first host context");
    verify(second_host.malloc_calls == 0 && default_host.malloc_calls == 0,
           "allocation leaked to another context");

    second_pointer = LL_CALLOC_FOR(owner_arg(&second), 2, 4, "owner calloc");
    verify(second_pointer != NULL && second_host.calloc_calls == 1
           && second_host.last_what_ok,
           "calloc did not use the second host context");
    first_pointer = LL_REALLOC_FOR(owner_arg(&first), first_pointer, 32,
                                   "owner realloc");
    verify(first_pointer != NULL && first_host.realloc_calls == 1
           && first_host.last_what_ok,
           "realloc did not use the first host context");
    LL_FREE_FOR(owner_arg(&second), second_pointer);
    LL_FREE_FOR(owner_arg(&first), first_pointer);
    verify(second_host.free_calls == 1 && first_host.free_calls == 1,
           "free did not use the matching owner");

    owner_evaluations = 0;
    value_evaluations = 0;
    rounded = ll_round_up_to_page_for(owner_arg(&first),
                                      value_arg(&value_evaluations, 4097));
    verify(rounded == 8192 && owner_evaluations == 1 && value_evaluations == 1,
           "first page rounding or single evaluation failed");
    verify(ll_page_size_for(&second) == 8192
           && ll_round_down_to_page_for(&second, 8193) == 8192
           && ll_round_up_to_page_for(&second, 8193) == 16384,
           "second page-size context was not used");
    ll_report_mem_failure_for(&second);
    verify(second_host.mem_failure_calls == 1 && first_host.mem_failure_calls == 0,
           "memory-failure callback used the wrong context");

    errno = EDOM;
    LL_LOG_ERROR_FOR(owner_arg(&first), "owner-%u error-%d", first_host.id, 7);
    verify(first_host.log_calls == 1 && first_host.last_log_kind == LL_LOG_ERROR
           && first_host.log_format_ok
           && strcmp(first_host.last_message, "owner-1 error-7") == 0
           && first_host.errno_seen == EDOM && errno == EDOM,
           "error log context, formatting, or errno was wrong");
    errno = ERANGE;
    LL_LOG_SYSERROR_FOR(owner_arg(&second), "system-%u", second_host.id);
    verify(second_host.log_calls == 1 && second_host.last_log_kind == LL_LOG_SYSERROR
           && second_host.log_format_ok
           && strcmp(second_host.last_message, "system-2") == 0
           && second_host.errno_seen == ERANGE && errno == ERANGE,
           "system-error log context, formatting, or errno was wrong");
    LL_DEBUG_FOR(owner_arg(&first), "debug-%u", first_host.id);
    verify(first_host.log_calls == 2 && first_host.last_log_kind == LL_LOG_DEBUG
           && strcmp(first_host.last_message, "debug-1") == 0,
           "debug log did not use the first context");

    debug_a = 0;
    debug_b = 0;
    LL_IF_DEBUG_FOR(&first, linker, ++debug_a);
    LL_IF_DEBUG_FOR(&second, linker, ++debug_b);
    verify(debug_a == 1 && debug_b == 0,
           "debug masks were shared across owners");

    predicate_calls = 0;
    message_evaluations = 0;
    LL_CHECK_FOR(owner_arg(&first), ++predicate_calls == 1);
    LL_CHECKM_FOR(owner_arg(&second), true, "message-%d",
                  message_arg(9));
    verify(predicate_calls == 1 && message_evaluations == 0,
           "check predicate or skipped message evaluation was wrong");

    predicate_calls = 0;
    owner_evaluations = 0;
#if LL_ASSERTS_ENABLED
    LL_ASSERT_FOR(owner_arg(&first), ++predicate_calls == 1);
    verify(predicate_calls == 1 && owner_evaluations == 0,
           "passing assertion evaluated an unused owner");
#else
    LL_ASSERT_FOR(owner_arg(&first), ++predicate_calls == 1);
    verify(predicate_calls == 0 && owner_evaluations == 0,
           "disabled assertion evaluated an argument");
#endif

    first_pointer = LL_MALLOC(19, "default allocation");
    verify(first_pointer != NULL && default_host.malloc_calls == 1
           && default_host.last_what_ok,
           "legacy default allocation stopped using the default host");
    LL_FREE(first_pointer);
    verify(default_host.free_calls == 1,
           "legacy default free stopped using the default host");

    return failures == 0 ? 0 : 1;
}
