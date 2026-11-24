/* Focused checks for private per-linker error storage. */

#include "ll_linker.h"

#include <errno.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint64_t before;
    ll_linker linker;
    unsigned char after[32];
} GuardedLinker;

typedef struct {
    unsigned char before[32];
    char text[LL_ERROR_CAPACITY + 512];
    unsigned char after[32];
} GuardedInput;

enum {
    LINKER_TAIL_SIZE = sizeof(ll_linker)
                       - (offsetof(ll_linker, error_message)
                          + sizeof(((ll_linker *)0)->error_message)),
    LINKER_TAIL_OFFSET = offsetof(ll_linker, error_message)
                         + sizeof(((ll_linker *)0)->error_message)
};

static unsigned forbidden_calls;

static void *forbidden_malloc(void *ctx, size_t size, const char *what)
{
    (void)ctx;
    (void)size;
    (void)what;
    ++forbidden_calls;
    return NULL;
}

static void *forbidden_calloc(void *ctx, size_t count, size_t size,
                              const char *what)
{
    (void)ctx;
    (void)count;
    (void)size;
    (void)what;
    ++forbidden_calls;
    return NULL;
}

static void *forbidden_realloc(void *ctx, void *ptr, size_t size,
                               const char *what)
{
    (void)ctx;
    (void)ptr;
    (void)size;
    (void)what;
    ++forbidden_calls;
    return NULL;
}

static void forbidden_free(void *ctx, void *ptr)
{
    (void)ctx;
    (void)ptr;
    ++forbidden_calls;
}

static void forbidden_log(void *ctx, int kind, const char *fmt, va_list ap)
{
    (void)ctx;
    (void)kind;
    (void)fmt;
    (void)ap;
    ++forbidden_calls;
}

static _Noreturn void forbidden_fatal(void *ctx, const char *fmt, va_list ap)
{
    (void)ctx;
    (void)fmt;
    (void)ap;
    _Exit(97);
}

static int check(bool condition, const char *message)
{
    if (!condition) {
        (void)fprintf(stderr, "error_state: %s\n", message);
        return 0;
    }
    return 1;
}

static int check_canaries(const GuardedLinker *first,
                          const GuardedLinker *second,
                          const GuardedInput *input)
{
    const unsigned char expected_after[32] = {
        [0] = 0xa5, [1] = 0xa5, [2] = 0xa5, [3] = 0xa5,
        [4] = 0xa5, [5] = 0xa5, [6] = 0xa5, [7] = 0xa5,
        [8] = 0xa5, [9] = 0xa5, [10] = 0xa5, [11] = 0xa5,
        [12] = 0xa5, [13] = 0xa5, [14] = 0xa5, [15] = 0xa5,
        [16] = 0xa5, [17] = 0xa5, [18] = 0xa5, [19] = 0xa5,
        [20] = 0xa5, [21] = 0xa5, [22] = 0xa5, [23] = 0xa5,
        [24] = 0xa5, [25] = 0xa5, [26] = 0xa5, [27] = 0xa5,
        [28] = 0xa5, [29] = 0xa5, [30] = 0xa5, [31] = 0xa5
    };
    const unsigned char expected_before = 0x5a;
    for (size_t i = 0; i < sizeof(first->after); ++i) {
        if (first->after[i] != expected_after[i]
            || second->after[i] != expected_after[i]) {
            return check(false, "owner trailing guard changed");
        }
    }
    if (first->before != UINT64_C(0x5a5a5a5a5a5a5a5a)
        || second->before != UINT64_C(0x5a5a5a5a5a5a5a5a)) {
        return check(false, "owner leading guard changed");
    }
    for (size_t i = 0; i < sizeof(input->before); ++i) {
        if (input->before[i] != expected_before
            || input->after[i] != 0xa5) {
            return check(false, "long input guard changed");
        }
    }
    return 1;
}

int main(void)
{
    GuardedLinker first = {
        .before = UINT64_C(0x5a5a5a5a5a5a5a5a),
        .linker = {0},
        .after = {0}
    };
    GuardedLinker second = {
        .before = UINT64_C(0x5a5a5a5a5a5a5a5a),
        .linker = {0},
        .after = {0}
    };
    GuardedInput input;
    unsigned char first_tail_before[LINKER_TAIL_SIZE];
    unsigned char second_tail_before[LINKER_TAIL_SIZE];
    ll_compat_host_ops poison = {0};
    static const char default_sentinel[] = "default owner sentinel";
    int ok = 1;

    poison.malloc_ = forbidden_malloc;
    poison.calloc_ = forbidden_calloc;
    poison.realloc_ = forbidden_realloc;
    poison.free_ = forbidden_free;
    poison.log = forbidden_log;
    poison.fatal = forbidden_fatal;
    memset(first.after, 0xa5, sizeof(first.after));
    memset(second.after, 0xa5, sizeof(second.after));
    ll_install_host_for(&first.linker, &poison, NULL);
    ll_install_host_for(&second.linker, &poison, NULL);

    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_sentinel,
           sizeof(default_sentinel));
    memcpy(first_tail_before,
           (unsigned char *)&first.linker
               + LINKER_TAIL_OFFSET,
           sizeof(first_tail_before));
    memcpy(second_tail_before,
           (unsigned char *)&second.linker
               + LINKER_TAIL_OFFSET,
           sizeof(second_tail_before));

    ok &= check(ll_error_set(&first.linker, LL_E_FORMAT,
                             "bad %s at index %u", "object", 17u)
                    == LL_E_FORMAT,
                "formatted error returned wrong status");
    ok &= check(first.linker.error_status == LL_E_FORMAT
                    && strcmp(first.linker.error_message,
                              "bad object at index 17") == 0,
                "formatted diagnostic was not stored");
    ok &= check(second.linker.error_status == LL_OK
                    && second.linker.error_message[0] == '\0',
                "first owner's error leaked into second owner");

    ok &= check(ll_error_set(&second.linker, LL_E_UNRESOLVED,
                             "missing %s", "target") == LL_E_UNRESOLVED,
                "second owner status was not returned");
    ok &= check(first.linker.error_status == LL_E_FORMAT
                    && strcmp(first.linker.error_message,
                              "bad object at index 17") == 0,
                "second owner's error changed first owner");

    errno = EDOM;
    ok &= check(ll_error_set(&first.linker, LL_E_IO, "errno probe %d", 9)
                    == LL_E_IO,
                "errno probe returned wrong status");
    ok &= check(errno == EDOM, "error recording changed errno");

    ok &= check(ll_error_set(&first.linker, LL_OK, "ignored %d", 1) == LL_OK,
                "LL_OK did not return success");
    ok &= check(first.linker.error_status == LL_OK
                    && first.linker.error_message[0] == '\0',
                "LL_OK did not clear error state");
    (void)ll_error_set(&first.linker, LL_E_INIT, "clear me");
    ll_error_clear(&first.linker);
    ok &= check(first.linker.error_status == LL_OK
                    && first.linker.error_message[0] == '\0',
                "ll_error_clear did not clear error state");

    ok &= check(ll_error_set(&first.linker, LL_E_IO, NULL) == LL_E_IO
                    && first.linker.error_status == LL_E_IO
                    && strcmp(first.linker.error_message,
                              "linker operation failed") == 0,
                "NULL format did not use fallback while retaining status");
    ok &= check(ll_error_set(NULL, LL_E_FORMAT, "ignored") == LL_E_STATE,
                "NULL owner did not return LL_E_STATE");
    ok &= check(ll_error_set(NULL, LL_E_FORMAT, NULL) == LL_E_STATE,
                "NULL owner/format contract changed");
    ll_error_clear(NULL);

    memset(&input, 0, sizeof(input));
    memset(input.before, 0x5a, sizeof(input.before));
    memset(input.after, 0xa5, sizeof(input.after));
    memset(input.text, 'x', sizeof(input.text));
    input.text[LL_ERROR_CAPACITY - 1] = '\0';
    ok &= check(ll_error_set(&first.linker, LL_E_FORMAT, "%s", input.text)
                    == LL_E_FORMAT,
                "exact-boundary message returned wrong status");
    ok &= check(strlen(first.linker.error_message) == LL_ERROR_CAPACITY - 1
                    && first.linker.error_message[LL_ERROR_CAPACITY - 1] == '\0'
                    && first.linker.error_message[LL_ERROR_CAPACITY - 4] == 'x',
                "exact-boundary message was truncated or not terminated");

    input.text[LL_ERROR_CAPACITY - 1] = 'x';
    input.text[LL_ERROR_CAPACITY + 200] = '\0';
    errno = ERANGE;
    ok &= check(ll_error_set(&second.linker, LL_E_UNSUPPORTED, "%s",
                             input.text) == LL_E_UNSUPPORTED,
                "long message returned wrong status");
    ok &= check(errno == ERANGE, "long error recording changed errno");
    ok &= check(second.linker.error_status == LL_E_UNSUPPORTED
                    && strlen(second.linker.error_message)
                           == LL_ERROR_CAPACITY - 1
                    && memcmp(second.linker.error_message, input.text,
                              LL_ERROR_CAPACITY - 4) == 0
                    && memcmp(second.linker.error_message
                                  + LL_ERROR_CAPACITY - 4,
                              "...", 4) == 0,
                "long message did not retain prefix, suffix and NUL");

    ok &= check(ll_default_linker.error_status == LL_E_NOT_FOUND
                    && strcmp(ll_default_linker.error_message,
                              "default owner sentinel") == 0,
                "owner-scoped error modified the default owner");
    ok &= check(memcmp(first_tail_before,
                       (unsigned char *)&first.linker
                           + LINKER_TAIL_OFFSET,
                       sizeof(first_tail_before)) == 0
                    && memcmp(second_tail_before,
                              (unsigned char *)&second.linker
                                  + LINKER_TAIL_OFFSET,
                              sizeof(second_tail_before)) == 0,
                "error recording changed owner bytes after the message");
    ok &= check(forbidden_calls == 0,
                "error recording invoked a host allocator/logger/fatal hook");
    ok &= check(check_canaries(&first, &second, &input),
                "memory guard verification failed");

    return ok ? 0 : 1;
}
