#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/select.h>
#include <time.h>
#include <unistd.h>

#define RTS_USER_SIGNALS 1
static struct { struct { bool install_signal_handlers; } MiscFlags; }
    RtsFlags = {{ true }};
static volatile sig_atomic_t pending;
static sigset_t users;
static int calls;
static bool inject_at_wait;
static bool fail_block;
static bool fail_restore;
static long expected_nsec;
static bool expect_null_timeout;

static void on_signal(int sig) { pending = sig; }
#define signals_pending() (pending != 0)

static int blockUserSignalsWithSavedMask(sigset_t *saved)
{
    if (fail_block) { errno = EACCES; return -1; }
    return sigprocmask(SIG_BLOCK, &users, saved);
}

/* Force delivery after the pending check but before the atomic wait. */
static int injected_pselect(int nfds, fd_set *rf, fd_set *wf, fd_set *ef,
                            const struct timespec *ts, const sigset_t *mask)
{
    sigset_t current;
    calls++;
    assert(sigprocmask(SIG_SETMASK, NULL, &current) == 0);
    assert(sigismember(&current, SIGINT) == 1);
    assert(sigismember(mask, SIGINT) == 0);
    assert(sigismember(mask, SIGUSR1) == 1);
    assert((ts == NULL) == expect_null_timeout);
    if (ts != NULL) { assert(ts->tv_sec == 0); assert(ts->tv_nsec == expected_nsec); }
    if (inject_at_wait) {
        assert(raise(SIGINT) == 0);
        assert(pending == 0); /* delivery remains blocked until pselect */
    }
    return pselect(nfds, rf, wf, ef, ts, mask);
}

/* A successful mask restoration is allowed to alter errno. */
static int noisy_sigprocmask(int how, const sigset_t *set, sigset_t *old)
{
    if (fail_restore) { errno = EINVAL; return -1; }
    int rc = sigprocmask(how, set, old);
    if (rc == 0) { errno = EDOM; }
    return rc;
}
#define pselect injected_pselect
#define sigprocmask noisy_sigprocmask
#include "SelectSignalWait.inc"
#undef sigprocmask
#undef pselect

static void check_mask(void)
{
    sigset_t mask;
    assert(sigprocmask(SIG_SETMASK, NULL, &mask) == 0);
    assert(sigismember(&mask, SIGINT) == 0);
    assert(sigismember(&mask, SIGUSR1) == 1);
}

int main(void)
{
    struct sigaction action = {0};
    sigset_t original, start;
    struct timeval timeout;
    int fds[2], result;
    fd_set rf;
    char byte;
    action.sa_handler = on_signal;
    assert(sigemptyset(&action.sa_mask) == 0);
    assert(sigaction(SIGINT, &action, NULL) == 0);
    assert(sigemptyset(&users) == 0);
    assert(sigaddset(&users, SIGINT) == 0);
    assert(sigprocmask(SIG_SETMASK, NULL, &original) == 0);
    start = original;
    assert(sigdelset(&start, SIGINT) == 0);
    assert(sigaddset(&start, SIGUSR1) == 0);
    assert(sigprocmask(SIG_SETMASK, &start, NULL) == 0);

    /* The old select sequence misses a signal already queued before waiting.
     * A finite 20ms timeout witnesses the missed wakeup without hanging test. */
    assert(raise(SIGINT) == 0);
    assert(pending == SIGINT);
    timeout = (struct timeval){0, 20000};
    assert(select(0, NULL, NULL, NULL, &timeout) == 0);
    assert(pending == SIGINT);

    /* Same queued signal: new path returns immediately without pselect. */
    timeout = (struct timeval){0, 20000};
    errno = 0;
    result = selectWithUserSignals(0, NULL, NULL, &timeout);
    assert(result == -1 && errno == EINTR && calls == 0);
    assert(timeout.tv_usec == 20000);
    check_mask();

    /* Signal arrives inside the otherwise vulnerable check/wait interval. */
    pending = 0; inject_at_wait = true; expect_null_timeout = true;
    result = selectWithUserSignals(0, NULL, NULL, NULL);
    assert(result == -1 && errno == EINTR && calls == 1 && pending == SIGINT);
    check_mask();

    pending = 0; inject_at_wait = false; expect_null_timeout = false;
    expected_nsec = 1000000;
    timeout = (struct timeval){0, 1000};
    assert(selectWithUserSignals(0, NULL, NULL, &timeout) == 0);
    assert(timeout.tv_usec == 1000);
    check_mask();

    assert(pipe(fds) == 0);
    assert(write(fds[1], "x", 1) == 1);
    FD_ZERO(&rf); FD_SET(fds[0], &rf);
    expected_nsec = 0; timeout = (struct timeval){0, 0};
    assert(selectWithUserSignals(fds[0]+1, &rf, NULL, &timeout) == 1);
    assert(FD_ISSET(fds[0], &rf));
    assert(read(fds[0], &byte, 1) == 1 && byte == 'x');
    assert(close(fds[0]) == 0); assert(close(fds[1]) == 0);
    FD_ZERO(&rf); FD_SET(fds[0], &rf);
    result = selectWithUserSignals(fds[0]+1, &rf, NULL, &timeout);
    assert(result == -1 && errno == EBADF);
    check_mask();

    fail_block = true;
    result = selectWithUserSignals(0, NULL, NULL, &timeout);
    assert(result == -1 && errno == EACCES);
    check_mask();
    fail_block = false;

    /* Restoration failure must not be reported as successful readiness. The
     * caller must treat this error as fatal; the test repairs its own mask. */
    fail_restore = true;
    result = selectWithUserSignals(0, NULL, NULL, &timeout);
    assert(result == -1 && errno == EINVAL);
    fail_restore = false;
    assert(sigprocmask(SIG_SETMASK, &start, NULL) == 0);
    check_mask();

    /* With RTS signal installation disabled, retain the ordinary select path.
     * An injected blocking failure proves the signal helper is not called. */
    RtsFlags.MiscFlags.install_signal_handlers = false;
    fail_block = true;
    int previous_calls = calls;
    assert(raise(SIGINT) == 0);
    timeout = (struct timeval){0, 0};
    assert(selectWithUserSignals(0, NULL, NULL, &timeout) == 0);
    assert(calls == previous_calls && pending == SIGINT);
    check_mask();

    assert(sigprocmask(SIG_SETMASK, &original, NULL) == 0);
    puts("PASS: queued signal, atomic delivery, timeout, readiness, errno, masks");
    return 0;
}
