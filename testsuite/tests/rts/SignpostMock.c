/* Compile the production wrappers against an observable os_signpost backend. */
#define pthread_atfork mock_pthread_atfork
#include "Signpost.c"
#undef pthread_atfork
#include <assert.h>
#include <errno.h>
#include <stdatomic.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

extern int pthread_atfork(void (*)(void), void (*)(void), void (*)(void));

static atomic_bool enabled;
static atomic_bool fail_atfork;
static atomic_bool fork_in_create;
static atomic_bool fork_worker_ready, fork_worker_stop;
static atomic_uint creates, enabled_checks, child_api_calls, id_generations, begins, ends, events;
static atomic_uint_fast64_t next_id;
static atomic_bool ended[32768];
static int log_object;
static pid_t owner_pid;
static int nested_child_status = -1;

static int child_calls_after_fork(void);

int mock_pthread_atfork(void (*prepare)(void), void (*parent)(void),
                        void (*child)(void))
{
    if (atomic_load(&fail_atfork)) return EAGAIN;
    return pthread_atfork(prepare, parent, child);
}

os_log_t os_log_create(const char *subsystem, const char *category)
{
    assert(strcmp(subsystem, "org.haskell.ghc.rts") == 0);
    assert(strcmp(category, OS_LOG_CATEGORY_POINTS_OF_INTEREST) == 0);
    atomic_fetch_add(&creates, 1);
    if (atomic_exchange(&fork_in_create, false)) {
        pid_t child = fork();
        assert(child >= 0);
        if (child == 0) _exit(child_calls_after_fork());
        int status = 0;
        assert(waitpid(child, &status, 0) == child);
        nested_child_status = WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
    }
    return &log_object;
}

bool os_signpost_enabled(os_log_t log)
{
    assert(log == &log_object);
    atomic_fetch_add(&enabled_checks, 1);
    if (getpid() != owner_pid) atomic_fetch_add(&child_api_calls, 1);
    return atomic_load(&enabled);
}
os_signpost_id_t os_signpost_id_generate(os_log_t log)
{
    assert(log == &log_object);
    atomic_fetch_add(&id_generations, 1);
    if (getpid() != owner_pid) atomic_fetch_add(&child_api_calls, 1);
    return atomic_fetch_add(&next_id, 1) + 1;
}
void mockSignpostEmit(unsigned kind, os_signpost_id_t id,
                      const char *name, const char *format, ...)
{
    assert(name != NULL && format != NULL);
    if (getpid() != owner_pid) atomic_fetch_add(&child_api_calls, 1);
    if (kind == 0) {
        assert(id != 0 && id < 32768);
        atomic_fetch_add(&begins, 1);
    } else if (kind == 1) {
        assert(id != 0 && id < 32768);
        assert(!atomic_exchange(&ended[id], true));
        atomic_fetch_add(&ends, 1);
    } else {
        assert(id == OS_SIGNPOST_ID_EXCLUSIVE);
        atomic_fetch_add(&events, 1);
    }
}

static void *worker(void *arg)
{
    (void)arg;
    initSignposts();
    for (unsigned i = 0; i < 1000; i++) {
        /* Identical or newly grown capability numbers need no shared array. */
        uint64_t id = signpostGcBegin(UINT32_MAX, i % 2);
        assert(id != 0);
        signpostGcEnd(id, i, i);
    }
    return NULL;
}

static void *fork_worker(void *arg)
{
    (void)arg;
    struct timespec pause = { .tv_sec = 0, .tv_nsec = 1000000L };
    atomic_store_explicit(&fork_worker_ready, true, memory_order_release);
    while (!atomic_load_explicit(&fork_worker_stop, memory_order_acquire)) {
        (void)nanosleep(&pause, NULL);
    }
    return NULL;
}

static void start_fork_worker(pthread_t *thread)
{
    assert(pthread_create(thread, NULL, fork_worker, NULL) == 0);
    while (!atomic_load_explicit(&fork_worker_ready, memory_order_acquire)) {
        (void)nanosleep(&(struct timespec){ .tv_sec = 0, .tv_nsec = 1000000L }, NULL);
    }
}

static void stop_fork_worker(pthread_t thread)
{
    atomic_store_explicit(&fork_worker_stop, true, memory_order_release);
    assert(pthread_join(thread, NULL) == 0);
}

static unsigned snapshot_calls(void)
{
    return atomic_load(&creates) + atomic_load(&enabled_checks) +
           atomic_load(&child_api_calls) + atomic_load(&id_generations) + atomic_load(&begins) +
           atomic_load(&ends) + atomic_load(&events);
}

static int child_calls_after_fork(void)
{
    unsigned before = snapshot_calls();
    initSignposts();
    initSignposts();
    if (signpostGcBegin(1, 2) != OS_SIGNPOST_ID_NULL) return 40;
    signpostGcEnd(UINT64_C(123), 456, 789);
    signpostThreadCreate(1, 2);
    signpostThreadRun(1, 2);
    signpostThreadStop(1, 2, 3);
    signpostUserMsg(1, "child");
    signpostUserMarker(1, "child");
    return snapshot_calls() == before ? 0 : 41;
}

static int wait_child(pid_t child)
{
    int status = 0;
    if (waitpid(child, &status, 0) != child) return 90;
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return 91;
}

static int run_fork_after_init(void)
{
    atomic_store(&enabled, true);
    initSignposts();
    signpostThreadCreate(0, 1); /* Ensure the parent emitted a signpost. */
    pthread_t thread;
    start_fork_worker(&thread);
    pid_t child = fork();
    assert(child >= 0);
    if (child == 0) _exit(child_calls_after_fork());
    int child_status = wait_child(child);
    atomic_store(&enabled, true);
    unsigned parent_events = atomic_load(&events);
    signpostThreadRun(0, 2);
    assert(atomic_load(&events) == parent_events + 1);
    stop_fork_worker(thread);
    return child_status;
}

static int run_fork_during_create(void)
{
    atomic_store(&enabled, true);
    pthread_t thread;
    start_fork_worker(&thread);
    atomic_store(&fork_in_create, true);
    initSignposts(); /* mock os_log_create forks while pthread_once is active */
    assert(atomic_load(&creates) == 1);
    assert(nested_child_status == 0);
    stop_fork_worker(thread);
    signpostThreadRun(0, 2); /* Parent logging remains active. */
    assert(atomic_load(&events) == 1);
    return 0;
}

static int run_registration_failure(void)
{
    atomic_store(&enabled, true);
    atomic_store(&fail_atfork, true);
    initSignposts();
    assert(atomic_load(&creates) == 0);
    atomic_store(&fail_atfork, false);
    initSignposts(); /* pthread_once must not retry unsafely */
    assert(atomic_load(&creates) == 0);
    unsigned before = snapshot_calls();
    if (signpostGcBegin(1, 2) != OS_SIGNPOST_ID_NULL) return 50;
    signpostGcEnd(123, 456, 789);
    signpostThreadCreate(1, 2);
    signpostThreadRun(1, 2);
    signpostThreadStop(1, 2, 3);
    signpostUserMsg(1, "disabled");
    signpostUserMarker(1, "disabled");
    return snapshot_calls() == before ? 0 : 51;
}

static int run_existing_tests(void)
{
    if (__builtin_available(macOS 10.14, *)) {
        assert(signpostGcBegin(0, 0) == 0); /* Before initialisation. */
        initSignposts();
        initSignposts();
        assert(atomic_load(&creates) == 1);
        assert(signpostGcBegin(0, 0) == 0); /* Disabled begin. */
        atomic_store(&enabled, true);
        signpostGcEnd(0, 0, 0);             /* Must not emit an orphan end. */
        assert(atomic_load(&ends) == 0);
        uint64_t abandoned = signpostGcBegin(0, 1);
        atomic_store(&enabled, false);
        signpostGcEnd(abandoned, 0, 0);    /* Instruments detached. */
        atomic_store(&enabled, true);
        pthread_t threads[8];
        for (unsigned i = 0; i < 8; i++) {
            assert(pthread_create(&threads[i], NULL, worker, NULL) == 0);
        }
        for (unsigned i = 0; i < 8; i++) {
            assert(pthread_join(threads[i], NULL) == 0);
        }
        assert(atomic_load(&creates) == 1);
        assert(atomic_load(&begins) == 8001);
        assert(atomic_load(&ends) == 8000);
        signpostThreadCreate(0, 1);
        signpostThreadRun(0, 1);
        signpostThreadStop(0, 1, 1);
        signpostUserMsg(0, "message");
        signpostUserMarker(0, "marker");
        signpostUserMsg(0, NULL);
        signpostUserMarker(0, NULL);
        assert(atomic_load(&events) == 5);
    }
    return 0;
}

static int run_isolated_case(int (*test_case)(void))
{
    pid_t child = fork();
    assert(child >= 0);
    if (child == 0) {
        owner_pid = getpid();
        _exit(test_case());
    }
    return wait_child(child);
}

int main(int argc, char **argv)
{
    owner_pid = getpid();
    if (__builtin_available(macOS 10.14, *)) {
        if (argc == 1) {
            int status = run_isolated_case(run_fork_after_init);
            if (status != 0) return status;
            status = run_isolated_case(run_fork_during_create);
            if (status != 0) return status;
            status = run_isolated_case(run_registration_failure);
            if (status != 0) return status;
            return run_existing_tests();
        }
        if (argc != 2) return 64;
        if (strcmp(argv[1], "fork-after-init") == 0) return run_fork_after_init();
        if (strcmp(argv[1], "fork-during-create") == 0) return run_fork_during_create();
        if (strcmp(argv[1], "registration-failure") == 0) return run_registration_failure();
        return 64;
    }
    return 0;
}
