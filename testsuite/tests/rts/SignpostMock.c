/* Compile the production wrappers against an observable os_signpost backend. */
#include "Signpost.c"
#include <assert.h>
#include <stdatomic.h>
#include <string.h>

static atomic_bool enabled;
static atomic_uint creates, begins, ends, events;
static atomic_uint_fast64_t next_id;
static atomic_bool ended[32768];
static int log_object;

os_log_t os_log_create(const char *subsystem, const char *category)
{
    assert(strcmp(subsystem, "org.haskell.ghc.rts") == 0);
    assert(strcmp(category, OS_LOG_CATEGORY_POINTS_OF_INTEREST) == 0);
    atomic_fetch_add(&creates, 1);
    return &log_object;
}
bool os_signpost_enabled(os_log_t log)
{
    assert(log == &log_object);
    return atomic_load(&enabled);
}
os_signpost_id_t os_signpost_id_generate(os_log_t log)
{
    assert(log == &log_object);
    return atomic_fetch_add(&next_id, 1) + 1;
}
void mockSignpostEmit(unsigned kind, os_signpost_id_t id,
                      const char *name, const char *format, ...)
{
    assert(name != NULL && format != NULL);
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
int main(void)
{
    if (__builtin_available(macOS 10.14, *)) {
        assert(signpostGcBegin(0, 0) == 0); // Before initialisation.
        initSignposts();
        initSignposts();
        assert(atomic_load(&creates) == 1);
        assert(signpostGcBegin(0, 0) == 0); // Disabled begin.
        atomic_store(&enabled, true);
        signpostGcEnd(0, 0, 0);             // Must not emit an orphan end.
        assert(atomic_load(&ends) == 0);
        uint64_t abandoned = signpostGcBegin(0, 1);
        atomic_store(&enabled, false);
        signpostGcEnd(abandoned, 0, 0);    // Instruments detached.
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
