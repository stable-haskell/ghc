/* (c) The GHC Team, 2026
 * GC intervals and thread/user events for Instruments' Points of Interest.
 * The disabled path still calls these wrappers and checks the OS log flag;
 * its cost has not been measured.
 */
#include "rts/PosixSource.h"
#include "Signpost.h"

#if defined(darwin_HOST_OS)
#include <os/log.h>
#include <os/signpost.h>
#include <pthread.h>

/* os_log_create caches one permanent object per subsystem/category. Keep the
 * handle for process lifetime, including across RTS shutdown, so events never
 * race a log-handle teardown. pthread_once publishes it before scheduler use. */
static os_log_t ghc_signpost_log;
static pthread_once_t signpost_once = PTHREAD_ONCE_INIT;

static void
createSignpostLog(void)
{
    if (__builtin_available(macOS 10.14, *)) {
        ghc_signpost_log = os_log_create("org.haskell.ghc.rts",
                                        OS_LOG_CATEGORY_POINTS_OF_INTEREST);
    }
}

void
initSignposts(void)
{
    pthread_once(&signpost_once, createSignpostLog);
}

uint64_t
signpostGcBegin(uint32_t cap_no, uint32_t gen)
{
    if (__builtin_available(macOS 10.14, *)) {
        if (ghc_signpost_log == NULL || !os_signpost_enabled(ghc_signpost_log)) {
            return OS_SIGNPOST_ID_NULL;
        }
        os_signpost_id_t id = os_signpost_id_generate(ghc_signpost_log);
        os_signpost_interval_begin(ghc_signpost_log, id, "GC",
                                   "cap=%u gen=%u", cap_no, gen);
        return id;
    }
    return 0;
}

void
signpostGcEnd(uint64_t id, uint64_t copied, uint64_t slop)
{
    if (__builtin_available(macOS 10.14, *)) {
        /* Never end an interval whose begin was disabled. The OS macro gates
         * emission if Instruments detached after the begin event. */
        if (id == OS_SIGNPOST_ID_NULL || ghc_signpost_log == NULL) {
            return;
        }
        os_signpost_interval_end(ghc_signpost_log, id, "GC",
                                 "copied=%llu slop=%llu",
                                 (unsigned long long)copied,
                                 (unsigned long long)slop);
    }
}

void
signpostThreadCreate(uint32_t cap_no, uint64_t tid)
{
    if (__builtin_available(macOS 10.14, *)) {
        if (ghc_signpost_log == NULL || !os_signpost_enabled(ghc_signpost_log)) {
            return;
        }
        os_signpost_event_emit(ghc_signpost_log, OS_SIGNPOST_ID_EXCLUSIVE,
                               "Thread", "create cap=%u tid=%llu", cap_no, (unsigned long long)tid);
    }
}

void
signpostThreadRun(uint32_t cap_no, uint64_t tid)
{
    if (__builtin_available(macOS 10.14, *)) {
        if (ghc_signpost_log == NULL || !os_signpost_enabled(ghc_signpost_log)) {
            return;
        }
        os_signpost_event_emit(ghc_signpost_log, OS_SIGNPOST_ID_EXCLUSIVE,
                               "Thread", "run cap=%u tid=%llu", cap_no, (unsigned long long)tid);
    }
}

void
signpostThreadStop(uint32_t cap_no, uint64_t tid, uint16_t status)
{
    if (__builtin_available(macOS 10.14, *)) {
        if (ghc_signpost_log == NULL || !os_signpost_enabled(ghc_signpost_log)) {
            return;
        }
        os_signpost_event_emit(ghc_signpost_log, OS_SIGNPOST_ID_EXCLUSIVE,
                               "Thread", "stop cap=%u tid=%llu status=%u", cap_no, (unsigned long long)tid, (unsigned)status);
    }
}

void
signpostUserMsg(uint32_t cap_no, const char *msg)
{
    if (__builtin_available(macOS 10.14, *)) {
        if (ghc_signpost_log == NULL || !os_signpost_enabled(ghc_signpost_log) || msg == NULL) {
            return;
        }
        os_signpost_event_emit(ghc_signpost_log, OS_SIGNPOST_ID_EXCLUSIVE,
                               "User", "cap=%u msg=%{public}s", cap_no, msg);
    }
}

void
signpostUserMarker(uint32_t cap_no, const char *msg)
{
    if (__builtin_available(macOS 10.14, *)) {
        if (ghc_signpost_log == NULL || !os_signpost_enabled(ghc_signpost_log) || msg == NULL) {
            return;
        }
        os_signpost_event_emit(ghc_signpost_log, OS_SIGNPOST_ID_EXCLUSIVE,
                               "User Marker", "cap=%u marker=%{public}s", cap_no, msg);
    }
}

#endif /* darwin_HOST_OS */
