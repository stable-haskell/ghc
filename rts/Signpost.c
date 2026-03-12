/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2025
 *
 * os_signpost integration for macOS Instruments profiling.
 *
 * Uses the os_signpost API to emit structured events visible in
 * Apple Instruments. GC pauses appear as intervals in the "Points of
 * Interest" lane; thread and user events appear as point events.
 *
 * All functions check os_signpost_enabled() before doing any work,
 * so the overhead when Instruments is not attached is near zero
 * (a single branch on the log handle's signpost-enabled flag).
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "Signpost.h"

#if defined(darwin_HOST_OS)

/* The log handle used for all GHC RTS signposts.
 * Category "Points of Interest" makes events show up by default
 * in Instruments without needing a custom .instrpkg. */
static os_log_t ghc_signpost_log = NULL;

/* Per-capability signpost IDs for tracking GC begin/end pairs.
 * Each capability's GC interval gets its own signpost ID so that
 * concurrent GCs on different capabilities show as separate intervals. */
static os_signpost_id_t *gc_signpost_ids = NULL;
static uint32_t n_caps_signpost = 0;

void
initSignposts(void)
{
    ghc_signpost_log = os_log_create("org.haskell.ghc.rts",
                                     OS_LOG_CATEGORY_POINTS_OF_INTEREST);

    /* Called after initScheduler(), so getNumCapabilities() returns
     * the correct value. Defensive minimum of 1. */
    n_caps_signpost = getNumCapabilities();
    if (n_caps_signpost == 0) {
        n_caps_signpost = 1;
    }

    gc_signpost_ids = stgMallocBytes(
        n_caps_signpost * sizeof(os_signpost_id_t),
        "initSignposts");

    for (uint32_t i = 0; i < n_caps_signpost; i++) {
        gc_signpost_ids[i] = OS_SIGNPOST_ID_NULL;
    }
}

void
freeSignposts(void)
{
    if (gc_signpost_ids != NULL) {
        stgFree(gc_signpost_ids);
        gc_signpost_ids = NULL;
    }
    /* os_log_t objects are managed by the OS, no need to release */
    ghc_signpost_log = NULL;
    n_caps_signpost = 0;
}

/* ---- GC events ---- */

void
signpostGcBegin(uint32_t cap_no, uint32_t gen)
{
    if (!ghc_signpost_log || !os_signpost_enabled(ghc_signpost_log)) return;

    os_signpost_id_t spid = os_signpost_id_generate(ghc_signpost_log);

    if (cap_no < n_caps_signpost) {
        gc_signpost_ids[cap_no] = spid;
    }

    os_signpost_interval_begin(ghc_signpost_log, spid, "GC",
                               "cap=%u gen=%u", cap_no, gen);
}

void
signpostGcEnd(uint32_t cap_no, uint64_t copied, uint64_t slop)
{
    if (!ghc_signpost_log || !os_signpost_enabled(ghc_signpost_log)) return;

    os_signpost_id_t spid = OS_SIGNPOST_ID_NULL;
    if (cap_no < n_caps_signpost) {
        spid = gc_signpost_ids[cap_no];
        gc_signpost_ids[cap_no] = OS_SIGNPOST_ID_NULL;
    }

    os_signpost_interval_end(ghc_signpost_log, spid, "GC",
                             "copied=%llu slop=%llu",
                             (unsigned long long)copied,
                             (unsigned long long)slop);
}

/* ---- Thread lifecycle ---- */

void
signpostThreadCreate(uint32_t cap_no, uint64_t tid)
{
    if (!ghc_signpost_log || !os_signpost_enabled(ghc_signpost_log)) return;

    os_signpost_event_emit(ghc_signpost_log,
                           OS_SIGNPOST_ID_EXCLUSIVE,
                           "Thread",
                           "create cap=%u tid=%llu",
                           cap_no, (unsigned long long)tid);
}

void
signpostThreadRun(uint32_t cap_no, uint64_t tid)
{
    if (!ghc_signpost_log || !os_signpost_enabled(ghc_signpost_log)) return;

    os_signpost_event_emit(ghc_signpost_log,
                           OS_SIGNPOST_ID_EXCLUSIVE,
                           "Thread",
                           "run cap=%u tid=%llu",
                           cap_no, (unsigned long long)tid);
}

void
signpostThreadStop(uint32_t cap_no, uint64_t tid, uint16_t status)
{
    if (!ghc_signpost_log || !os_signpost_enabled(ghc_signpost_log)) return;

    os_signpost_event_emit(ghc_signpost_log,
                           OS_SIGNPOST_ID_EXCLUSIVE,
                           "Thread",
                           "stop cap=%u tid=%llu status=%u",
                           cap_no, (unsigned long long)tid, status);
}

/* ---- User events ---- */

void
signpostUserMsg(uint32_t cap_no, const char *msg)
{
    if (!ghc_signpost_log || !os_signpost_enabled(ghc_signpost_log)) return;

    os_signpost_event_emit(ghc_signpost_log,
                           OS_SIGNPOST_ID_EXCLUSIVE,
                           "User",
                           "cap=%u msg=%{public}s",
                           cap_no, msg);
}

void
signpostUserMarker(uint32_t cap_no, const char *msg)
{
    if (!ghc_signpost_log || !os_signpost_enabled(ghc_signpost_log)) return;

    os_signpost_event_emit(ghc_signpost_log,
                           OS_SIGNPOST_ID_EXCLUSIVE,
                           "User Marker",
                           "cap=%u marker=%{public}s",
                           cap_no, msg);
}

#endif /* darwin_HOST_OS */
