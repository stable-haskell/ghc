#ifndef LINKER_UNLOAD_TRACE_H
#define LINKER_UNLOAD_TRACE_H

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* Optional, best-effort phase observations for timeout diagnosis. Keep the
 * test's stdout, stderr, and errno intact, including when opening/writing the
 * trace fails. Wall-clock stamps identify phases, not benchmark intervals.
 * Tracing changes timing and cannot prove that an intermittent wait is fixed.
 */
static void linker_unload_trace(const char *phase)
{
    const int saved_errno = errno;
    const char *path = getenv("GHC_LINKER_UNLOAD_TRACE");

    if (phase != NULL && path != NULL && path[0] != '\0') {
        FILE *stream = fopen(path, "a");
        if (stream != NULL) {
            struct timespec stamp;
            if (timespec_get(&stamp, TIME_UTC) == TIME_UTC) {
                (void)fprintf(stream, "%lld.%09ld %s\n",
                              (long long)stamp.tv_sec, stamp.tv_nsec, phase);
            } else {
                (void)fprintf(stream, "time-unavailable %s\n", phase);
            }
            (void)fclose(stream);
        }
    }
    errno = saved_errno;
}

#endif
