/*
 * ghc-throttle-status: Report current ghc-throttle slot usage (Windows)
 *
 * Licensed under the Apache License, Version 2.0.
 *
 * Probes named mutexes to report which slots are currently held.
 * Unlike the POSIX version which scans lock files, Windows named mutexes
 * cannot be enumerated — we probe slots 0..max_jobs-1 by name.
 *
 * Usage:
 *   ghc-throttle-status              — probe default slot count
 *   ghc-throttle-status --help       — show usage
 *
 * Note: Windows does not provide a way to determine which PID holds a
 * named mutex, so holder information is not available.
 */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SLOTS 256
#define MUTEX_NAME_BUF 64

/* --------------------------------------------------------------------------
 * CPU count detection
 * -------------------------------------------------------------------------- */

static int get_ncpus(void)
{
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return (int)si.dwNumberOfProcessors > 0 ? (int)si.dwNumberOfProcessors : 2;
}

/* --------------------------------------------------------------------------
 * Concurrency limit (needed to know how many slots to probe)
 * -------------------------------------------------------------------------- */

static int get_max_jobs(void)
{
    const char *env = getenv("GHC_THROTTLE_JOBS");
    if (env && *env) {
        char *end;
        long val = strtol(env, &end, 10);
        if (*end == '\0' && val > 0 && val <= MAX_SLOTS)
            return (int)val;
        fprintf(stderr,
                "ghc-throttle-status: ignoring invalid GHC_THROTTLE_JOBS=%s\n",
                env);
    }
    /* Default: half the CPUs, clamped to [1, MAX_SLOTS]. */
    int n = get_ncpus() / 2;
    if (n <= 0) return 1;
    return n <= MAX_SLOTS ? n : MAX_SLOTS;
}

/* --------------------------------------------------------------------------
 * Help
 * -------------------------------------------------------------------------- */

static void print_usage(void)
{
    printf("ghc-throttle-status: Report current ghc-throttle slot usage\n"
           "\n"
           "Usage:\n"
           "  ghc-throttle-status              Show slot status\n"
           "  ghc-throttle-status --help|-h    Show this help\n"
           "\n"
           "Environment:\n"
           "  GHC_THROTTLE_JOBS  Max slots to probe (default: ncpus / 2)\n"
           "\n"
           "Note: On Windows, named mutexes are used instead of lock files.\n"
           "      PID-of-holder detection is not available.\n");
}

/* --------------------------------------------------------------------------
 * Slot probing
 * -------------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    if (argc > 1 &&
        (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0))
    {
        print_usage();
        return 0;
    }

    int max_jobs = get_max_jobs();
    int held = 0;
    int nslots = 0;

    /* locked: 0 = free, 1 = locked, 2 = unknown (probe error) */
    struct slot_info {
        int number;
        int locked;
    } slots[MAX_SLOTS];

    for (int i = 0; i < max_jobs; i++) {
        char name[MUTEX_NAME_BUF];
        _snprintf(name, sizeof(name), "Local\\ghc-throttle-slot-%d", i);
        name[sizeof(name) - 1] = '\0';

        /* Try to open the existing mutex (don't create one).
         * Need SYNCHRONIZE for WaitForSingleObject and MUTEX_MODIFY_STATE
         * for ReleaseMutex (when we acquire a free slot and release it). */
        HANDLE h = OpenMutexA(SYNCHRONIZE | MUTEX_MODIFY_STATE, FALSE, name);
        if (h == NULL) {
            DWORD err = GetLastError();
            if (err == ERROR_FILE_NOT_FOUND) {
                /* Mutex doesn't exist — slot has never been used. */
                slots[nslots].number = i;
                slots[nslots].locked = 0;
                nslots++;
            } else {
                /* Other errors (e.g., access denied) — record as unknown. */
                fprintf(stderr, "ghc-throttle-status: cannot probe slot %d: "
                        "error %lu\n", i, (unsigned long)err);
                slots[nslots].number = i;
                slots[nslots].locked = 2; /* unknown */
                nslots++;
            }
            continue;
        }

        /* Mutex exists — check if it's held by trying a zero-timeout wait. */
        DWORD result = WaitForSingleObject(h, 0);
        slots[nslots].number = i;

        if (result == WAIT_OBJECT_0 || result == WAIT_ABANDONED) {
            /* We acquired it — it was free.  Release immediately. */
            ReleaseMutex(h);
            slots[nslots].locked = 0;
        } else if (result == WAIT_TIMEOUT) {
            /* Mutex is held by another process. */
            slots[nslots].locked = 1;
            held++;
        } else {
            /* WAIT_FAILED or unexpected result — record as unknown. */
            fprintf(stderr, "ghc-throttle-status: WaitForSingleObject slot %d "
                    "failed: %lu\n", i, (unsigned long)GetLastError());
            slots[nslots].locked = 2; /* unknown */
        }

        CloseHandle(h);
        nslots++;
    }

    printf("GHC Throttle: %d/%d slots in use  [named mutexes]\n",
           held, max_jobs);
    for (int i = 0; i < nslots; i++) {
        if (slots[i].locked == 1) {
            printf("  slot.%-3d  locked\n", slots[i].number);
        } else if (slots[i].locked == 2) {
            printf("  slot.%-3d  unknown (probe failed)\n", slots[i].number);
        } else {
            printf("  slot.%-3d  free\n", slots[i].number);
        }
    }

    return 0;
}
