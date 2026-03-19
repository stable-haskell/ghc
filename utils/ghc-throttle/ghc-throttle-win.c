/*
 * ghc-throttle: Transparent GHC concurrency limiter (Windows)
 *
 * Licensed under the Apache License, Version 2.0.
 *
 * Windows implementation using named mutexes.  Unlike the POSIX version
 * (which uses flock + exec), Windows cannot replace the current process
 * image, so we hold the mutex while the child GHC runs via CreateProcess.
 *
 * Named mutexes are kernel objects that are automatically released when the
 * owning process exits — crash-safe like flock() on POSIX.
 *
 * Environment variables:
 *   GHC_THROTTLE_GHC   — path to the real GHC binary
 *   GHC_THROTTLE_JOBS  — max concurrent GHC processes (default: ncpus / 2)
 *   GHC_THROTTLE_DEBUG — if set to "1", print diagnostics to stderr
 */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Maximum number of concurrency slots we ever allow. */
#define MAX_SLOTS 256

/* Size of path buffers. */
#define PATH_BUF 4096

/* Maximum length for a mutex name (Local\ghc-throttle-slot-NNN). */
#define MUTEX_NAME_BUF 64

static int debug_enabled = 0;

#define DBG(...) do { \
    if (debug_enabled) { \
        fprintf(stderr, "ghc-throttle[%lu]: ", (unsigned long)GetCurrentProcessId()); \
        fprintf(stderr, __VA_ARGS__); \
        fprintf(stderr, "\n"); \
    } \
} while (0)

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
 * Concurrency limit
 * -------------------------------------------------------------------------- */

static int get_max_jobs(void)
{
    const char *env = getenv("GHC_THROTTLE_JOBS");
    if (env && *env) {
        char *end;
        long val = strtol(env, &end, 10);
        if (*end == '\0' && val > 0 && val <= MAX_SLOTS)
            return (int)val;
        fprintf(stderr, "ghc-throttle: ignoring invalid GHC_THROTTLE_JOBS=%s\n",
                env);
    }
    /* Default: half the CPUs, clamped to [1, MAX_SLOTS]. */
    int n = get_ncpus() / 2;
    if (n <= 0) return 1;
    return n <= MAX_SLOTS ? n : MAX_SLOTS;
}

/* --------------------------------------------------------------------------
 * Query flag detection
 * -------------------------------------------------------------------------- */

static int is_query_flag(const char *arg)
{
    return strcmp(arg, "--version") == 0
        || strcmp(arg, "--numeric-version") == 0
        || strcmp(arg, "--info") == 0
        || strcmp(arg, "--supported-extensions") == 0
        || strcmp(arg, "--supported-languages") == 0
        || strcmp(arg, "--show-options") == 0
        || strncmp(arg, "--print-", 8) == 0;
}

static int should_bypass(int argc, char *argv[])
{
    for (int i = 1; i < argc; i++) {
        /* Stop scanning at end-of-options marker. */
        if (strcmp(argv[i], "--") == 0)
            break;
        if (is_query_flag(argv[i]))
            return 1;
        if (strcmp(argv[i], "-jsem") == 0 ||
            strncmp(argv[i], "-jsem=", 6) == 0)
            return 1;
    }
    return 0;
}

/* --------------------------------------------------------------------------
 * Slot acquisition via named mutexes
 *
 * Uses Local\ prefix so the namespace is per-logon-session (roughly
 * matching the per-UID semantics of the POSIX flock version).
 * -------------------------------------------------------------------------- */

static HANDLE acquired_mutex = NULL;

static void acquire_slot(int max_jobs)
{
    char name[MUTEX_NAME_BUF];
    HANDLE h;

    /* Fast path: try each slot without blocking.
     * ERROR_ALREADY_EXISTS only tells us whether *this* call created the
     * mutex vs. opened an existing one — it does not indicate contention.
     * Instead, open/create the mutex and do a zero-timeout wait to test
     * availability. */
    for (int i = 0; i < max_jobs; i++) {
        _snprintf(name, sizeof(name), "Local\\ghc-throttle-slot-%d", i);
        name[sizeof(name) - 1] = '\0';

        h = CreateMutexA(NULL, FALSE, name);
        if (h == NULL)
            continue;

        DWORD result = WaitForSingleObject(h, 0);
        if (result == WAIT_OBJECT_0 || result == WAIT_ABANDONED) {
            /* Got the mutex without blocking — slot acquired. */
            DBG("acquired slot %d (fast path)", i);
            acquired_mutex = h;
            return;
        }
        if (result == WAIT_FAILED) {
            DBG("WaitForSingleObject slot %d failed: %lu",
                i, (unsigned long)GetLastError());
        }
        /* WAIT_TIMEOUT or WAIT_FAILED — try next slot. */
        CloseHandle(h);
    }

    /* Slow path: block on a deterministic slot based on PID. */
    int slot = (int)(GetCurrentProcessId() % (DWORD)max_jobs);
    _snprintf(name, sizeof(name), "Local\\ghc-throttle-slot-%d", slot);
    name[sizeof(name) - 1] = '\0';

    h = CreateMutexA(NULL, FALSE, name);
    if (h == NULL) {
        fprintf(stderr, "ghc-throttle: CreateMutex(%s) failed: %lu\n",
                name, (unsigned long)GetLastError());
        return; /* proceed unthrottled */
    }

    DBG("blocking on slot %d (slow path)", slot);

    DWORD result = WaitForSingleObject(h, INFINITE);
    if (result == WAIT_OBJECT_0 || result == WAIT_ABANDONED) {
        DBG("acquired slot %d (slow path)", slot);
        acquired_mutex = h;
    } else {
        fprintf(stderr, "ghc-throttle: WaitForSingleObject failed: %lu\n",
                (unsigned long)GetLastError());
        CloseHandle(h);
        /* proceed unthrottled */
    }
}

/* --------------------------------------------------------------------------
 * Command-line construction
 *
 * Windows requires a flat command-line string.  Each argument is wrapped in
 * double quotes with internal quotes and backslashes escaped.
 * -------------------------------------------------------------------------- */

/* Windows command-line quoting follows the CommandLineToArgvW convention:
 * - Wrap each argument in double quotes
 * - Backslashes are literal UNLESS followed by a double quote
 * - A run of N backslashes before a " becomes 2N+1 characters: N*2 backslashes + \"
 * - A run of N backslashes at the end of the argument becomes 2N (before closing ")
 * See: https://learn.microsoft.com/en-us/cpp/c-language/parsing-c-command-line-arguments */

/* Compute the length needed for a quoted argument (including quotes and space). */
static size_t quoted_len(const char *arg)
{
    size_t len = 3; /* opening quote, closing quote, trailing space or NUL */
    size_t nbs = 0; /* backslash run length */
    for (const char *p = arg; *p; p++) {
        if (*p == '\\') {
            nbs++;
            len++;
        } else if (*p == '"') {
            len += nbs + 2; /* double the backslashes + escaped quote */
            nbs = 0;
        } else {
            nbs = 0;
            len++;
        }
    }
    len += nbs; /* double trailing backslashes before closing quote */
    return len;
}

/* Write a quoted argument to dst, return pointer past the last written char. */
static char *quote_arg(char *dst, const char *arg)
{
    *dst++ = '"';
    size_t nbs = 0;
    for (const char *p = arg; *p; p++) {
        if (*p == '\\') {
            nbs++;
            *dst++ = '\\';
        } else if (*p == '"') {
            /* Double the backslashes preceding this quote, then escape it. */
            for (size_t i = 0; i < nbs; i++)
                *dst++ = '\\';
            *dst++ = '\\';
            *dst++ = '"';
            nbs = 0;
        } else {
            nbs = 0;
            *dst++ = *p;
        }
    }
    /* Double trailing backslashes before the closing quote. */
    for (size_t i = 0; i < nbs; i++)
        *dst++ = '\\';
    *dst++ = '"';
    return dst;
}

static char *build_cmdline(int argc, char *argv[])
{
    size_t total = 0;
    for (int i = 0; i < argc; i++) {
        size_t q = quoted_len(argv[i]);
        if (total + q < total) /* overflow check */
            return NULL;
        total += q;
    }
    if (total + (size_t)argc < total) /* overflow check */
        return NULL;
    total += (size_t)argc; /* spaces between args + NUL */

    char *cmdline = malloc(total);
    if (!cmdline)
        return NULL;

    char *p = cmdline;
    for (int i = 0; i < argc; i++) {
        if (i > 0)
            *p++ = ' ';
        p = quote_arg(p, argv[i]);
    }
    *p = '\0';
    return cmdline;
}

/* --------------------------------------------------------------------------
 * Real GHC discovery
 *
 * Priority:
 *   1. GHC_THROTTLE_GHC environment variable
 *   2. <self_path> + ".real" suffix (using GetModuleFileName)
 *   3. PATH search with self-exclusion (case-insensitive path comparison)
 * -------------------------------------------------------------------------- */

static int file_exists(const char *path)
{
    DWORD attrs = GetFileAttributesA(path);
    return attrs != INVALID_FILE_ATTRIBUTES &&
           !(attrs & FILE_ATTRIBUTE_DIRECTORY);
}

/* Normalize a path for comparison — resolves to full path. */
static int get_full_path(const char *path, char *buf, size_t bufsz)
{
    DWORD len = GetFullPathNameA(path, (DWORD)bufsz, buf, NULL);
    return (len > 0 && len < (DWORD)bufsz) ? 0 : -1;
}

static const char *search_path_for_ghc(const char *name, const char *self_full)
{
    static char found[PATH_BUF];
    const char *path_env = getenv("PATH");
    if (!path_env)
        return NULL;

    size_t plen = strlen(path_env);
    char *buf = malloc(plen + 1);
    if (!buf)
        return NULL;
    memcpy(buf, path_env, plen + 1);

    /* PATH separator on Windows is ';'.  Use manual tokenization instead
     * of strtok_s which may not be available in MinGW. */
    for (char *p = buf; *p != '\0'; ) {
        /* Find the next ';' or end of string. */
        char *sep = strchr(p, ';');
        if (sep)
            *sep = '\0';

        if (*p != '\0') { /* skip empty entries */
            int n = _snprintf(found, sizeof(found), "%s\\%s", p, name);
            if (n > 0 && (size_t)n < sizeof(found) && file_exists(found)) {
                /* Self-exclusion: compare normalized paths (case-insensitive). */
                char candidate_full[PATH_BUF];
                if (get_full_path(found, candidate_full, sizeof(candidate_full)) == 0 &&
                    _stricmp(candidate_full, self_full) != 0)
                {
                    free(buf);
                    return found;
                }
            }
        }

        if (sep)
            p = sep + 1;
        else
            break;
    }
    free(buf);
    return NULL;
}

static const char *find_real_ghc(const char *argv0)
{
    /* 1. Explicit environment variable. */
    const char *env = getenv("GHC_THROTTLE_GHC");
    if (env && *env) {
        DBG("using GHC_THROTTLE_GHC=%s", env);
        return env;
    }

    /* Resolve our own absolute path using GetModuleFileName. */
    static char self_path[PATH_BUF];
    DWORD len = GetModuleFileNameA(NULL, self_path, sizeof(self_path));
    const char *resolved = (len > 0 && len < sizeof(self_path)) ? self_path : NULL;

    /* 2. <self_path>.real — look for the real binary next to us. */
    static char real_path[PATH_BUF];
    const char *base_for_real = resolved ? resolved : argv0;
    int n = _snprintf(real_path, sizeof(real_path), "%s.real", base_for_real);
    if (n > 0 && (size_t)n < sizeof(real_path) && file_exists(real_path)) {
        DBG("found %s", real_path);
        return real_path;
    }
    /* Also try with .exe.real → .real.exe mapping. */
    {
        size_t blen = strlen(base_for_real);
        if (blen > 4 && _stricmp(base_for_real + blen - 4, ".exe") == 0) {
            n = _snprintf(real_path, sizeof(real_path), "%.*s.real.exe",
                         (int)(blen - 4), base_for_real);
            if (n > 0 && (size_t)n < sizeof(real_path) && file_exists(real_path)) {
                DBG("found %s", real_path);
                return real_path;
            }
        }
    }

    /* 3. PATH search with self-exclusion.
     * Use the resolved path's basename (from GetModuleFileName) since it
     * always includes the .exe suffix.  argv0 may omit it in MSYS/MinGW. */
    if (resolved) {
        char self_full[PATH_BUF];
        if (get_full_path(resolved, self_full, sizeof(self_full)) == 0) {
            const char *base = strrchr(resolved, '\\');
            if (!base)
                base = strrchr(resolved, '/');
            const char *name = base ? base + 1 : resolved;
            const char *found = search_path_for_ghc(name, self_full);
            if (found) {
                DBG("found %s via PATH search", found);
                return found;
            }
        }
    }

    return NULL;
}

/* --------------------------------------------------------------------------
 * main
 * -------------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    if (argc < 1 || !argv[0]) {
        fprintf(stderr, "ghc-throttle: argv[0] is NULL\n");
        return 127;
    }

    const char *dbg = getenv("GHC_THROTTLE_DEBUG");
    if (dbg && dbg[0] == '1')
        debug_enabled = 1;

    /* Detect recursion: if we've already been through ghc-throttle once,
     * the sentinel will be set.  This catches all discovery paths. */
    const char *active = getenv("GHC_THROTTLE_ACTIVE");
    if (active && active[0] == '1') {
        fprintf(stderr,
                "ghc-throttle: recursion detected — the resolved GHC "
                "appears to be ghc-throttle itself.\n");
        return 127;
    }

    const char *real_ghc = find_real_ghc(argv[0]);
    if (!real_ghc) {
        fprintf(stderr,
                "ghc-throttle: cannot find real GHC.\n"
                "  Set GHC_THROTTLE_GHC or install as ghc.exe with ghc.exe.real alongside.\n");
        return 127;
    }

    if (!should_bypass(argc, argv)) {
        int max_jobs = get_max_jobs();
        DBG("max_jobs=%d", max_jobs);
        acquire_slot(max_jobs);
    } else {
        DBG("bypassing throttle (query flag or -jsem)");
    }

    /* Set a sentinel to detect recursion if we accidentally spawn ourselves.
     * If this fails, abort — without the sentinel, a misconfigured real_ghc
     * path could cause infinite recursion. */
    if (!SetEnvironmentVariableA("GHC_THROTTLE_ACTIVE", "1")) {
        fprintf(stderr, "ghc-throttle: SetEnvironmentVariable failed: %lu\n",
                (unsigned long)GetLastError());
        return 127;
    }

    /* Build the command line.  argv[0] is replaced with the real GHC path. */
    argv[0] = (char *)real_ghc;
    char *cmdline = build_cmdline(argc, argv);
    if (!cmdline) {
        fprintf(stderr, "ghc-throttle: out of memory building command line\n");
        if (acquired_mutex) {
            ReleaseMutex(acquired_mutex);
            CloseHandle(acquired_mutex);
        }
        return 127;
    }

    /* Launch the real GHC as a child process.
     * Unlike POSIX exec(), the wrapper stays alive holding the mutex.
     * The mutex is released automatically when this process exits. */
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    DBG("exec: %s", cmdline);

    /* bInheritHandles=TRUE: build systems redirect stdin/stdout/stderr via
     * inheritable handles, so the child must inherit them for piped I/O to
     * work correctly in CI and build system contexts. */
    if (!CreateProcessA(NULL, cmdline, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        fprintf(stderr, "ghc-throttle: CreateProcess failed: %lu\n",
                (unsigned long)GetLastError());
        free(cmdline);
        if (acquired_mutex) {
            ReleaseMutex(acquired_mutex);
            CloseHandle(acquired_mutex);
        }
        return 127;
    }
    free(cmdline);

    /* Wait for GHC to finish.  The mutex is held throughout. */
    DWORD wait_result = WaitForSingleObject(pi.hProcess, INFINITE);
    DWORD exit_code = 127;

    if (wait_result == WAIT_OBJECT_0) {
        if (!GetExitCodeProcess(pi.hProcess, &exit_code))
            exit_code = 1;
    } else {
        fprintf(stderr, "ghc-throttle: WaitForSingleObject failed: %lu\n",
                (unsigned long)GetLastError());
        TerminateProcess(pi.hProcess, 1);
        /* Wait for termination to complete before releasing the mutex,
         * otherwise the concurrency slot is freed while GHC may still
         * be running briefly during teardown. */
        WaitForSingleObject(pi.hProcess, 5000);
        exit_code = 1;
    }

    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    /* Explicitly release and close the mutex for clean teardown. */
    if (acquired_mutex) {
        ReleaseMutex(acquired_mutex);
        CloseHandle(acquired_mutex);
    }

    return (int)exit_code;
}
