/*
 * ghc-throttle: Transparent GHC concurrency limiter
 *
 * Licensed under the Apache License, Version 2.0.
 *
 * A drop-in wrapper that limits the number of concurrent GHC processes using
 * flock()-based slot reservation.  flock() locks survive exec(), so after
 * acquiring a slot the wrapper exec()s the real GHC — zero runtime overhead.
 * When GHC exits (normally, via signal, or crash) the kernel closes the fd
 * and releases the lock automatically.  No cleanup logic required.
 *
 * IMPORTANT: The lock directory MUST be on a local filesystem.  flock()
 * semantics on NFS are emulated via fcntl() and may not survive exec().
 *
 * Deployment:
 *   Option A — rename real ghc to ghc.real, install this as ghc.
 *   Option B — set GHC_THROTTLE_GHC and use --with-compiler.
 *
 * Environment variables:
 *   GHC_THROTTLE_GHC   — path to the real GHC binary
 *   GHC_THROTTLE_JOBS  — max concurrent GHC processes (default: ncpus / 2)
 *   GHC_THROTTLE_DIR   — lock directory (default: /tmp/ghc-throttle-$UID)
 *   GHC_THROTTLE_DEBUG — if set to "1", print diagnostics to stderr
 */

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef __APPLE__
#include <sys/sysctl.h>
#include <mach-o/dyld.h>  /* _NSGetExecutablePath */
#elif defined(__FreeBSD__)
#include <sys/sysctl.h>   /* sysctl, KERN_PROC_PATHNAME */
#endif

/* Maximum number of concurrency slots we ever allow. */
#define MAX_SLOTS 256

/* Size of path buffers. */
#define PATH_BUF 4096

static int debug_enabled = 0;

#define DBG(...) do { \
    if (debug_enabled) { \
        fprintf(stderr, "ghc-throttle[%d]: ", (int)getpid()); \
        fprintf(stderr, __VA_ARGS__); \
        fprintf(stderr, "\n"); \
    } \
} while (0)

/* --------------------------------------------------------------------------
 * CPU count detection
 * -------------------------------------------------------------------------- */

static int get_ncpus(void)
{
#ifdef __APPLE__
    int ncpu = 0;
    size_t len = sizeof(ncpu);
    if (sysctlbyname("hw.ncpu", &ncpu, &len, NULL, 0) == 0 && ncpu > 0)
        return ncpu;
#else
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n > 0)
        return (int)n;
#endif
    return 2; /* safe fallback */
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
 * Lock directory
 * -------------------------------------------------------------------------- */

/* Returns 0 on success, -1 if the lock directory could not be created.
 * On failure the caller should skip acquire_slot() and proceed unthrottled. */
static int get_lock_dir(char *buf, size_t bufsz)
{
    int n;
    const char *env = getenv("GHC_THROTTLE_DIR");
    if (env && *env) {
        n = snprintf(buf, bufsz, "%s", env);
    } else {
        n = snprintf(buf, bufsz, "/tmp/ghc-throttle-%u", (unsigned)getuid());
    }
    if (n < 0 || (size_t)n >= bufsz) {
        fprintf(stderr, "ghc-throttle: lock dir path too long\n");
        return -1;
    }
    /* Ensure the directory exists (mkdir -p equivalent for one level). */
    if (mkdir(buf, 0700) != 0) {
        if (errno != EEXIST) {
            fprintf(stderr, "ghc-throttle: cannot create lock dir %s: %s\n",
                    buf, strerror(errno));
            return -1;
        }
        /* Verify the existing path is a directory we own with safe
         * permissions, not a symlink planted by another user in /tmp. */
        struct stat st;
        if (lstat(buf, &st) != 0 ||
            !S_ISDIR(st.st_mode) ||
            st.st_uid != getuid())
        {
            fprintf(stderr, "ghc-throttle: %s: not a directory owned by us\n",
                    buf);
            return -1;
        }
        /* Reject directories without exactly 0700 permissions to prevent
         * other users from interfering with our lock files. */
        if ((st.st_mode & 07777) != 0700) {
            fprintf(stderr,
                    "ghc-throttle: %s: unsafe permissions %04o (expected 0700)\n",
                    buf, (unsigned)(st.st_mode & 07777));
            return -1;
        }
    }
    return 0;
}

/* --------------------------------------------------------------------------
 * Query flag detection
 *
 * GHC invocations like --version, --info, --numeric-version are instant
 * queries that don't compile anything.  Throttling these is wasteful —
 * build systems call them during configuration and they should not block
 * on compilation slots.
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

/* Check if this invocation should bypass throttling entirely. */
static int should_bypass(int argc, char *argv[])
{
    for (int i = 1; i < argc; i++) {
        /* Stop scanning at end-of-options marker.  Everything after "--"
         * is a positional argument and should not trigger bypass. */
        if (strcmp(argv[i], "--") == 0)
            break;
        /* Query flags — no compilation work. */
        if (is_query_flag(argv[i]))
            return 1;
        /* -jsem — build system already manages concurrency via jobserver.
         * Handles both "-jsem <path>" and "-jsem=<path>" forms. */
        if (strcmp(argv[i], "-jsem") == 0 ||
            strncmp(argv[i], "-jsem=", 6) == 0)
            return 1;
    }
    return 0;
}

/* --------------------------------------------------------------------------
 * Slot acquisition
 *
 * Fast path: non-blocking scan across all N lock files.
 * Slow path: block on slot (pid % N) to distribute waiters.
 * -------------------------------------------------------------------------- */

static void acquire_slot(const char *lock_dir, int max_jobs)
{
    char path[PATH_BUF];
    int fd;

    /* Fast path: try each slot without blocking. */
    for (int i = 0; i < max_jobs; i++) {
        int n = snprintf(path, sizeof(path), "%s/slot.%d", lock_dir, i);
        if (n < 0 || (size_t)n >= sizeof(path))
            continue; /* path truncated, skip this slot */

        /*
         * IMPORTANT: Do NOT add O_CLOEXEC here.  The entire correctness model
         * depends on this fd surviving across exec() so that the flock is held
         * by the real GHC process.  When GHC exits, the kernel closes the fd
         * and releases the lock automatically.
         */
        fd = open(path, O_RDWR | O_CREAT, 0600);
        if (fd < 0)
            continue;
        if (flock(fd, LOCK_EX | LOCK_NB) == 0) {
            DBG("acquired slot %d (fast path)", i);
            return; /* fd stays open, survives exec */
        }
        close(fd);
    }

    /* Slow path: block on a deterministic slot based on PID.
     * Same O_CLOEXEC caveat as fast path — do NOT add it. */
    int slot = (int)(getpid() % max_jobs);
    {
        int n = snprintf(path, sizeof(path), "%s/slot.%d", lock_dir, slot);
        if (n < 0 || (size_t)n >= sizeof(path))
            return; /* path truncated, proceed unthrottled */
    }
    fd = open(path, O_RDWR | O_CREAT, 0600);
    if (fd < 0) {
        fprintf(stderr, "ghc-throttle: cannot open %s: %s\n",
                path, strerror(errno));
        return; /* proceed unthrottled rather than failing the build */
    }

    DBG("blocking on slot %d (slow path)", slot);

    /* Retry flock on EINTR — signals can interrupt the blocking wait. */
    while (flock(fd, LOCK_EX) != 0) {
        if (errno == EINTR)
            continue;
        fprintf(stderr, "ghc-throttle: flock(%s) failed: %s\n",
                path, strerror(errno));
        close(fd);
        return; /* proceed unthrottled */
    }
    DBG("acquired slot %d (slow path)", slot);
    /* fd stays open, survives exec */
}

/* --------------------------------------------------------------------------
 * Real GHC discovery
 *
 * Priority:
 *   1. GHC_THROTTLE_GHC environment variable
 *   2. <self_path> + ".real" suffix (using resolved absolute path)
 *   3. PATH search with self-exclusion (skip entries resolving to same inode)
 * -------------------------------------------------------------------------- */

/* Check that path is a regular executable file (not a directory or device). */
static int is_regular_executable(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode) &&
           access(path, X_OK) == 0;
}

/* Resolve a path to its device + inode for identity comparison. */
static int get_identity(const char *path, dev_t *dev, ino_t *ino)
{
    struct stat st;
    if (stat(path, &st) != 0)
        return -1;
    *dev = st.st_dev;
    *ino = st.st_ino;
    return 0;
}

/* Search PATH for a binary named `name`, skipping entries that resolve to the
 * same binary as ourselves (self_dev:self_ino).  This allows ghc-throttle
 * installed as e.g. "ghc-9.14" to find the real "ghc-9.14" in PATH. */
static const char *search_path_for_ghc(const char *name,
                                       dev_t self_dev, ino_t self_ino)
{
    static char found[PATH_BUF];
    const char *path_env = getenv("PATH");
    if (!path_env)
        return NULL;

    /* Work on a mutable copy of PATH. */
    size_t plen = strlen(path_env);
    char *buf = malloc(plen + 1);
    if (!buf)
        return NULL;
    memcpy(buf, path_env, plen + 1);

    char *saveptr = NULL;
    for (char *dir = strtok_r(buf, ":", &saveptr);
         dir != NULL;
         dir = strtok_r(NULL, ":", &saveptr))
    {
        int n = snprintf(found, sizeof(found), "%s/%s", dir, name);
        if (n < 0 || (size_t)n >= sizeof(found))
            continue; /* path truncated, skip */
        if (access(found, X_OK) != 0)
            continue;
        /* Verify it's a regular file, not a directory (access(X_OK) succeeds
         * on directories where the user has search permission). */
        {
            struct stat cst;
            if (stat(found, &cst) != 0 || !S_ISREG(cst.st_mode))
                continue;
        }
        dev_t d;
        ino_t i;
        if (get_identity(found, &d, &i) == 0 &&
            (d != self_dev || i != self_ino))
        {
            free(buf);
            return found;
        }
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

    /* Resolve our own absolute path early — needed for both the ".real"
     * suffix check and the PATH self-exclusion.  When argv[0] is a bare
     * name (e.g. "ghc"), appending ".real" to it would look for "ghc.real"
     * in CWD instead of next to the actual binary.  Using /proc/self/exe
     * (Linux) or _NSGetExecutablePath (macOS) gives us the true location. */
    static char self_path[PATH_BUF];
    const char *resolved = NULL;
#ifdef __linux__
    {
        ssize_t len = readlink("/proc/self/exe", self_path, sizeof(self_path) - 1);
        /* Reject if readlink filled the entire buffer — the path may be
         * truncated.  PATH_BUF (4096) is typically >= PATH_MAX on
         * supported platforms, but the truncation check is still needed
         * since PATH_MAX is not always a compile-time constant. */
        if (len > 0 && (size_t)len < sizeof(self_path) - 1) {
            self_path[len] = '\0';
            resolved = self_path;
        }
    }
#elif defined(__APPLE__)
    {
        uint32_t self_size = sizeof(self_path);
        if (_NSGetExecutablePath(self_path, &self_size) == 0)
            resolved = self_path;
    }
#elif defined(__FreeBSD__)
    {
        int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
        size_t len = sizeof(self_path);
        if (sysctl(mib, 4, self_path, &len, NULL, 0) == 0 && len > 0)
            resolved = self_path;
    }
#endif

    /* 2. <self_path>.real — look for the real binary next to us.
     * Prefer the resolved absolute path so this works even when argv[0]
     * is a bare name like "ghc" found via PATH. */
    static char real_path[PATH_BUF];
    const char *base_for_real = resolved ? resolved : argv0;
    int n = snprintf(real_path, sizeof(real_path), "%s.real", base_for_real);
    if (n > 0 && (size_t)n < sizeof(real_path) &&
        is_regular_executable(real_path)) {
        DBG("found %s", real_path);
        return real_path;
    }

    /* 3. PATH search with self-exclusion. */
    dev_t self_dev;
    ino_t self_ino;
    int have_self = 0;
    if (resolved)
        have_self = (get_identity(resolved, &self_dev, &self_ino) == 0);
    if (!have_self)
        have_self = (get_identity(argv0, &self_dev, &self_ino) == 0);

    if (have_self) {
        /* Use the basename of argv[0] so ghc-throttle installed as e.g.
         * "ghc-9.14" will search PATH for "ghc-9.14", not just "ghc". */
        const char *base = strrchr(argv0, '/');
        const char *name = base ? base + 1 : argv0;
        const char *found = search_path_for_ghc(name, self_dev, self_ino);
        if (found) {
            DBG("found %s via PATH search", found);
            return found;
        }
    }

    return NULL;
}

/* --------------------------------------------------------------------------
 * main
 * -------------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    /* Guard against pathological exec with empty argv. */
    if (argc < 1 || !argv[0]) {
        fprintf(stderr, "ghc-throttle: argv[0] is NULL\n");
        return 127;
    }

    /* Check debug flag early. */
    const char *dbg = getenv("GHC_THROTTLE_DEBUG");
    if (dbg && dbg[0] == '1')
        debug_enabled = 1;

    /* Detect recursion: if we've already been through ghc-throttle once,
     * the sentinel will be set.  This catches all discovery paths (env var,
     * .real suffix, PATH search) pointing back to this wrapper. */
    const char *active = getenv("GHC_THROTTLE_ACTIVE");
    if (active && active[0] == '1') {
        fprintf(stderr,
                "ghc-throttle: recursion detected — the resolved GHC "
                "appears to be ghc-throttle itself.\n");
        return 127;
    }

    /* Find the real GHC. */
    const char *real_ghc = find_real_ghc(argv[0]);
    if (!real_ghc) {
        fprintf(stderr,
                "ghc-throttle: cannot find real GHC.\n"
                "  Set GHC_THROTTLE_GHC or install as ghc with ghc.real alongside.\n");
        return 127;
    }

    /* Skip throttling for query flags and -jsem invocations. */
    if (!should_bypass(argc, argv)) {
        /* Determine concurrency limit. */
        int max_jobs = get_max_jobs();
        DBG("max_jobs=%d", max_jobs);

        /* Prepare lock directory.  If creation fails, proceed unthrottled
         * rather than spamming errors from acquire_slot. */
        char lock_dir[PATH_BUF];
        if (get_lock_dir(lock_dir, sizeof(lock_dir)) == 0)
            acquire_slot(lock_dir, max_jobs);
    } else {
        DBG("bypassing throttle (query flag or -jsem)");
    }

    /* Set a sentinel to detect recursion if we accidentally exec ourselves.
     * If setenv fails (e.g. ENOMEM), abort — without the sentinel, a
     * misconfigured real_ghc path could cause infinite recursion. */
    if (setenv("GHC_THROTTLE_ACTIVE", "1", 1) != 0) {
        fprintf(stderr, "ghc-throttle: setenv failed: %s\n", strerror(errno));
        return 127;
    }

    /* Become the real GHC.  argv[0] is replaced so GHC sees its real name.
     * Use execv for resolved paths (contains '/'), execvp for bare names
     * (e.g., GHC_THROTTLE_GHC=ghc-9.14 without a path). */
    argv[0] = (char *)real_ghc;
    if (strchr(real_ghc, '/'))
        execv(real_ghc, argv);
    else
        execvp(real_ghc, argv);

    /* exec failed. */
    fprintf(stderr, "ghc-throttle: exec(%s) failed: %s\n",
            real_ghc, strerror(errno));
    return 127;
}
