/*
 * ghc-throttle-status: Report current ghc-throttle slot usage
 *
 * Licensed under the Apache License, Version 2.0.
 *
 * Scans the lock directory and reports which slots are currently held,
 * and by which PID (where available).
 *
 * Usage:
 *   ghc-throttle-status              — auto-detect lock dir
 *   ghc-throttle-status /path/to/dir — explicit lock dir
 *   ghc-throttle-status --help       — show usage
 */

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef __APPLE__
#include <libproc.h>
#endif

#ifdef __linux__
#include <sys/sysmacros.h>  /* makedev() */
#endif

#define PATH_BUF 4096
#define MAX_SLOTS 256

#ifdef __APPLE__
#include <sys/sysctl.h>
#endif

/* --------------------------------------------------------------------------
 * CPU count and configured capacity
 *
 * The status tool needs to know the configured max_jobs to report total
 * capacity, even when not all slot files exist yet (they are created
 * lazily by ghc-throttle on first use).
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
    return 2;
}

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
 * PID-of-lock-holder detection (best-effort)
 *
 * There is no portable way to get the PID that holds a flock.  On Linux
 * we can use /proc/locks; on macOS we simply report "unknown".
 * -------------------------------------------------------------------------- */

#ifdef __linux__
/* Cached /proc/locks entries for efficient batch lookup.
 * Parsing /proc/locks once avoids repeated full scans when
 * multiple slots are held. */
struct lock_entry {
    dev_t dev;
    unsigned long long ino;
    pid_t pid;
};

static struct lock_entry *lock_cache = NULL;
static int lock_cache_n = 0;

static void load_lock_cache(void)
{
    FILE *fp = fopen("/proc/locks", "r");
    if (!fp)
        return;

    char line[512];
    int cap = 64;
    lock_cache = malloc(sizeof(struct lock_entry) * (size_t)cap);
    if (!lock_cache) { fclose(fp); return; }

    while (fgets(line, sizeof(line), fp)) {
        if (!strstr(line, "FLOCK"))
            continue;

        pid_t pid = 0;
        unsigned int maj = 0, min = 0;
        unsigned long long lock_ino = 0;

        /* Example: "1: FLOCK  ADVISORY  WRITE 12345 08:01:654321 0 EOF"
         * Use %llu for inode to avoid truncation on 32-bit platforms
         * where ino_t may be 64-bit but unsigned long is 32-bit. */
        if (sscanf(line, "%*d: FLOCK %*s %*s %d %x:%x:%llu",
                   &pid, &maj, &min, &lock_ino) == 4) {
            if (lock_cache_n >= cap) {
                cap *= 2;
                struct lock_entry *tmp = realloc(lock_cache,
                    sizeof(struct lock_entry) * (size_t)cap);
                if (!tmp) break;
                lock_cache = tmp;
            }
            lock_cache[lock_cache_n].dev = makedev(maj, min);
            lock_cache[lock_cache_n].ino = lock_ino;
            lock_cache[lock_cache_n].pid = pid;
            lock_cache_n++;
        }
    }
    fclose(fp);
}

static pid_t find_lock_holder_linux(dev_t dev, ino_t ino)
{
    unsigned long long target_ino = (unsigned long long)ino;
    for (int i = 0; i < lock_cache_n; i++) {
        if (lock_cache[i].ino == target_ino && lock_cache[i].dev == dev)
            return lock_cache[i].pid;
    }
    return 0;
}
#endif /* __linux__ */

static pid_t find_lock_holder(const char *path)
{
    struct stat st;
    if (stat(path, &st) != 0)
        return 0;

#ifdef __linux__
    return find_lock_holder_linux(st.st_dev, st.st_ino);
#else
    return 0; /* macOS: no easy way without lsof */
#endif
}

/* Get the command name for a PID (best-effort). */
static int get_proc_name(pid_t pid, char *buf, size_t bufsz)
{
#ifdef __APPLE__
    char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
    if (proc_pidpath(pid, pathbuf, sizeof(pathbuf)) > 0) {
        /* Extract basename. */
        const char *base = strrchr(pathbuf, '/');
        snprintf(buf, bufsz, "%s", base ? base + 1 : pathbuf);
        return 0;
    }
#elif defined(__linux__)
    char cmdline_path[64];
    snprintf(cmdline_path, sizeof(cmdline_path), "/proc/%d/comm", (int)pid);
    FILE *fp = fopen(cmdline_path, "r");
    if (fp) {
        if (fgets(buf, (int)bufsz, fp)) {
            /* Strip trailing newline. */
            size_t len = strlen(buf);
            if (len > 0 && buf[len - 1] == '\n')
                buf[len - 1] = '\0';
            fclose(fp);
            return 0;
        }
        fclose(fp);
    }
#else
    (void)pid;
    (void)buf;
    (void)bufsz;
#endif
    return -1;
}

/* --------------------------------------------------------------------------
 * Lock directory detection
 * -------------------------------------------------------------------------- */

/* Returns 0 on success, -1 if the path was truncated. */
static int get_lock_dir(char *buf, size_t bufsz, const char *arg)
{
    int n;
    if (arg) {
        n = snprintf(buf, bufsz, "%s", arg);
    } else {
        const char *env = getenv("GHC_THROTTLE_DIR");
        if (env && *env)
            n = snprintf(buf, bufsz, "%s", env);
        else
            n = snprintf(buf, bufsz, "/tmp/ghc-throttle-%u", (unsigned)getuid());
    }
    if (n < 0 || (size_t)n >= bufsz) {
        fprintf(stderr, "ghc-throttle-status: lock dir path too long\n");
        return -1;
    }
    return 0;
}

/* --------------------------------------------------------------------------
 * Help
 * -------------------------------------------------------------------------- */

static void print_usage(void)
{
    printf("ghc-throttle-status: Report current ghc-throttle slot usage\n"
           "\n"
           "Usage:\n"
           "  ghc-throttle-status              Show slot status (auto-detect lock dir)\n"
           "  ghc-throttle-status <dir>        Show slot status for given lock dir\n"
           "  ghc-throttle-status --help|-h    Show this help\n"
           "\n"
           "Environment:\n"
           "  GHC_THROTTLE_DIR   Lock directory (default: /tmp/ghc-throttle-$UID)\n");
}

/* --------------------------------------------------------------------------
 * Slot scanning
 * -------------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    /* Handle --help / -h. */
    if (argc > 1 &&
        (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0))
    {
        print_usage();
        return 0;
    }

    char lock_dir[PATH_BUF];
    if (get_lock_dir(lock_dir, sizeof(lock_dir), argc > 1 ? argv[1] : NULL) != 0)
        return 1;

    DIR *dp = opendir(lock_dir);
    if (!dp) {
        if (errno == ENOENT) {
            printf("GHC Throttle: no lock directory at %s (no active sessions)\n",
                   lock_dir);
            return 0;
        }
        fprintf(stderr, "ghc-throttle-status: cannot open %s: %s\n",
                lock_dir, strerror(errno));
        return 1;
    }

#ifdef __linux__
    /* Parse /proc/locks once upfront so lock-holder lookups are O(1). */
    load_lock_cache();
#endif

    /* Collect slot info: we need to know total and held counts. */
    struct slot_info {
        int number;
        int locked;
        pid_t holder;
        char proc_name[128];
    } slots[MAX_SLOTS];
    int nslots = 0;

    struct dirent *ent;
    while ((ent = readdir(dp)) != NULL) {
        /* Parse "slot.NNN" with strtol for overflow safety (the directory
         * may be user-supplied, so filenames are untrusted input). */
        if (strncmp(ent->d_name, "slot.", 5) != 0)
            continue;
        char *end;
        long slot_long = strtol(ent->d_name + 5, &end, 10);
        if (end == ent->d_name + 5 || *end != '\0')
            continue; /* no digits or trailing garbage */
        if (slot_long < 0 || slot_long >= MAX_SLOTS)
            continue; /* ignore out-of-range slot numbers */
        int slot_num = (int)slot_long;
        if (nslots >= MAX_SLOTS)
            break;

        char path[PATH_BUF];
        int pn = snprintf(path, sizeof(path), "%s/%s", lock_dir, ent->d_name);
        if (pn < 0 || (size_t)pn >= sizeof(path))
            continue; /* path truncated, skip */

        /* Verify it's a regular file before opening — in a user-supplied lock
         * directory, a slot entry could be a FIFO or device that would block
         * or cause side effects on open(). */
        {
            struct stat fst;
            if (lstat(path, &fst) != 0 || !S_ISREG(fst.st_mode))
                continue;
        }

        /* Use O_RDWR for the flock probe — some BSD/macOS kernels require
         * a writable fd for LOCK_EX even with LOCK_NB. */
        int fd = open(path, O_RDWR);
        if (fd < 0)
            continue;

        struct slot_info *si = &slots[nslots];
        si->number = slot_num;
        si->proc_name[0] = '\0';

        /* Try non-blocking lock to see if it's held. */
        if (flock(fd, LOCK_EX | LOCK_NB) == 0) {
            /* We got it — slot was free.  Unlock immediately. */
            flock(fd, LOCK_UN);
            si->locked = 0;
            si->holder = 0;
        } else if (errno == EWOULDBLOCK || errno == EAGAIN) {
            /* Slot is held by someone else. */
            si->locked = 1;
            si->holder = find_lock_holder(path);
            if (si->holder > 0)
                get_proc_name(si->holder, si->proc_name, sizeof(si->proc_name));
        } else {
            /* Unexpected flock error (ENOLCK, etc.) — skip this slot. */
            close(fd);
            continue;
        }
        close(fd);
        nslots++;
    }
    closedir(dp);

    /* Sort by slot number for stable output. */
    for (int i = 0; i < nslots - 1; i++)
        for (int j = i + 1; j < nslots; j++)
            if (slots[j].number < slots[i].number) {
                struct slot_info tmp = slots[i];
                slots[i] = slots[j];
                slots[j] = tmp;
            }

    /* Count held slots within the configured range only. */
    int max_jobs = get_max_jobs();
    int held = 0;
    for (int j = 0; j < nslots; j++)
        if (slots[j].locked && slots[j].number < max_jobs)
            held++;
    printf("GHC Throttle: %d/%d slots in use  [%s]\n", held, max_jobs, lock_dir);

    /* Print all configured slots.  For slots with existing lock files, use
     * the probed status; for slots without files yet (lazily created), show
     * as free. */
    for (int s = 0; s < max_jobs; s++) {
        /* Find this slot in the scanned results. */
        struct slot_info *si = NULL;
        for (int j = 0; j < nslots; j++) {
            if (slots[j].number == s) {
                si = &slots[j];
                break;
            }
        }
        if (si && si->locked) {
            if (si->holder > 0) {
                if (si->proc_name[0])
                    printf("  slot.%-3d  locked  (PID %d, %s)\n",
                           s, (int)si->holder, si->proc_name);
                else
                    printf("  slot.%-3d  locked  (PID %d)\n",
                           s, (int)si->holder);
            } else {
                printf("  slot.%-3d  locked\n", s);
            }
        } else {
            printf("  slot.%-3d  free\n", s);
        }
    }
    /* Also show any slots beyond max_jobs that have existing files. */
    for (int j = 0; j < nslots; j++) {
        if (slots[j].number >= max_jobs && slots[j].locked) {
            printf("  slot.%-3d  locked  (beyond configured max)\n",
                   slots[j].number);
        }
    }

#ifdef __linux__
    free(lock_cache);
#endif
    return 0;
}
