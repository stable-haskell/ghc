/* Process, handle-fd, access, and signal helpers for the MicroHs stage1
 * tool builds (hsc2hs, ghc-pkg).
 *
 * MicroHs has no `process` or `unix` packages, so spawning is implemented
 * on top of `posix_spawnp(3)` (PATH lookup), waiting via blocking
 * `waitpid(2)`, and handler file descriptors via `fileno(3)` on the
 * `FILE *` inside a MicroHs BFILE (see `mhs_tool_handle_fd`).
 *
 * This file is POSIX-only. It is compiled only into stage1 tool
 * binaries (never into the compiler itself).
 */

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "mhs_tool_posix.h"

#if defined(__APPLE__)
#include <crt_externs.h>
#define MHS_TOOL_ENVIRON (*_NSGetEnviron())
#else
extern char **environ;
#define MHS_TOOL_ENVIRON environ
#endif

/* Redirect one standard stream for the child to be spawned.
 *
 * A non-NULL path redirects the stream to that file (read-only for
 * descriptor 0, write-only otherwise; the files are pre-created by the
 * caller, so no O_CREAT here). Otherwise a non-negative fd is duplicated
 * onto the target descriptor. Otherwise the stream is inherited.
 */
static int
mhs_tool_add_redirect(posix_spawn_file_actions_t *fa, int target,
                      const char *path, int fd)
{
  if (path != NULL) {
    int flags = (target == 0) ? O_RDONLY : O_WRONLY;
    return posix_spawn_file_actions_addopen(fa, target, path, flags, 0600);
  }
  if (fd >= 0) {
    return posix_spawn_file_actions_adddup2(fa, target, fd);
  }
  return 0;
}

/* Spawn `prog` (looked up along PATH) with the given argument vector.
 * `argv[0]` must be the program name. `envp` is NULL to inherit the
 * parent environment. Returns the child pid, or -1 with errno set.
 */
int
mhs_tool_spawn(const char *prog, char *const *argv,
               const char *std_in, int std_in_fd,
               const char *std_out, int std_out_fd,
               const char *std_err, int std_err_fd,
               char *const *envp)
{
  posix_spawn_file_actions_t fa;
  pid_t pid = (pid_t) -1;
  int rc;

  if (prog == NULL || argv == NULL) {
    errno = EINVAL;
    return -1;
  }
  rc = posix_spawn_file_actions_init(&fa);
  if (rc != 0) {
    errno = rc;
    return -1;
  }
  rc = mhs_tool_add_redirect(&fa, 0, std_in, std_in_fd);
  if (rc == 0) {
    rc = mhs_tool_add_redirect(&fa, 1, std_out, std_out_fd);
  }
  if (rc == 0) {
    rc = mhs_tool_add_redirect(&fa, 2, std_err, std_err_fd);
  }
  if (rc == 0) {
    if (envp == NULL) {
      rc = posix_spawnp(&pid, prog, &fa, NULL, argv, MHS_TOOL_ENVIRON);
    } else {
      rc = posix_spawnp(&pid, prog, &fa, NULL, argv, envp);
    }
  }
  posix_spawn_file_actions_destroy(&fa);
  if (rc != 0) {
    errno = rc;
    return -1;
  }
  return (int) pid;
}

/* Blocking wait for `pid`. On success stores the exit code
 * (WEXITSTATUS, or 128 + signal number when killed by a signal)
 * in `*code` and returns 0; returns -1 with errno set on failure.
 */
int
mhs_tool_waitpid(int pid, int *code)
{
  int status;
  pid_t r;

  if (code == NULL) {
    errno = EINVAL;
    return -1;
  }
  for (;;) {
    r = waitpid((pid_t) pid, &status, 0);
    if (r < 0) {
      if (errno == EINTR) {
        continue;
      }
      return -1;
    }
    break;
  }
  if (WIFEXITED(status)) {
    *code = WEXITSTATUS(status);
  } else if (WIFSIGNALED(status)) {
    *code = 128 + WTERMSIG(status);
  } else {
    *code = 127;
  }
  return 0;
}

/* MicroHs runtime handle layout (see src/runtime/bfile.c in MicroHs
 * 0.16.12.0): the method table starts the BFILE struct. A text-mode
 * handle carries the UTF-8 transducer layer on top of the raw file
 * layer; unwrapping one level reaches the FILE * holding the OS fd.
 */
struct mhs_tool_bfile_mets {
  int (*getb)(void *);
  void (*ungetb)(int, void *);
  void (*putb)(int, void *);
  void (*flushb)(void *);
  void (*closeb)(void *);
  size_t (*readb)(uint8_t *, size_t, void *);
  size_t (*writeb)(const uint8_t *, size_t, void *);
  int (*isattyb)(void *);
  int64_t (*filesizeb)(void *);
};

struct mhs_tool_bfile_file {
  struct mhs_tool_bfile_mets mets;
  FILE *file;
};

struct mhs_tool_bfile_utf8 {
  struct mhs_tool_bfile_mets mets;
  struct mhs_tool_bfile_mets *bfile;
  int unget;
  uint8_t pend[3];
  int npend;
};

extern int getb_utf8(void *);
extern int getb_file(void *);

/* Return the OS file descriptor for a MicroHs handle's BFILE pointer
 * (as passed to `withHandleAny`), or -1 when it cannot be determined.
 */
int
mhs_tool_handle_fd(void *p)
{
  struct mhs_tool_bfile_mets *bp = (struct mhs_tool_bfile_mets *) p;
  FILE *f;

  if (bp == NULL) {
    errno = EINVAL;
    return -1;
  }
  if (bp->getb == getb_utf8) {
    struct mhs_tool_bfile_utf8 *u = (struct mhs_tool_bfile_utf8 *) bp;
    bp = u->bfile;
  }
  if (bp == NULL || bp->getb != getb_file) {
    errno = ENOTSUP;
    return -1;
  }
  f = ((struct mhs_tool_bfile_file *) bp)->file;
  if (f == NULL) {
    errno = EBADF;
    return -1;
  }
  return fileno(f);
}

/* Nonzero when `path` exists and is executable (X_OK), zero otherwise. */
int
mhs_tool_access_x(const char *path)
{
  if (path == NULL) {
    return 0;
  }
  return access(path, X_OK) == 0 ? 1 : 0;
}

/* Change the working directory. Returns 0 on success, -1 with errno
 * set on failure.
 */
int
mhs_tool_chdir(const char *path)
{
  if (path == NULL) {
    errno = EINVAL;
    return -1;
  }
  return chdir(path);
}
