/* Modification time and touch for the MicroHs stage1 bootstrap.
   Linked only when a stage1 binary is linked; typechecking does not need it. */
#if !defined(_POSIX_C_SOURCE) || _POSIX_C_SOURCE < 200809L
#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif

#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/stat.h>
#include <unistd.h>

#include "mhs_posix.h"

int64_t
mhs_mtime(const char *path)
{
  struct stat st;
  if (path == NULL)
    return -1;
  if (stat(path, &st) != 0)
    return -1;
  return (int64_t) st.st_mtime;
}

intptr_t
mhs_touch(const char *path)
{
  if (path == NULL)
    return -1;
  if (utimensat(AT_FDCWD, path, NULL, 0) == 0)
    return 0;
  if (errno != ENOENT)
    return -1;
  {
    int fd = open(path, O_WRONLY | O_CREAT, 0666);
    intptr_t r;
    if (fd < 0)
      return -1;
    if (futimens(fd, NULL) != 0)
      r = -1;
    else
      r = 0;
    if (close(fd) != 0)
      r = -1;
    return r;
  }
}
