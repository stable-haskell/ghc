#ifndef MHS_POSIX_H
#define MHS_POSIX_H
#include <stdint.h>
int64_t mhs_mtime(const char *path);
intptr_t mhs_touch(const char *path);
#endif
