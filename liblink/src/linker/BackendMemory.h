/* Host operations used by portable object-format implementations. */
#pragma once

#include "ll_compat.h"

#include "ll_begin_private.h"

int ll_backend_open_readonly(const ll_pathchar *path);
void ll_backend_close_file(int fd);

/* Preserve the caller's mapping policy and error message. POSIX failures use
 * LL_E_IO; hosts without file-backed section mappings use LL_E_UNSUPPORTED.
 * The compatibility Windows loader supplies heap images, not mapped files. */
void *ll_backend_map_file_write(ll_linker *L, int fd, size_t bytes, int offset,
                                ll_status *failure_status);
void *ll_backend_map_anon_write(ll_linker *L, size_t bytes);

/* OS result only: the caller retains the existing GOT protection policy.
 * This does not set a linker error or change object failure state. */
bool ll_backend_protect_readonly(void *start, size_t bytes);

#include "ll_end_private.h"
