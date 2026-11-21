/* Prototypes for the MicroHs stage1 tool helpers in mhs_tool_posix.c.
 *
 * MicroHs emits a call for every 'foreign import ccall' into its
 * generated C file, which is compiled as its own translation unit;
 * without these prototypes the implicit declarations are rejected.
 * Include this header via a header-qualified import, e.g.
 *   foreign import ccall "mhs_tool_posix.h mhs_tool_spawn" ...
 */
#ifndef MHS_TOOL_POSIX_H
#define MHS_TOOL_POSIX_H

#include <stdint.h>

int mhs_tool_spawn(const char *prog, char *const *argv,
                   const char *std_in, int std_in_fd,
                   const char *std_out, int std_out_fd,
                   const char *std_err, int std_err_fd,
                   char *const *envp);
int mhs_tool_waitpid(int pid, int *code);
int mhs_tool_handle_fd(void *p);
int mhs_tool_access_x(const char *path);
int mhs_tool_chdir(const char *path);
int64_t mhs_mtime(const char *path);

#endif /* MHS_TOOL_POSIX_H */
