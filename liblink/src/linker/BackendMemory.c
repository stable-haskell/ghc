/* Keep host SDK operations separate from portable file-format code. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "linker/BackendMemory.h"
#include "linker/MMap.h"
#include "ll_fs.h"

#include <errno.h>
#include <fcntl.h>

#if defined(mingw32_HOST_OS)
#include <windows.h>
#include <io.h>
#include <share.h>
#else
#include <unistd.h>
#include <sys/stat.h>
#endif

int ll_backend_open_readonly(const ll_pathchar *path)
{
#if defined(mingw32_HOST_OS)
    return FS(swopen)(path, _O_RDONLY | _O_BINARY, _SH_DENYNO, 0);
#elif defined(openbsd_HOST_OS)
    return open(path, O_RDONLY, S_IRUSR);
#else
    return open(path, O_RDONLY);
#endif
}

void ll_backend_close_file(int fd)
{
#if defined(mingw32_HOST_OS)
    (void)_close(fd);
#else
    (void)close(fd);
#endif
}

void *ll_backend_map_file_write(ll_linker *L, int fd, size_t bytes, int offset,
                                ll_status *failure_status)
{
#if defined(mingw32_HOST_OS) || !RTS_LINKER_USE_MMAP
    (void)L;
    (void)fd;
    (void)bytes;
    (void)offset;
    *failure_status = LL_E_UNSUPPORTED;
    errno = ENOSYS;
    return NULL;
#else
    *failure_status = LL_E_IO;
    return ll_mmap_for_linker(L, bytes, MEM_READ_WRITE, 0, fd, offset);
#endif
}

void *ll_backend_map_anon_write(ll_linker *L, size_t bytes)
{
#if defined(mingw32_HOST_OS)
    /* The existing host service owns a VirtualAlloc reservation and uses the
     * same VirtualFree disposal path as other SECTION_MMAP allocations. */
    return ll_mmap_anon_for_linker(L, bytes);
#elif RTS_LINKER_USE_MMAP
#if defined(MAP_ANON)
    const uint32_t anonymous = MAP_ANON;
#else
    const uint32_t anonymous = MAP_ANONYMOUS;
#endif
    return ll_mmap_for_linker(L, bytes, MEM_READ_WRITE, anonymous, -1, 0);
#else
    (void)L;
    (void)bytes;
    errno = ENOSYS;
    return NULL;
#endif
}

bool ll_backend_protect_readonly(void *start, size_t bytes)
{
#if defined(mingw32_HOST_OS)
    DWORD previous;
    return bytes == 0 || VirtualProtect(start, bytes, PAGE_READONLY, &previous) != 0;
#elif RTS_LINKER_USE_MMAP
    return mprotect(start, bytes, PROT_READ) == 0;
#else
    (void)start;
    (void)bytes;
    errno = ENOSYS;
    return false;
#endif
}
