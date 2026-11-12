#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_linker.h"
#include "ll_path.h"

#include <string.h>
#include <stddef.h>

#include <libgen.h>
#include <ctype.h>
#if defined(mingw32_HOST_OS)
/* Using Secure APIs */
#define MINGW_HAS_SECURE_API 1
#include <wchar.h>
#endif

pathchar* pathdup(struct ll_linker *L, const pathchar *path)
{
    pathchar *ret;
#if defined(mingw32_HOST_OS)
    size_t path_bytes = (wcslen(path) + 1) * sizeof(*ret);
    ret = LL_MALLOC_FOR(L, path_bytes, "pathdup");
    memcpy(ret, path, path_bytes);
#else
    /* sigh, strdup() isn't a POSIX function, so do it the long way */
    ret = LL_MALLOC_FOR(L, strlen(path)+1, "pathdup" );
    strcpy(ret, path);
#endif
    return ret;
}

pathchar* pathdir(struct ll_linker *L, const pathchar *path)
{
    pathchar *ret;
#if defined(mingw32_HOST_OS)
    pathchar *drive, *dirName;
    size_t memberLen = pathlen(path) + 1;
    dirName = LL_MALLOC_FOR(L, pathsize * memberLen, "pathdir(path)");
    ret     = LL_MALLOC_FOR(L, pathsize * memberLen, "pathdir(path)");
    drive   = LL_MALLOC_FOR(L, pathsize * _MAX_DRIVE, "pathdir(path)");
    _wsplitpath_s(path, drive, _MAX_DRIVE, dirName, pathsize * pathlen(path), NULL, 0, NULL, 0);
    pathprintf(ret, memberLen, WSTR("%" PATH_FMT "%" PATH_FMT), drive, dirName);
    LL_FREE_FOR(L, drive);
    LL_FREE_FOR(L, dirName);
#else
    // N.B. cast is safe as we do not modify dirName
    const pathchar* dirName = dirname((pathchar *) path);
    size_t memberLen  = pathlen(dirName);
    ret = LL_MALLOC_FOR(L, pathsize * (memberLen + 2), "pathdir(path)");
    strcpy(ret, dirName);
    ret[memberLen  ] = '/';
    ret[memberLen+1] = '\0';
#endif
    return ret;
}

pathchar* mkPath(struct ll_linker *L, const char* path)
{
#if defined(mingw32_HOST_OS)
    size_t required = mbstowcs(NULL, path, 0);
    pathchar *ret = LL_MALLOC_FOR(L, sizeof(pathchar) * (required + 1), "mkPath");
    if (mbstowcs(ret, path, required) == (size_t)-1)
    {
        LL_FATAL_FOR(L, "mkPath failed converting char* to wchar_t*");
    }
    ret[required] = '\0';
    return ret;
#else
    return pathdup(L, path);
#endif
}

ll_hs_bool endsWithPath(const pathchar* base, const pathchar* str) {
    int blen = pathlen(base);
    int slen = pathlen(str);
    return (blen >= slen) && (0 == pathcmp(base + blen - slen, str));
}
