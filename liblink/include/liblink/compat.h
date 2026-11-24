/* Temporary stage-1 interface for the RTS linker compatibility shim. */
#pragma once

#include "ll_autoconf.h"
#include "ll_platform.h"
#include "ll_visibility.h"
#include <stdint.h>
#include <wchar.h>

#if SIZEOF_VOID_P == 8
typedef int64_t ll_compat_int;
#elif SIZEOF_VOID_P == 4
typedef int32_t ll_compat_int;
#else
#error "The linker requires a 32-bit or 64-bit pointer width"
#endif

#if defined(mingw32_HOST_OS)
typedef wchar_t ll_compat_pathchar;
#define LL_COMPAT_PATH_FMT "ls"
#else
typedef char ll_compat_pathchar;
#define LL_COMPAT_PATH_FMT "s"
#endif

typedef enum {
    LL_COMPAT_OBJECT_LOADED,
    LL_COMPAT_OBJECT_NEEDED,
    LL_COMPAT_OBJECT_RESOLVED,
    LL_COMPAT_OBJECT_READY,
    LL_COMPAT_OBJECT_UNLOADED,
    LL_COMPAT_OBJECT_DONT_RESOLVE,
    LL_COMPAT_OBJECT_NOT_LOADED     /* The object was either never loaded or has been
                             fully unloaded */
} ll_compat_ostatus;

LL_FRIEND_API void ll_compat_initLinker (void);
LL_FRIEND_API void ll_compat_initLinker_ (int retain_cafs);
LL_FRIEND_API ll_compat_int ll_compat_insertSymbol(ll_compat_pathchar* obj_name, char* key, void* data);
LL_FRIEND_API void *ll_compat_lookupSymbol( char *lbl );
LL_FRIEND_API ll_compat_ostatus ll_compat_getObjectLoadStatus( ll_compat_pathchar *path );
LL_FRIEND_API ll_compat_int ll_compat_unloadObj( ll_compat_pathchar *path );
LL_FRIEND_API ll_compat_int ll_compat_purgeObj( ll_compat_pathchar *path );
LL_FRIEND_API ll_compat_int ll_compat_loadObj( ll_compat_pathchar *path );
LL_FRIEND_API ll_compat_int ll_compat_loadArchive( ll_compat_pathchar *path );
LL_FRIEND_API ll_compat_int ll_compat_resolveObjs( void );
LL_FRIEND_API void *ll_compat_loadNativeObj( ll_compat_pathchar *path, char **errmsg );
LL_FRIEND_API ll_compat_int ll_compat_unloadNativeObj( void *handle );
LL_FRIEND_API void *ll_compat_lookupSymbolInNativeObj(void *handle, const char *symbol_name);
LL_FRIEND_API const char *ll_compat_addDLL(ll_compat_pathchar* dll_name);
LL_FRIEND_API void * ll_compat_addLibrarySearchPath(ll_compat_pathchar* dll_path);
LL_FRIEND_API ll_compat_int ll_compat_removeLibrarySearchPath(void * dll_path_index);
LL_FRIEND_API void ll_compat_warnMissingKBLibraryPaths( void );
LL_FRIEND_API ll_compat_pathchar* ll_compat_findSystemLibrary(ll_compat_pathchar* dll_name);
