/* Keep the public RTS linker entry points while the implementation lives in liblink. */
#include "Rts.h"
#include "liblink/compat.h"

_Static_assert(sizeof(ll_compat_int) == sizeof(HsInt), "linker HsInt size mismatch");
_Static_assert(_Alignof(ll_compat_int) == _Alignof(HsInt), "linker HsInt alignment mismatch");
_Static_assert(sizeof(ll_compat_int) == sizeof(HsBool), "linker HsBool size mismatch");
_Static_assert(_Alignof(ll_compat_int) == _Alignof(HsBool), "linker HsBool alignment mismatch");
_Static_assert(sizeof(ll_compat_pathchar) == sizeof(pathchar), "linker pathchar size mismatch");
_Static_assert(_Alignof(ll_compat_pathchar) == _Alignof(pathchar), "linker pathchar alignment mismatch");
_Static_assert(sizeof(ll_compat_ostatus) == sizeof(OStatus), "linker OStatus size mismatch");
_Static_assert(_Alignof(ll_compat_ostatus) == _Alignof(OStatus), "linker OStatus alignment mismatch");
_Static_assert(((ll_compat_int)-1 < (ll_compat_int)0) == ((HsInt)-1 < (HsInt)0), "linker HsInt signedness mismatch");
_Static_assert(((ll_compat_int)-1 < (ll_compat_int)0) == ((HsBool)-1 < (HsBool)0), "linker HsBool signedness mismatch");
_Static_assert(((ll_compat_ostatus)-1 < (ll_compat_ostatus)0) == ((OStatus)-1 < (OStatus)0), "linker OStatus signedness mismatch");
_Static_assert((int)LL_COMPAT_OBJECT_LOADED == (int)OBJECT_LOADED, "linker OBJECT_LOADED mismatch");
_Static_assert((int)LL_COMPAT_OBJECT_NEEDED == (int)OBJECT_NEEDED, "linker OBJECT_NEEDED mismatch");
_Static_assert((int)LL_COMPAT_OBJECT_RESOLVED == (int)OBJECT_RESOLVED, "linker OBJECT_RESOLVED mismatch");
_Static_assert((int)LL_COMPAT_OBJECT_READY == (int)OBJECT_READY, "linker OBJECT_READY mismatch");
_Static_assert((int)LL_COMPAT_OBJECT_UNLOADED == (int)OBJECT_UNLOADED, "linker OBJECT_UNLOADED mismatch");
_Static_assert((int)LL_COMPAT_OBJECT_DONT_RESOLVE == (int)OBJECT_DONT_RESOLVE, "linker OBJECT_DONT_RESOLVE mismatch");
_Static_assert((int)LL_COMPAT_OBJECT_NOT_LOADED == (int)OBJECT_NOT_LOADED, "linker OBJECT_NOT_LOADED mismatch");

LL_FRIEND_API void initLinker (void)
{ ll_compat_initLinker(); }

LL_FRIEND_API void initLinker_ (int retain_cafs)
{ ll_compat_initLinker_(retain_cafs); }

LL_FRIEND_API HsInt insertSymbol(pathchar* obj_name, char* key, void* data)
{ return ll_compat_insertSymbol(obj_name, key, data); }

LL_FRIEND_API void *lookupSymbol( char *lbl )
{ return ll_compat_lookupSymbol(lbl); }

LL_FRIEND_API OStatus getObjectLoadStatus( pathchar *path )
{ return (OStatus)ll_compat_getObjectLoadStatus(path); }

LL_FRIEND_API HsInt unloadObj( pathchar *path )
{ return ll_compat_unloadObj(path); }

LL_FRIEND_API HsInt purgeObj( pathchar *path )
{ return ll_compat_purgeObj(path); }

LL_FRIEND_API HsInt loadObj( pathchar *path )
{ return ll_compat_loadObj(path); }

LL_FRIEND_API HsInt loadArchive( pathchar *path )
{ return ll_compat_loadArchive(path); }

LL_FRIEND_API HsInt resolveObjs( void )
{ return ll_compat_resolveObjs(); }

LL_FRIEND_API void *loadNativeObj( pathchar *path, char **errmsg )
{ return ll_compat_loadNativeObj(path, errmsg); }

LL_FRIEND_API HsInt unloadNativeObj( void *handle )
{ return ll_compat_unloadNativeObj(handle); }

LL_FRIEND_API void *lookupSymbolInNativeObj(void *handle, const char *symbol_name)
{ return ll_compat_lookupSymbolInNativeObj(handle, symbol_name); }

LL_FRIEND_API const char *addDLL(pathchar* dll_name)
{ return ll_compat_addDLL(dll_name); }

LL_FRIEND_API HsPtr addLibrarySearchPath(pathchar* dll_path)
{ return ll_compat_addLibrarySearchPath(dll_path); }

LL_FRIEND_API HsBool removeLibrarySearchPath(HsPtr dll_path_index)
{ return ll_compat_removeLibrarySearchPath(dll_path_index); }

LL_FRIEND_API void warnMissingKBLibraryPaths( void )
{ ll_compat_warnMissingKBLibraryPaths(); }

LL_FRIEND_API pathchar* findSystemLibrary(pathchar* dll_name)
{ return ll_compat_findSystemLibrary(dll_name); }
