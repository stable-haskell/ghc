/* Private spellings keep the copied linker bodies unchanged. */
#pragma once

#if !defined(LL_INTERNAL_SOURCE)
#error "Linker compatibility aliases are private to linker sources"
#endif

#define pathchar ll_compat_pathchar
#define PATH_FMT LL_COMPAT_PATH_FMT
#define OStatus ll_compat_ostatus
#define OBJECT_LOADED LL_COMPAT_OBJECT_LOADED
#define OBJECT_NEEDED LL_COMPAT_OBJECT_NEEDED
#define OBJECT_RESOLVED LL_COMPAT_OBJECT_RESOLVED
#define OBJECT_READY LL_COMPAT_OBJECT_READY
#define OBJECT_UNLOADED LL_COMPAT_OBJECT_UNLOADED
#define OBJECT_DONT_RESOLVE LL_COMPAT_OBJECT_DONT_RESOLVE
#define OBJECT_NOT_LOADED LL_COMPAT_OBJECT_NOT_LOADED

#define initLinker ll_compat_initLinker
#define initLinker_ ll_compat_initLinker_
#define insertSymbol ll_compat_insertSymbol
#define lookupSymbol ll_compat_lookupSymbol
#define getObjectLoadStatus ll_compat_getObjectLoadStatus
#define unloadObj ll_compat_unloadObj
#define purgeObj ll_compat_purgeObj
#define loadObj ll_compat_loadObj
#define loadArchive ll_compat_loadArchive
#define resolveObjs ll_compat_resolveObjs
#define loadNativeObj ll_compat_loadNativeObj
#define unloadNativeObj ll_compat_unloadNativeObj
#define lookupSymbolInNativeObj ll_compat_lookupSymbolInNativeObj
#define addDLL ll_compat_addDLL
#define addLibrarySearchPath ll_compat_addLibrarySearchPath
#define removeLibrarySearchPath ll_compat_removeLibrarySearchPath
#define warnMissingKBLibraryPaths ll_compat_warnMissingKBLibraryPaths
#define findSystemLibrary ll_compat_findSystemLibrary
