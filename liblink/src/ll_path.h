#pragma once
#include "ll_compat.h"
#include "rts/Linker.h"
#define pathdup ll_path_dup
#define pathdir ll_path_dir
#define mkPath ll_path_make
#define endsWithPath ll_path_ends_with

/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2016
 *
 * Platform-independent path manipulation utilities
 *
 * --------------------------------------------------------------------------*/

#if defined(LL_INTERNAL_SOURCE)
// Use wchar_t for pathnames on Windows (#5697)
#if defined(mingw32_HOST_OS)
#include "ll_fs.h"

#define pathcmp wcscmp
#define pathlen wcslen
// N.B. Use the Win32-based file routines from utils/fs.
#define pathopen FS(fwopen)
#define pathstat FS(_wstat)
#define struct_stat struct _stat
#define open wopen
#define WSTR(s) L##s
#define pathprintf snwprintf
#define pathcopy wcscpy
#define pathsize sizeof(wchar_t)
#else
#define pathcmp strcmp
#define pathlen strlen
#define pathopen fopen
#define pathstat stat
#define struct_stat struct stat
#define WSTR(s) s
#define pathprintf snprintf
#define pathsize sizeof(char)
#define pathcopy strcpy
#endif

#endif

#include "ll_begin_private.h"

pathchar* pathdup(const pathchar *path);
pathchar* pathdir(const pathchar *path);
pathchar* mkPath(const char* path);
ll_hs_bool endsWithPath(const pathchar* base, const pathchar* str);

#include "ll_end_private.h"

#if !defined(LL_INTERNAL_SOURCE)
#undef pathdup
#undef pathdir
#undef mkPath
#undef endsWithPath
#endif
