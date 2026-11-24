/* Windows process services. These declarations describe the running host,
 * never an object-file target. No replacement handles or unwind types. */
#pragma once
#include "ll_compat.h"
#include "ll_path.h"
#include <windows.h>

typedef struct _ObjectCode ObjectCode;

#include "ll_begin_private.h"

void initLinker_PEi386( ll_linker *L );
void exitLinker_PEi386( void );
const char * addDLL_PEi386( ll_linker *L, const pathchar *dll_name, HINSTANCE *instance  );
pathchar* findSystemLibrary_PEi386( ll_linker *L, pathchar* dll_name );
ll_hs_ptr addLibrarySearchPath_PEi386( ll_linker *L, pathchar* dll_path );
bool removeLibrarySearchPath_PEi386( ll_linker *L, ll_hs_ptr dll_path_index );
ll_symbol_addr *lookupSymbolInDLL_PEi386 (ll_linker *L, const ll_symbol_name* lbl,
                                          HINSTANCE instance, pathchar* dll_name,
                                          ObjectCode *dependent);

pathchar* resolveSymbolAddr_PEi386 ( ll_linker *L, pathchar* buffer, int size,
                                     ll_symbol_addr* symbol, uintptr_t* top );

// MingW-w64 is missing these from the implementation. So we have to look them up
typedef DLL_DIRECTORY_COOKIE(WINAPI *LPAddDLLDirectory)(PCWSTR NewDirectory);
typedef WINBOOL(WINAPI *LPRemoveDLLDirectory)(DLL_DIRECTORY_COOKIE Cookie);

/* A record for storing handles into DLLs. */
typedef
struct _OpenedDLL {
    pathchar*          name;
    struct _OpenedDLL* next;
    HINSTANCE instance;
} OpenedDLL;

#include "ll_end_private.h"
