#define LL_INTERNAL_SOURCE 1
/* Windows process integration for the portable COFF object backend.
 * SDK loaded-module records, handles and calling conventions stay here. */
#include "ll_compat.h"
#include "ll_target.h"
#include "LinkerInternals.h"
#include "linker/PEi386.h"
#include "linker/PEi386Services.h"
#include "ll_failure.h"
#include "ll_noexec.h"
#include "RtsSymbolInfo.h"

#if defined(mingw32_HOST_OS)
#include "linker/PEi386Host.h"
#include <shfolder.h>
#include <wchar.h>
#include <dbghelp.h>
#include <psapi.h>
#include <stdlib.h>
#include <string.h>

static void addDLLHandle(ll_linker *L, const pathchar *name, HINSTANCE instance);
static bool checkIfDllLoaded(ll_linker *L, HINSTANCE instance);

/*
 * Note [Avoiding repeated DLL loading]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * As LoadLibraryEx tends to be expensive and addDLL_PEi386 is called on every
 * DLL-imported symbol, we use a hash-map to keep track of which DLLs have
 * already been loaded. This hash-map is keyed on the dll_name passed to
 * addDLL_PEi386 and is mapped to its HINSTANCE. This serves as a quick check
 * to avoid repeated calls to LoadLibraryEx for the identical DLL. See #26009.
 */

static void initLoadedDllCache(ll_linker *L, LoadedDllCache *cache) {
    cache->hash = allocHashTable(L);
}

static int hash_path(const HashTable *table, ll_word w)
{
    const pathchar *key = (pathchar*) w;
    return hashBuffer(table, key, sizeof(pathchar) * wcslen(key));
}

static int compare_path(ll_word key1, ll_word key2)
{
    return wcscmp((pathchar*) key1, (pathchar*) key2) == 0;
}

static void addLoadedDll(ll_linker *L, LoadedDllCache *cache,
                         const pathchar *dll_name, HINSTANCE instance)
{
    // dll_name might be deallocated, we need to copy it to have a stable reference to the contents
    // See #26613
    size_t size = wcslen(dll_name) + 1;
    pathchar* dll_name_copy = LL_MALLOC_FOR(L, size * sizeof(pathchar), "addLoadedDll");
    wcsncpy(dll_name_copy, dll_name, size);
    insertHashTable_(cache->hash, (ll_word) dll_name_copy, instance, hash_path);
}

static HINSTANCE isDllLoaded(const LoadedDllCache *cache, const pathchar *dll_name)
{
    void *result = lookupHashTable_(cache->hash, (ll_word) dll_name, hash_path, compare_path);
    return (HINSTANCE) result;
}

void initLinker_PEi386(ll_linker *L)
{
    initLoadedDllCache(L, &L->loaded_dll_cache);

    if (!ghciInsertSymbolTable(L,
                               WSTR("(GHCi/Ld special symbols)"), "__image_base__",
                               GetModuleHandleW (NULL), LL_HS_BOOL_TRUE,
                               LL_SYM_CODE, 0, NULL)) {
        LL_FATAL_FOR(L, "ghciInsertSymbolTable failed");
    }

#if defined(mingw32_HOST_OS)
    addDLLHandle(L, WSTR("*.exe"), GetModuleHandle(NULL));
#endif

    /* Register the cleanup routine as an exit handler,  this gives other exit handlers
     * a chance to run which may need linker information.  Exit handlers are ran in
     * reverse registration order so this needs to be before the linker loads anything.
     */
    atexit (exitLinker_PEi386);
}

void exitLinker_PEi386(void)
{
}

/* Adds a DLL instance to the list of DLLs in which to search for symbols. */
static void addDLLHandle(ll_linker *L, const pathchar* dll_name, HINSTANCE instance) {

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "addDLLHandle(%" PATH_FMT ")...\n", dll_name));
    /* At this point, we actually know what was loaded.
       So bail out if it's already been loaded.  */
    if (checkIfDllLoaded(L, instance))
    {
        LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "already loaded: addDLLHandle(%" PATH_FMT ")\n", dll_name));
        return;
    }

    OpenedDLL* o_dll;
    o_dll           = LL_MALLOC_FOR(L, sizeof(OpenedDLL), "addDLLHandle" );
    o_dll->name     = dll_name ? pathdup(L, dll_name) : NULL;
    o_dll->instance = instance;
    o_dll->next     = L->opened_dlls;
    L->opened_dlls  = o_dll;

    /* Now discover the dependencies of dll_name that were
       just loaded in our process space. The reason is we have access to them
       without the user having to explicitly specify them.  */
    PIMAGE_NT_HEADERS header =
        (PIMAGE_NT_HEADERS)((BYTE *)instance +
         ((PIMAGE_DOS_HEADER)instance)->e_lfanew);
    PIMAGE_IMPORT_DESCRIPTOR imports =
        (PIMAGE_IMPORT_DESCRIPTOR)((BYTE *)instance + header->
        OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);

    bool importTableMissing =
        header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].Size == 0;

    if (importTableMissing) {
        return;
    }

    /* Ignore these compatibility shims.  */
    const pathchar* ms_dll = WSTR("api-ms-win-");
    const int len = wcslen(ms_dll);

    do {
        pathchar* module = mkPath(L,
                                  (char*)(BYTE *)instance + imports->Name);
        HINSTANCE module_instance = GetModuleHandleW(module);
        if (0 != wcsncmp(module, ms_dll, len)
            && module_instance
            && !checkIfDllLoaded(L, module_instance))
        {
            LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "Loading dependency %" PATH_FMT " -> %" PATH_FMT ".\n", dll_name, module));
            /* Now recursively load dependencies too.  */
            addDLLHandle(L, module, module_instance);
        }
        LL_FREE_FOR(L, module);
        imports++;
    } while (imports->Name);
    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "done: addDLLHandle(%" PATH_FMT ")\n", dll_name));
}

static OpenedDLL* findLoadedDll(ll_linker *L, HINSTANCE instance)
{
    for (OpenedDLL* o_dll = L->opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
        if (o_dll->instance == instance)
        {
            return o_dll;
        }
    }

    return NULL;
}

static bool checkIfDllLoaded(ll_linker *L, HINSTANCE instance)
{
    return findLoadedDll (L, instance) != NULL;
}

const char *
addDLL_PEi386( ll_linker *L, const pathchar *dll_name, HINSTANCE *loaded )
{
    /* ------------------- Win32 DLL loader ------------------- */
    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "addDLL; dll_name = `%" PATH_FMT "'\n", dll_name));

    // See Note [Avoiding repeated DLL loading]
    HINSTANCE instance = isDllLoaded(&L->loaded_dll_cache, dll_name);
    if (instance) {
        if (loaded) {
            *loaded = instance;
        }
        return NULL;
    }

    /* The file name has no suffix (yet) so that we can try
       both foo.dll and foo.drv

      The documentation for LoadLibrary says:
        If no file name extension is specified in the lpFileName
        parameter, the default library extension .dll is
        appended. However, the file name string can include a trailing
        point character (.) to indicate that the module name has no
        extension. */

    size_t bufsize = pathlen(dll_name) + 10;
    pathchar *buf = LL_MALLOC_FOR(L, bufsize * sizeof(wchar_t), "addDLL");

    /* These are ordered by probability of success and order we'd like them.  */
    const wchar_t *formats[] = { L"%ls.DLL", L"%ls.DRV", L"lib%ls.DLL", L"%ls" };
    const DWORD flags[] = { LOAD_LIBRARY_SEARCH_USER_DIRS | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS, 0 };

    /* Iterate through the possible flags and formats.  */
    for (int cFlag = 0; cFlag < 2; cFlag++) {
        for (int cFormat = 0; cFormat < 4; cFormat++) {
            snwprintf(buf, bufsize, formats[cFormat], dll_name);
            instance = LoadLibraryExW(buf, NULL, flags[cFlag]);
            if (instance == NULL) {
                if (GetLastError() != ERROR_MOD_NOT_FOUND) {
                    goto error;
                }
            } else {
                goto loaded_ok; /* We're done. DLL has been loaded.  */
            }
        }
    }

    // We failed to load
    goto error;

loaded_ok:
    addLoadedDll(L, &L->loaded_dll_cache, dll_name, instance);
    addDLLHandle(L, buf, instance);
    if (loaded) {
        *loaded = instance;
    }
    LL_FREE_FOR(L, buf);

    return NULL;

error:
    LL_FREE_FOR(L, buf);

    char* errormsg = LL_MALLOC_FOR(L, sizeof(char) * 80, "addDLL_PEi386");
    if (loaded) *loaded = NULL;
    snprintf(errormsg, 80, "addDLL: %" PATH_FMT " or dependencies not loaded. (Win32 error %lu)", dll_name, GetLastError());
    /* LoadLibrary failed; return a ptr to the error msg. */
    return errormsg;
}

pathchar* findSystemLibrary_PEi386( ll_linker *L, pathchar* dll_name )
{
    const unsigned int init_buf_size = 1024;
    unsigned int bufsize             = init_buf_size;
    wchar_t* result = LL_MALLOC_FOR(L, sizeof(wchar_t) * bufsize, "findSystemLibrary_PEi386");
    DWORD wResult   = SearchPathW(NULL, dll_name, NULL, bufsize, result, NULL);

    if (wResult > bufsize) {
        result  = LL_REALLOC_FOR(L, result, sizeof(wchar_t) * wResult, "findSystemLibrary_PEi386");
        wResult = SearchPathW(NULL, dll_name, NULL, wResult, result, NULL);
    }


    if (!wResult) {
        LL_FREE_FOR(L, result);
        return NULL;
    }

    return result;
}

ll_hs_ptr addLibrarySearchPath_PEi386(ll_linker *L, pathchar* dll_path)
{
    // Make sure the path is an absolute path in UNC-style to ensure that we
    // aren't subject to the MAX_PATH restriction. See #21059.
    wchar_t *abs_path = __rts_create_device_name(dll_path);

    ll_hs_ptr result = AddDllDirectory(abs_path);
    if (!result) {
        LL_LOG_SYSERROR_FOR(L, "addLibrarySearchPath: %" PATH_FMT " (Win32 error %lu)", abs_path, GetLastError());
        free(abs_path);
        return NULL;
    }

    free(abs_path);
    return result;
}

bool removeLibrarySearchPath_PEi386(ll_linker *L, ll_hs_ptr dll_path_index)
{
    bool result = false;

    if (dll_path_index != NULL) {
        result = RemoveDllDirectory(dll_path_index);
        // dll_path_index is now invalid, do not use it after this point.

        if (!result) {
            LL_LOG_SYSERROR_FOR(L, "removeLibrarySearchPath: (Win32 error %lu)", GetLastError());
            return false;
        }
    }

    return !result;
}


/* -----------------------
 * This loads import libraries following Microsoft's official standard in the PE
 * documentation. This is a smaller more efficient format which is just a list
 * of symbol name => dll.
 *
 * This function must fail gracefully and if it does, the filestream needs to
 * be reset to what it was when the function was called.
 */
bool checkAndLoadImportLibrary( ll_linker *L, pathchar* arch_name, char* member_name, FILE* f )
{
    char* image;
    if (L->load_dll_warn) { return 0; }

    /* Based on Import Library specification. PE Spec section 7.1 */

    COFF_import_header hdr;
    size_t n;

    n = fread(&hdr, 1, sizeof_COFF_import_Header, f);
    if (n != sizeof_COFF_import_Header)  {
        LL_LOG_ERROR_FOR(L, "loadImportLibrary: error whilst reading `%s' header "
                   "in `%" PATH_FMT "'\n",
                   member_name, arch_name);
        fseek(f, -(long int)sizeof_COFF_import_Header, SEEK_CUR);
        return false;
    }

    if (   hdr.Sig1 != LL_COFF_IMAGE_FILE_MACHINE_UNKNOWN
        || hdr.Sig2 != LL_COFF_IMPORT_OBJECT_HDR_SIG2
        || getObjectType (L, (char*)&hdr, arch_name) != COFF_IMPORT_LIB) {
        fseek(f, -(long int)sizeof_COFF_import_Header, SEEK_CUR);
        LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "loadArchive: Object `%s` is not an import lib. Skipping...\n", member_name));
        return false;
    }

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "loadArchive: reading %lu bytes at %ld\n", (unsigned long)hdr.SizeOfData, ftell(f)));

    image = LL_MALLOC_FOR(L, hdr.SizeOfData, "checkAndLoadImportLibrary(image)");
    n = fread(image, 1, hdr.SizeOfData, f);
    if (n != hdr.SizeOfData) {
        LL_LOG_ERROR_FOR(L, "loadArchive: error whilst reading `%s' header in `%" PATH_FMT "'. Did not read enough bytes.\n",
            member_name, arch_name);
        fseek(f, -(n + sizeof_COFF_import_Header), SEEK_CUR);
        return false;
    }

    char* symbol  = strtok(image, "\0");
    int symLen    = strlen(symbol) + 1;
    int nameLen   = n - symLen;
    char* dllName = LL_MALLOC_FOR(L, sizeof(char) * nameLen,
                                   "checkAndLoadImportLibrary(dllname)");
    dllName       = strncpy(dllName, image + symLen, nameLen);
    pathchar* dll = LL_MALLOC_FOR(L, sizeof(wchar_t) * nameLen,
                                   "checkAndLoadImportLibrary(dll)");
    mbstowcs(dll, dllName, nameLen);
    LL_FREE_FOR(L, dllName);

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "loadArchive: read symbol %s from lib `%" PATH_FMT "'\n", symbol, dll));
    // We must call `addDLL_PEi386` directly rather than `addDLL` because `addDLL`
    // is now a wrapper around `loadNativeObj` which acquires a lock which we
    // already have here.
    HINSTANCE instance;
    const char* result = addDLL_PEi386(L, dll, &instance);

    LL_FREE_FOR(L, image);

    if (result != NULL) {
        LL_LOG_ERROR_FOR(L, "Could not load `%" PATH_FMT "'. Reason: %s\n", dll, result);
        L->load_dll_warn = true;

        LL_FREE_FOR(L, dll);
        fseek(f, -(n + sizeof_COFF_import_Header), SEEK_CUR);
        return false;
    }

    LL_FREE_FOR(L, dll);

    // See Note [Windows API Set]
    // We must immediately tie the symbol to the shared library.  The easiest
    // way is to load the symbol immediately. We already have all the
    // information so might as well
    ll_symbol_addr* sym = lookupSymbolInDLL_PEi386 (L, symbol,
                                                    instance, dll, NULL);

    // Could be an import descriptor etc, skip if no symbol.
    if (!sym)
      return true;

    // The symbol must have been found, and we can add it to the RTS symbol table
    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "checkAndLoadImportLibrary: resolved symbol %s to %p\n", symbol, sym));
    // Because the symbol has been loaded before we actually need it, if a
    // stronger reference wants to add a duplicate we should discard this
    // one to preserve link order.
    ll_sym_type symType = LL_SYM_DUP_DISCARD | LL_SYM_HIDDEN;
    symType |= hdr.Type == LL_COFF_IMPORT_OBJECT_CODE ? LL_SYM_CODE : LL_SYM_DATA;

    if (!ghciInsertSymbolTable(L, dll, symbol, sym, false, symType, 0, NULL))
      return false;

    return true;
}

ll_symbol_addr*
ll_pe_host_lookup_dlls ( ll_linker *L, const ll_symbol_name* lbl,
                     ObjectCode *dependent )
{
    OpenedDLL* o_dll;
    ll_symbol_addr* res;

    for (o_dll = L->opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
        res = lookupSymbolInDLL_PEi386(L, lbl, o_dll->instance,
                                       o_dll->name, dependent);
        if (res != NULL) return res;
        /* An allocation failure is not a missing definition in this DLL. */
        if (L->error_status != LL_OK) return NULL;
    }
    return NULL;
}

ll_symbol_addr*
lookupSymbolInDLL_PEi386 ( ll_linker *L, const ll_symbol_name* lbl,
                           HINSTANCE instance, pathchar* dll_name LL_UNUSED,
                           ObjectCode *dependent)
{
    ll_symbol_addr* sym;

    LL_ASSERT_FOR(L, dependent == NULL || dependent->L == L);

    /* debugBelch("look in %ls for %s\n", dll_name, lbl); */

    sym = GetProcAddress(instance, lbl);
    if (sym != NULL) {
        /*debugBelch("found %s in %ls\n", lbl, dll_name);*/
        return sym;
    }

    // TODO: Drop this
    /* Ticket #2283.
       Long description: http://support.microsoft.com/kb/132044
       tl;dr:
         If C/C++ compiler sees __declspec(dllimport) ... foo ...
         it generates call *__imp_foo, and __imp_foo here has exactly
         the same semantics as in __imp_foo = GetProcAddress(..., "foo")
     */
    if (sym == NULL && dependent && strncmp (lbl, "__imp_", 6) == 0) {
        sym = GetProcAddress(instance,
                             lbl + 6);
        if (sym != NULL) {
            ll_symbol_addr** indirect = m32_alloc_for(L, dependent->rw_m32, sizeof(ll_symbol_addr*), 8);
            if (indirect == NULL) {
                if (L->error_status == LL_OK)
                    ll_error_set(L, LL_E_NOMEM,
                                 "lookupSymbolInDLLs: could not allocate indirection for %s",
                                 lbl);
                return NULL;
            }
            *indirect = sym;
            LL_IF_DEBUG_FOR(L, linker,
              LL_DEBUG_FOR(L, "warning: %s from %S is linked instead of %s\n",
                         lbl+6, dll_name, lbl));
            return (void*) indirect;
           }
    }

    return NULL;
}

bool ll_pe_host_load_bfd_head(ObjectCode *oc, const char *dllName,
                              ll_symbol_name **symbol, ll_symbol_addr **module)
{
    COFF_HEADER_INFO *info = oc->coff_info->ch_info;
    COFF_symbol *sym;
    ll_symbol_name *sname;
    ll_symbol_addr *addr;
          pathchar* dirName = pathdir(oc->L, oc->fileName);
          LL_IF_DEBUG_FOR(oc->L, linker,
                          LL_DEBUG_FOR(oc->L, "\naddLibrarySearchPath: dll_path = `%" PATH_FMT "'\n", dirName));
          ll_hs_ptr token       = addLibrarySearchPath_PEi386(oc->L, dirName);
          LL_FREE_FOR(oc->L, dirName);

          sym   = &oc->coff_info->symbols[oc->n_symbols-1];
          sname = get_sym_name (oc->L, getSymShortName(oc->L, info, sym), oc);

          LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                   LL_DEBUG_FOR(oc->L, "loading symbol `%s' from dll: '%ls' => `%s'\n",
                              sname, oc->fileName, dllName));

          pathchar* dll = mkPath(oc->L, dllName);
          HINSTANCE dllInstance = 0;
          const char* result = addDLL_PEi386(oc->L, dll, &dllInstance);
          LL_IF_DEBUG_FOR(oc->L, linker,
                          LL_DEBUG_FOR(oc->L, "\nremoveLibrarySearchPath: ptr = `%p'\n", token));
          removeLibrarySearchPath_PEi386(oc->L, token);
          LL_FREE_FOR(oc->L, dll);

          if (result != NULL || dllInstance == 0) {
              LL_LOG_ERROR_FOR(oc->L, "Could not load `%s'. Reason: %s\n",
                         (char*)dllName, result);
              LL_FREE_FOR(oc->L, (void*)result);
              return false;
          }

          /* Set the _dll_iname symbol to the dll's handle.  */
          addr = (ll_symbol_addr*)dllInstance;
          *symbol = sname;
          *module = addr;
          return true;


}
bool ll_pe_host_register_unwind(ObjectCode *oc, unsigned target_arch,
                                unsigned section_index, bool *empty_table)
{
    Section section = oc->sections[section_index];
    *empty_table = false;
    /* SDK records and the registration service describe the running host. */
#if defined(x86_64_HOST_ARCH)
    const unsigned host_arch = LL_ARCH_X86_64;
#elif defined(aarch64_HOST_ARCH)
    const unsigned host_arch = LL_ARCH_AARCH64;
#else
    const unsigned host_arch = LL_ARCH_NATIVE_OTHER;
#endif
    if (target_arch != host_arch) {
        ll_error_set(oc->L, LL_E_UNSUPPORTED,
                     "PE unwind registration requires matching host and target architectures");
        return false;
    }
    oc->coff_info->pdata = &oc->sections[section_index];
#if defined(x86_64_HOST_ARCH)
    unsigned numEntries = section.size / sizeof(RUNTIME_FUNCTION);
    if (numEntries == 0) {
        *empty_table = true;
        return true;
    }

    /* Now register the exception handler for the range and point it
       to the unwind data.  */
    if (!RtlAddFunctionTable (section.start, numEntries, (uintptr_t) GetModuleHandleW(NULL))) {
        const DWORD win_error = GetLastError();
        ll_error_set(oc->L, LL_E_IO,
                     "Unable to register Exception handler for %p for "
                     "section %s in %" PATH_FMT " (Win32 error %lu)",
                     section.start, section.coff_info->name, oc->fileName,
                     (unsigned long)win_error);
        return false;
    }
#else
    /* Preserve the existing native ARM64 path. It records pdata but does
     * not register an x86_64 RUNTIME_FUNCTION array. ARM64 PE relocation
     * support remains subject to the backend's existing rejection. */
    (void)section;
#endif
    return true;
}

void ll_pe_host_unregister_unwind(ObjectCode *oc)
{
    if (!RtlDeleteFunctionTable (oc->coff_info->pdata->start))
      LL_DEBUG_FOR(oc->L, "Unable to remove Exception handlers for %" PATH_FMT "\n",
                  oc->fileName);
}

bool ll_pe_host_image_base(ll_linker *L, uintptr_t *base)
{
    *base = ll_noexec_enabled(L) ? L->services.cfg.noexec_base
                                 : (uintptr_t)GetModuleHandleW(NULL);
    return true;
}

ll_symbol_addr *ll_pe_host_get_proc(ll_linker *L LL_UNUSED, void *module,
                                   const ll_symbol_name *name)
{
    return GetProcAddress((HMODULE)module, name);
}
/* -----------------------------------------------------------------------------
 * Debugging operations.
 */

typedef struct _SymX { ll_symbol_name* name; uintptr_t loc; } SymX;

static int comp (const void * elem1, const void * elem2)
{
    SymX f = *((SymX*)elem1);
    SymX s = *((SymX*)elem2);
    if (f.loc > s.loc) return  1;
    if (f.loc < s.loc) return -1;
    return 0;
}

pathchar*
resolveSymbolAddr_PEi386 (ll_linker *L, pathchar* buffer, int size,
                          ll_symbol_addr* symbol, uintptr_t* top ){
    SYMBOL_INFO sym;
    ZeroMemory (&sym, sizeof(SYMBOL_INFO));
    sym.MaxNameLen = sizeof(char) * 1024;

    DWORD64 uDisplacement = 0;
    HANDLE hProcess = GetCurrentProcess();
    ObjectCode* obj = NULL;
    uintptr_t start, end;
    *top = 0;

    pathprintf (buffer, size, WSTR("0x%" PRIxPTR), symbol);

    if (SymFromAddr (hProcess, (uintptr_t)symbol, &uDisplacement, &sym))
    {
      /* Try using Windows symbols.  */
      wcscat (buffer, WSTR(" "));
      pathchar* name = mkPath (L, sym.Name);
      wcscat (buffer, name);
      LL_FREE_FOR(L, name);
      if (uDisplacement != 0)
      {
        int64_t displacement = (int64_t)uDisplacement;
        pathchar s_disp[50];
        if (displacement < 0)
          pathprintf ((pathchar*)s_disp, 50, WSTR("-%ld"), -displacement);
        else
          pathprintf ((pathchar*)s_disp, 50, WSTR("+%ld"), displacement);

        wcscat (buffer, s_disp);
      }
    }
    else
    {
      /* Try to calculate from information inside the rts.  */
      uintptr_t loc = (uintptr_t)symbol;
      for (ObjectCode* oc = L->gc.objects; oc; oc = oc->next) {
          for (int i = 0; i < oc->n_sections; i++) {
              Section section = oc->sections[i];
              start = (uintptr_t)section.start;
              end   = start + section.size;
              if (loc > start && loc <= end)
              {
                  wcscat (buffer, WSTR(" "));
                  if (oc->archiveMemberName)
                  {
                      wcscat (buffer, oc->archiveMemberName);
                  }
                  else
                  {
                      wcscat (buffer, oc->fileName);
                  }
                  pathchar s_disp[50];
                  pathprintf (s_disp, 50, WSTR("+0x%" PRIxPTR), loc - start);
                  wcscat (buffer, s_disp);
                  obj = oc;
                  goto exit_loop;
              }
          }
      }

      /* If we managed to make it here, we must not have any symbols nor be
         dealing with code we've linked. The only thing left is an internal
         segfault or one in a dynamic library. So let's enumerate the module
         address space.  */
      HMODULE *hMods = NULL;
      DWORD cbNeeded;
      EnumProcessModules (hProcess, hMods, 0, &cbNeeded);
      hMods = LL_MALLOC_FOR(L, cbNeeded, "resolveSymbolAddr_PEi386");
      if (EnumProcessModules (hProcess, hMods, cbNeeded, &cbNeeded))
      {
        uintptr_t loc = (uintptr_t)symbol;
        MODULEINFO info;
        for (uint32_t i = 0; i < cbNeeded / sizeof(HMODULE); i++) {
           ZeroMemory (&info, sizeof (MODULEINFO));
           if (GetModuleInformation (hProcess, hMods[i], &info,
                                     sizeof(MODULEINFO)))
           {
             uintptr_t start = (uintptr_t)info.lpBaseOfDll;
             uintptr_t end   = start + info.SizeOfImage;
             if (loc >= start && loc < end)
             {
                /* Hoera, finally found some information.  */
                pathchar tmp[MAX_PATH];
                if (GetModuleFileNameExW (hProcess, hMods[i], tmp, MAX_PATH))
                {
                  wcscat (buffer, WSTR(" "));
                  wcscat (buffer, tmp);
                  pathprintf (tmp, MAX_PATH, WSTR("+0x%" PRIxPTR), loc - start);
                  wcscat (buffer, tmp);
                }
                break;
             }
           }
        }
      }

      LL_FREE_FOR(L, hMods);
    }

    /* Finally any file/line number.  */
    IMAGEHLP_LINE64 lineInfo = {0};
    DWORD dwDisplacement = 0;
  exit_loop:
    if (SymGetLineFromAddr64(hProcess, (uintptr_t)symbol, &dwDisplacement,
                             &lineInfo))
    {
      /* Try using Windows symbols.  */
      pathchar s_line[512];
      pathprintf ((pathchar*) s_line, 512, WSTR("   %ls (%lu)"),
                  lineInfo.FileName, lineInfo.LineNumber);
      wcscat (buffer, s_line);
      if (dwDisplacement != 0)
      {
        pathprintf ((pathchar*) s_line, 512, WSTR(" +%lu byte%s"),
                    dwDisplacement,
                    (dwDisplacement == 1 ? WSTR("") : WSTR("s")));
      }
      wcscat (buffer, s_line);
    }
    else if (obj)
    {
      /* Try to calculate from information inside the rts.  */
      SymX* locs = LL_CALLOC_FOR(L, sizeof(SymX), obj->n_symbols,
                                   "resolveSymbolAddr");
      int blanks = 0;
      for (int i = 0; i < obj->n_symbols; i++) {
          ll_symbol_name* sym = obj->symbols[i].name;
          if (sym == NULL)
            {
               blanks++;
               continue;
            }
          RtsSymbolInfo* a = NULL;
          ghciLookupSymbolInfo(L, sym, &a);
          if (a) {
              SymX sx = {0};
              sx.name = sym;
              sx.loc  = (uintptr_t)a->value;
              locs[i] = sx;
          }
      }
      qsort (locs, obj->n_symbols, sizeof (SymX), comp);
      uintptr_t key  = (uintptr_t)symbol;
      SymX* res = NULL;

      for (int x = blanks; x < obj->n_symbols; x++) {
          if (x < (obj->n_symbols -1)) {
              if (locs[x].loc >= key && key < locs[x+1].loc) {
                res = &locs[x];
                break;
              }
          }
          else
          {
              if (locs[x].loc >= key) {
                  res = &locs[x];
                  break;
                }
          }
      }

      if (res) {
          pathchar s_disp[512];
          *top = (uintptr_t)res->loc;
          pathprintf ((pathchar*)s_disp, 512,
                      WSTR("\n\t\t (%s+0x%" PRIxPTR ")"),
                      res->name, res->loc - key);
          wcscat (buffer, s_disp);
      }
      LL_FREE_FOR(L, locs);
    }

    return buffer;
}


#else

/* An object backend can inspect COFF on a non-Windows host. These process
 * services cannot be emulated with object bytes or invented module bases. */
static bool pe_host_unsupported(ll_linker *L, const char *service)
{
    ll_error_set(L, LL_E_UNSUPPORTED, "PE %s requires a Windows host", service);
    return false;
}

bool ll_pe_host_image_base(ll_linker *L, uintptr_t *base)
{
    if (ll_noexec_enabled(L)) {
        *base = L->services.cfg.noexec_base;
        return true;
    }
    return pe_host_unsupported(L, "image-base discovery");
}

ll_symbol_addr *ll_pe_host_lookup_dlls(ll_linker *L,
                                      const ll_symbol_name *name LL_UNUSED,
                                      ObjectCode *dependent LL_UNUSED)
{
    pe_host_unsupported(L, "DLL symbol lookup");
    return NULL;
}

ll_symbol_addr *ll_pe_host_get_proc(ll_linker *L, void *module LL_UNUSED,
                                   const ll_symbol_name *name LL_UNUSED)
{
    pe_host_unsupported(L, "import-symbol resolution");
    return NULL;
}

bool ll_pe_host_load_bfd_head(ObjectCode *oc, const char *dll_name LL_UNUSED,
                              ll_symbol_name **symbol LL_UNUSED,
                              ll_symbol_addr **module LL_UNUSED)
{
    return pe_host_unsupported(oc->L, "import-library loading");
}

bool ll_pe_host_register_unwind(ObjectCode *oc,
                                unsigned target_arch LL_UNUSED,
                                unsigned section_index LL_UNUSED,
                                bool *empty_table LL_UNUSED)
{
    return pe_host_unsupported(oc->L, "unwind registration");
}

void ll_pe_host_unregister_unwind(ObjectCode *oc)
{
    /* A non-Windows service never installs pdata. Reaching this branch with
     * one is a state error, not successful deregistration. */
    pe_host_unsupported(oc->L, "unwind removal");
}

bool checkAndLoadImportLibrary(ll_linker *L, pathchar *archive LL_UNUSED,
                                char *member LL_UNUSED, FILE *file LL_UNUSED)
{
    return pe_host_unsupported(L, "import-library loading");
}

#endif
