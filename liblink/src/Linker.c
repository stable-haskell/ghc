/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#define LL_INTERNAL_SOURCE 1

#include "ll_compat.h"
#include "ll_hash.h"
#include "LinkerInternals.h"
#include "ll_failure.h"
#include "ll_format.h"
#include "RtsSymbolInfo.h"
#include "linker/M32Alloc.h"
#include "linker/CacheFlush.h"
#include "linker/SymbolExtras.h"
#include "linker/MMap.h"
#include "linker/M32Reservations.h"
#include "ll_path.h"

// get protos for is*()
#include <ctype.h>
#include <errno.h>

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#include <fcntl.h>
#include <unistd.h>
#include <inttypes.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ll_fs.h"

#if defined(HAVE_SYS_STAT_H)
#include <sys/stat.h>
#endif

#if defined(HAVE_DLFCN_H)
#include <dlfcn.h>
#endif

#if defined(OBJFORMAT_ELF)
#  include "linker/Elf.h"
#  include <regex.h>    // regex is already used by dlopen() so this is OK
                        // to use here without requiring an additional lib
#elif defined(OBJFORMAT_PEi386)
#  include "linker/PEi386.h"
#  include <windows.h>
#elif defined(OBJFORMAT_MACHO)
#  include "linker/MachO.h"
#  include <regex.h>
#  include <mach/machine.h>
#  include <mach-o/fat.h>
#endif

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
#  include "linker/LoadNativeObjPosix.h"
#endif

#if defined(dragonfly_HOST_OS)
#include <sys/tls.h>
#endif

#define UNUSED(x) (void)(x)

/*
 * Note [iconv and FreeBSD]
 * ~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * On FreeBSD libc.so provides an implementation of the iconv_* family of
 * functions. However, due to their implementation, these symbols cannot be
 * resolved via dlsym(); rather, they can only be resolved using the
 * explicitly-versioned dlvsym().
 *
 * This is problematic for the RTS linker since we may be asked to load
 * an object that depends upon iconv. To handle this we include a set of
 * fallback cases for these functions, allowing us to resolve them to the
 * symbols provided by the libc against which the RTS is linked.
 *
 * See #20354.
 */

#if defined(freebsd_HOST_OS)
extern void iconvctl(void);
extern void iconv_open_into(void);
extern void iconv_open(void);
extern void iconv_close(void);
extern void iconv_canonicalize(void);
extern void iconv(void);
#endif

/*
   Note [runtime-linker-support]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   When adding support for a new platform to the runtime linker please
   update `$TOP/configure.ac` under heading `Does target have runtime
   linker support?`.
 */
/* `symhash` is a Hash table mapping symbol names to RtsSymbolInfo.
   This hashtable will contain information on all symbols
   that we know of, however the .o they are in may not be loaded.

   Until the ObjectCode the symbol belongs to is actually
   loaded this symbol may be replaced. So do not rely on
   addresses of unloaded symbols.

   Note [runtime-linker-phases]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Broadly the behavior of the runtime linker can be
   split into the following five phases:

   - Indexing (e.g. ocVerifyImage and ocGetNames)
   - Initialization (e.g. ocResolve)
   - RunInit (e.g. ocRunInit)
   - Lookup (e.g. lookupSymbol/lookupSymbolInNativeObj)

   This is to enable lazy loading of symbols. Eager loading is problematic
   as it means that all symbols must be available, even those which we will
   never use. This is especially painful on Windows, where the number of
   libraries required to link things like QT or WxWidgets grows to be quite high.

   We proceed through these stages as follows,

   * During Indexing we verify and open the ObjectCode and
     perform a quick scan/indexing of the ObjectCode. All the work
     required to actually load the ObjectCode is done.

     All symbols from the ObjectCode are also inserted into
     `symhash`, where possible duplicates are handled via the semantics
     described in `ghciInsertSymbolTable`.

     This phase will produce ObjectCode with status `OBJECT_LOADED` or `OBJECT_NEEDED`
     depending on whether they are an archive member or not.

   * During initialization we load ObjectCode, perform relocations, execute
     static constructors etc. This phase may trigger other ObjectCodes to
     be loaded because of the calls to lookupSymbol.

     This phase will produce ObjectCode with status `OBJECT_NEEDED` if the
     previous status was `OBJECT_LOADED`.

   * During resolve we attempt to resolve all the symbols needed for the
     initial link. This essentially means, that for any ObjectCode given
     directly to the command-line we perform lookupSymbol on the required
     symbols. lookupSymbol may trigger the loading of additional ObjectCode
     if required. After resolving an object we mark its text as executable and
     not writable.

     This phase will produce ObjectCode with status `OBJECT_RESOLVED` if
     the previous status was `OBJECT_NEEDED`.

   * During RunInit we run the initializers ("constructors") of the objects
     that are in `OBJECT_RESOLVED` state and move them to `OBJECT_READY` state.
     This must be in a separate phase since we must ensure that all needed
     objects have been fully resolved before we can run their initializers.
     This is particularly tricky in the presence of cyclic dependencies (see
     #21253).

   * lookupSymbol is used to lookup any symbols required, both during initial
     link and during statement and expression compilations in the REPL.
     Declaration of e.g. a foreign import, will eventually call lookupSymbol
     which will either fail (symbol unknown) or succeed (and possibly trigger a
     load).

     This phase may transition an ObjectCode from `OBJECT_LOADED` to `OBJECT_RESOLVED`

   When a new scope is introduced (e.g. a new module imported) GHCi does a full re-link
   by calling unloadObj and starting over.
   When a new declaration or statement is performed ultimately lookupSymbol is called
   without doing a re-link.

   The goal of these different phases is to allow the linker to be able to perform
   "lazy loading" of ObjectCode. The reason for this is that we want to only link
   in symbols that are actually required for the link. This reduces:

   1) Dependency chains, if A.o required a .o in libB but A.o isn't required to link
      then we don't need to load libB. This means the dependency chain for libraries
      such as ucrt can be broken down.

   2) The number of duplicate symbols, since now only symbols that are
      true duplicates will display the error.
 */

/* Generic wrapper function to try and resolve oc files */
static int ocTryLoad( ObjectCode* oc );
/* Run initializers */
static int ocRunInit( ObjectCode* oc );
static int runPendingInitializers (ll_linker *L);

static void ghciRemoveSymbolTable(ll_linker *L, const ll_symbol_name* key,
    ObjectCode *owner)
{
    RtsSymbolInfo *pinfo = lookupStrHashTable(L->symhash, key);
    if (!pinfo || owner != pinfo->owner) return;
    removeStrHashTable(L->symhash, key, NULL);
    if (isSymbolImport (owner, key))
      LL_FREE_FOR(L, pinfo->value);

    LL_FREE_FOR(L, pinfo);
}

static const char *
symbolTypeString (ll_linker *L, ll_sym_type type)
{
    switch (type & ~(LL_SYM_DUP_DISCARD | LL_SYM_HIDDEN)) {
        case LL_SYM_CODE: return "code";
        case LL_SYM_DATA: return "data";
        case LL_SYM_INDIRECT_DATA: return "indirect-data";
        default: LL_FATAL_FOR(L, "symbolTypeString: unknown symbol type (%d)", type);
    }
}

/* -----------------------------------------------------------------------------
 * Insert symbols into hash tables, checking for duplicates.
 *
 * Returns: 0 on failure, nonzero on success
 */
/*
 Note [weak-symbols-support]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 While ghciInsertSymbolTable does implement extensive
 logic for weak symbol support, weak symbols are not currently
 fully supported by the RTS. This code is mostly here for COMDAT
 support which uses the weak symbols support.

 Linking weak symbols defined purely in C code with other C code
 should also work, probably. Observing weak symbols in Haskell
 won't.

 Some test have been written for weak symbols but have been disabled
 mostly because it's unsure how the weak symbols support should look.
 See #11223
 */
int ghciInsertSymbolTable(
   ll_linker *L,
   pathchar* obj_name,
   const ll_symbol_name* key,
   ll_symbol_addr* data,
   ll_strength strength,
   ll_sym_type type,
   unsigned long size,
   ObjectCode *owner)
{
   RtsSymbolInfo *pinfo = lookupStrHashTable(L->symhash, key);
   if (!pinfo) /* new entry */
   {
      pinfo = LL_MALLOC_FOR(L, sizeof (*pinfo), "ghciInsertToSymbolTable");
      pinfo->value = data;
      pinfo->owner = owner;
      pinfo->strength = strength;
      pinfo->type = type;
      pinfo->size = size;
      insertStrHashTable(L->symhash, key, pinfo);
      return 1;
   }
   else if (pinfo->type ^ type)
   {
       if(pinfo->type & LL_SYM_HIDDEN)
       {
            /* The existing symbol is hidden, let's replace it */
            pinfo->value = data;
            pinfo->owner = owner;
            pinfo->strength = strength;
            pinfo->type = type;
            pinfo->size = size;
            return 1;
       }
       /* We were asked to discard the symbol on duplicates, do so quietly.  */
       if (!(type & (LL_SYM_DUP_DISCARD | LL_SYM_HIDDEN)))
       {
         LL_DEBUG_FOR(L, "Symbol type mismatch (existing %d, new %d).\n", pinfo->type, type);
         LL_DEBUG_FOR(L, "Symbol %s was defined by %" PATH_FMT " to be a %s symbol.\n",
                    key, obj_name, symbolTypeString(L, type));
         LL_DEBUG_FOR(L, "      yet was defined by %" PATH_FMT " to be a %s symbol.\n",
                    pinfo->owner ? pinfo->owner->fileName : WSTR("<builtin>"),
                    symbolTypeString(L, pinfo->type));
       }
       return 1;
   }
   else if (pinfo->strength == LL_STRENGTH_STRONG)
   {
       /* The existing symbol is strong meaning we must never override it */
       LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "%s is already defined as a strong symbol; ignoring redefinition...", key));
       return 1;
   }
   else if (strength == LL_STRENGTH_WEAK &&
            data &&
            pinfo->strength == LL_STRENGTH_WEAK &&
            !pinfo->value)
   {
       /* The existing symbol is weak with a zero value; replace it with the new symbol. */
       pinfo->value = data;
       pinfo->owner = owner;
       pinfo->size = size;
       return 1;
   }
   else if (strength == LL_STRENGTH_WEAK)
   {
       return 1; /* weak symbol, because the symbol is weak, data = 0 and we
                 already know of another copy throw this one away.

                 or both weak symbols have a nonzero value. Keep the existing one.

                 This also preserves the semantics of linking against
                 the first symbol we find. */
   }
   else if (pinfo->strength == LL_STRENGTH_WEAK && strength != LL_STRENGTH_WEAK) /* weak symbol is in the table */
   {
      /* override the weak definition with the non-weak one */
      pinfo->value = data;
      pinfo->owner = owner;
      pinfo->strength = strength;
      pinfo->size = size;
      return 1;
   }
   else if (  pinfo->owner
           && pinfo->owner->status != OBJECT_READY
           && pinfo->owner->status != OBJECT_RESOLVED
           && pinfo->owner->status != OBJECT_NEEDED)
   {
        /* If the other symbol hasn't been loaded or will be loaded and we want to
           explicitly load the new one, we can just swap it out and load the one
           that has been requested. If not, just keep the first one encountered.

           Because the `symHash' table consists symbols we've also not loaded but
           found during the initial scan this is safe to do. If however the existing
           symbol has been loaded then it means we have a duplicate.

           This is essentially emulating the behavior of a linker wherein it will always
           link in object files that are .o file arguments, but only take object files
           from archives as needed. */
       if (owner && (owner->status == OBJECT_NEEDED
                     || owner->status == OBJECT_RESOLVED
                     || owner->status == OBJECT_READY)) {
           pinfo->value = data;
           pinfo->owner = owner;
           pinfo->strength = strength;
           pinfo->size = size;
       }

       return 1;
    }
    else if (pinfo->owner == owner)
    {
       /* If it's the same symbol, ignore. This makes ghciInsertSymbolTable idempotent */
       return 1;
    }
    else if (owner && owner->status == OBJECT_LOADED)
    {
        /* If the duplicate symbol is just in state OBJECT_LOADED it means we're in discovery of an
           member. It's not a real duplicate yet. If the Oc Becomes OBJECT_NEEDED then ocTryLoad will
           call this function again to trigger the duplicate error. */
        return 1;
    }

   ll_error_set(L, LL_E_DUPSYM,
                "duplicate definition for symbol %s in %" PATH_FMT,
                key, obj_name);
   LL_DEBUG_FOR(L,
      "GHC runtime linker: fatal error: I found a duplicate definition for symbol\n"
      "   %s\n"
      "whilst processing object file\n"
      "   %" PATH_FMT "\n"
      "The symbol was previously defined in\n"
      "   %" PATH_FMT "\n"
      "This could be caused by:\n"
      "   * Loading two different object files which export the same symbol\n"
      "   * Specifying the same object file twice on the GHCi command line\n"
      "   * An incorrect `package.conf' entry, causing some object to be\n"
      "     loaded twice.\n",
      (char*)key,
      obj_name,
      pinfo->owner == NULL ? WSTR("(GHCi built-in symbols)") :
      pinfo->owner->archiveMemberName ? pinfo->owner->archiveMemberName
      : pinfo->owner->fileName
   );

   return 0;
}

/* -----------------------------------------------------------------------------
* Looks up symbols into hash tables.
*
* Returns: 0 on failure and result is not set,
*          nonzero on success and result set to nonzero pointer
*/
ll_hs_bool ghciLookupSymbolInfo(ll_linker *L,
    const ll_symbol_name* key, RtsSymbolInfo **result)
{
    RtsSymbolInfo *pinfo = lookupStrHashTable(L->symhash, key);
    if (!pinfo) {
        *result = NULL;
        return LL_HS_BOOL_FALSE;
    }
    if (pinfo->strength == LL_STRENGTH_WEAK) {
        LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "lookupSymbolInfo: promoting %s\n", key));
        /* Once it's looked up, it can no longer be overridden */
        pinfo->strength = LL_STRENGTH_NORMAL;
    }

    *result = pinfo;
    return LL_HS_BOOL_TRUE;
}

/* -----------------------------------------------------------------------------
 * initialize the object linker
 */


static void insertBuiltin(void *ctx, const char *name, void *addr,
                          int strength, int type)
{
    ll_linker *L = ctx;
    if (! ghciInsertSymbolTable(L, WSTR("(GHCi built-in symbols)"),
                                name, addr, (ll_strength)strength,
                                (ll_sym_type)type, 0, NULL)) {
        LL_FATAL_FOR(L, "ghciInsertSymbolTable failed");
    }
}


void initLinker (void)
{
    // default to retaining CAFs for backwards compatibility.  Most
    // users will want initLinker_(0): otherwise unloadObj() will not
    // be able to unload object files when they contain CAFs.
    initLinker_(1);
}

void
ll_init_linker (ll_linker *L, int retain_cafs)
{
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
    int compileResult;
#endif

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "initLinker: start\n"));

    /* Make initLinker idempotent, so we can call it
       before every relevant operation; that means we
       don't need to initialise the linker separately */
    if (L->linker_init_done == 1) {
        LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "initLinker: idempotent return\n"));
        return;
    } else {
        L->linker_init_done = 1;
    }

    L->gc.mutex = &L->linker_mutex;
    if (L->services.host.init_unload_check != NULL) {
        L->services.host.init_unload_check(L->services.host.ctx);
    }

    ll_mutex_init_for(&L->services, &L->linker_mutex);

    L->symhash = allocStrHashTable(L);

    if (L->services.host.populate_builtins != NULL) {
        L->services.host.populate_builtins(L->services.host.ctx, retain_cafs, insertBuiltin, L);
    }

#   if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
#   if defined(RTLD_DEFAULT)
    L->dl_prog_handle = RTLD_DEFAULT;
#   else
    L->dl_prog_handle = dlopen(NULL, RTLD_LAZY);
#   endif /* RTLD_DEFAULT */

    compileResult = regcomp(&L->re_invalid,
           "(([^ \t()])+\\.so([^ \t:()])*):([ \t])*(invalid ELF header|file too short|invalid file format|Exec format error)",
           REG_EXTENDED);
    if (compileResult != 0) {
        LL_FATAL_FOR(L, "Compiling re_invalid failed");
    }
    compileResult = regcomp(&L->re_realso,
           "(GROUP|INPUT) *\\( *([^ )]+)",
           REG_EXTENDED);
    if (compileResult != 0) {
        LL_FATAL_FOR(L, "Compiling re_realso failed");
    }
#   endif

#if defined(OBJFORMAT_PEi386)
    initLinker_PEi386(L);
#endif

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "initLinker: done\n"));
    return;
}

void
ll_exit_linker(ll_linker *L) {
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
   if (L->linker_init_done == 1) {
      regfree(&L->re_invalid);
      regfree(&L->re_realso);
   }
#endif
   if (L->linker_init_done == 1) {
       ll_hash_free_owned_table((HashTable *)L->symhash);
       if (L->services.host.exit_unload_check != NULL) {
           L->services.host.exit_unload_check(L->services.host.ctx);
       }
   }
#if defined(mingw32_HOST_OS) && defined(NEED_M32)
   /* Exit is not proof that ObjectCode owners were disposed. Only drain the
    * free pool and retain live/failed reservations for a later retry. */
   (void)ll_m32_shutdown_for(L);
#endif
   ll_mutex_close_for(&L->services, &L->linker_mutex);
}

void
initLinker_ (int retain_cafs)
{
    ll_init_linker(&ll_default_linker, retain_cafs);
}

void
exitLinker(void)
{
    ll_exit_linker(&ll_default_linker);
}

/* -----------------------------------------------------------------------------
 *                  Loading DLL or .so dynamic libraries
 * -----------------------------------------------------------------------------
 *
 * Add a DLL from which symbols may be found.  In the ELF case, just
 * do RTLD_GLOBAL-style add, so no further messing around needs to
 * happen in order that symbols in the loaded .so are findable --
 * lookupSymbol() will subsequently see them by dlsym on the program's
 * dl-handle.  Returns NULL if success, otherwise ptr to an err msg.
 *
 * In the PEi386 case, open the DLLs and put handles to them in a
 * linked list.  When looking for a symbol, try all handles in the
 * list.  This means that we need to load even DLLs that are guaranteed
 * to be in the ghc.exe image already, just so we can get a handle
 * to give to loadSymbol, so that we can find the symbols.  For such
 * libraries, the LoadLibrary call should be a no-op except for returning
 * the handle.
 *
 */

#  if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)

/*
  Note [RTLD_LOCAL]
  ~~~~~~~~~~~~~~~~~
  In GHCi we want to be able to override previous .so's with newly
  loaded .so's when we recompile something.  This further implies that
  when we look up a symbol in internal_dlsym() we have to iterate
  through the loaded libraries (in order from most recently loaded to
  oldest) looking up the symbol in each one until we find it.

  However, this can cause problems for some symbols that are copied
  by the linker into the executable image at runtime - see #8935 for a
  lengthy discussion.  To solve that problem we need to look up
  symbols in the main executable *first*, before attempting to look
  them up in the loaded .so's.  But in order to make that work, we
  have to always call dlopen with RTLD_LOCAL, so that the loaded
  libraries don't populate the global symbol table.
*/

static void *
internal_dlsym(ll_linker *L, const char *symbol) {
    void *v;

    // concurrent dl* calls may alter dlerror
    ll_assert_lock_held_for(L, &L->linker_mutex);

    // clears dlerror
    dlerror();

    // look in program first
    v = dlsym(L->dl_prog_handle, symbol);
    if (dlerror() == NULL) {
        LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "internal_dlsym: found symbol '%s' in program\n", symbol));
        return v;
    }

    for (ObjectCode *nc = L->gc.loaded_objects; nc; nc = nc->next_loaded_object) {
        if (nc->type == DYNAMIC_OBJECT) {
          v = dlsym(nc->dlopen_handle, symbol);
          if (dlerror() == NULL) {
            LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "internal_dlsym: found symbol '%s' in shared object\n", symbol));
            return v;
          }
        }
    }

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "internal_dlsym: looking for symbol '%s' in special cases\n", symbol));
#   define SPECIAL_SYMBOL(sym) \
      if (strcmp(symbol, #sym) == 0) return (void*)&sym;

#   if defined(HAVE_SYS_STAT_H) && defined(linux_HOST_OS) && defined(__GLIBC__)
    // HACK: GLIBC implements these functions with a great deal of trickery where
    //       they are either inlined at compile time to their corresponding
    //       __xxxx(SYS_VER, ...) function or direct syscalls, or resolved at
    //       link time via libc_nonshared.a.
    //
    //       We borrow the approach that the LLVM JIT uses to resolve these
    //       symbols. See http://llvm.org/PR274 and #7072 for more info.

    SPECIAL_SYMBOL(stat);
    SPECIAL_SYMBOL(fstat);
    SPECIAL_SYMBOL(lstat);
    SPECIAL_SYMBOL(stat64);
    SPECIAL_SYMBOL(fstat64);
    SPECIAL_SYMBOL(lstat64);
    SPECIAL_SYMBOL(atexit);
    SPECIAL_SYMBOL(mknod);
#   endif

    // See Note [iconv and FreeBSD]
#   if defined(freebsd_HOST_OS)
    SPECIAL_SYMBOL(iconvctl);
    SPECIAL_SYMBOL(iconv_open_into);
    SPECIAL_SYMBOL(iconv_open);
    SPECIAL_SYMBOL(iconv_close);
    SPECIAL_SYMBOL(iconv_canonicalize);
    SPECIAL_SYMBOL(iconv);
#   endif

#undef SPECIAL_SYMBOL

    // we failed to find the symbol
    return NULL;
}
#  endif

void *lookupSymbolInNativeObj(void *handle, const char *symbol_name)
{
    ll_mutex_lock(&ll_default_linker.linker_mutex);

#if defined(OBJFORMAT_MACHO)
    // The Mach-O standard says ccall symbols representing a function are prefixed with _
    // https://math-atlas.sourceforge.net/devel/assembly/MachORuntime.pdf
    LL_CHECK(symbol_name[0] == '_');
    symbol_name = symbol_name+1;
#endif
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
    void *result = dlsym(handle, symbol_name);
#elif defined(OBJFORMAT_PEi386)
    void *result = lookupSymbolInDLL_PEi386(&ll_default_linker, symbol_name, handle, NULL, NULL);
#else
    void* result;
    UNUSED(handle);
    UNUSED(symbol_name);
    LL_FATAL("lookupSymbolInNativeObj: Unsupported platform");
#endif

    ll_mutex_unlock(&ll_default_linker.linker_mutex);
    return result;
}

const char *addDLL(pathchar* dll_name)
{
   char *errmsg;
   if (loadNativeObj(dll_name, &errmsg)) {
     return NULL;
   } else {
     LL_ASSERT(errmsg != NULL);
     return errmsg;
   }
}

/* -----------------------------------------------------------------------------
* Searches the system directories to determine if there is a system DLL that
* satisfies the given name. This prevent GHCi from linking against a static
* library if a DLL is available.
*
* Returns: NULL on failure or no DLL found, else the full path to the DLL
*          that can be loaded.
*/
pathchar* findSystemLibrary(pathchar* dll_name)
{
    LL_IF_DEBUG(linker, LL_DEBUG("\nfindSystemLibrary: dll_name = `%"
                                PATH_FMT "'\n", dll_name));

#if defined(OBJFORMAT_PEi386)
    return findSystemLibrary_PEi386(&ll_default_linker, dll_name);
#else
    (void)(dll_name); // Function not implemented for other platforms.
    return NULL;
#endif
}

/* -----------------------------------------------------------------------------
* Emits a warning determining that the system is missing a required security
* update that we need to get access to the proper APIs
*/
void warnMissingKBLibraryPaths( void )
{
    if (!ll_default_linker.missing_update_warn) {
        LL_DEBUG("Warning: If linking fails, consider installing KB2533623.\n");
        ll_default_linker.missing_update_warn = LL_HS_BOOL_TRUE;
    }
}

/* -----------------------------------------------------------------------------
* appends a directory to the process DLL Load path so LoadLibrary can find it
*
* Returns: NULL on failure, or pointer to be passed to removeLibrarySearchPath to
*          restore the search path to what it was before this call.
*/
ll_hs_ptr addLibrarySearchPath(pathchar* dll_path)
{
    LL_IF_DEBUG(linker, LL_DEBUG("\naddLibrarySearchPath: dll_path = `%"
                                PATH_FMT "'\n", dll_path));

#if defined(OBJFORMAT_PEi386)
    return addLibrarySearchPath_PEi386(&ll_default_linker, dll_path);
#else
    (void)(dll_path); // Function not implemented for other platforms.
    return NULL;
#endif
}

/* -----------------------------------------------------------------------------
* removes a directory from the process DLL Load path
*
* Returns: LL_HS_BOOL_TRUE on success, otherwise LL_HS_BOOL_FALSE
*/
ll_hs_bool removeLibrarySearchPath(ll_hs_ptr dll_path_index)
{
    LL_IF_DEBUG(linker, LL_DEBUG("\nremoveLibrarySearchPath: ptr = `%p'\n",
                                dll_path_index));

#if defined(OBJFORMAT_PEi386)
    return removeLibrarySearchPath_PEi386(&ll_default_linker, dll_path_index);
#else
    (void)(dll_path_index); // Function not implemented for other platforms.
    return LL_HS_BOOL_FALSE;
#endif
}

/* -----------------------------------------------------------------------------
 * insert a code symbol in the hash table
 *
 * Returns: 0 on failure, nonzero on success
 */
ll_hs_int insertSymbol(pathchar* obj_name, ll_symbol_name* key, ll_symbol_addr* data)
{
    return ghciInsertSymbolTable(&ll_default_linker, obj_name, key, data, LL_HS_BOOL_FALSE,
                                 LL_SYM_CODE, 0, NULL);
}

/* -----------------------------------------------------------------------------
 * Lookup a symbol in the hash table
 *
 * When 'dependent' is not NULL, adds it as a dependent to the owner of the
 * symbol.
 */
#if defined(OBJFORMAT_PEi386)
static ll_symbol_addr* lookupDependentSymbolImpl (ll_linker *L, ll_symbol_name* lbl,
                                       ObjectCode *dependent, ll_sym_type *type)
{
    ll_assert_lock_held_for(L, &L->linker_mutex);
    LL_ASSERT_FOR(L, dependent == NULL || dependent->L == L);
    return lookupSymbol_PEi386(L, lbl, dependent, type);
}

#else

static ll_symbol_addr* lookupDependentSymbolImpl (ll_linker *L, ll_symbol_name* lbl,
                                       ObjectCode *dependent, ll_sym_type *type)
{
    ll_assert_lock_held_for(L, &L->linker_mutex);
    LL_ASSERT_FOR(L, dependent == NULL || dependent->L == L);
    LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "lookupSymbol: looking up '%s'\n", lbl));

    LL_ASSERT_FOR(L, L->symhash != NULL);
    RtsSymbolInfo *pinfo;

    /* See Note [Resolving __dso_handle] */
    if (strcmp(lbl, MAYBE_LEADING_UNDERSCORE_STR("__dso_handle")) == 0) {
        if (dependent) {
            return dependent->image;
        } else {
            // In the case that we don't know which object the reference lives
            // in we return a random symbol from the executable image.
            return &lookupDependentSymbol;
        }
    }
    if (strcmp(lbl, MAYBE_LEADING_UNDERSCORE_STR("__cxa_atexit")) == 0 && dependent) {
        dependent->cxa_finalize = (cxa_finalize_fn) lookupDependentSymbol(
                L,
                MAYBE_LEADING_UNDERSCORE_STR("__cxa_finalize"),
                dependent,
                NULL);
    }

#if defined(OBJFORMAT_ELF)
    // Resolve references to the GOT if we know the origin object
    if (dependent && strncmp(lbl, "_GLOBAL_OFFSET_TABLE_", 21) == 0) {
        return dependent->info->got_start;
    }
#endif

    if (!ghciLookupSymbolInfo(L, lbl, &pinfo)) {
        LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "lookupSymbol: symbol '%s' not found, trying dlsym\n", lbl));

#       if defined(OBJFORMAT_ELF)
        ll_symbol_addr *ret = internal_dlsym(L, lbl);
        if (type) {
            // We assume that the symbol is code since this is usually the case
            // and dlsym doesn't tell us.
            *type = LL_SYM_CODE;
        }

        // Generally the dynamic linker would define _DYNAMIC, which is
        // supposed to point to various bits of dynamic linker state (see
        // [1]). However, if dynamic linking isn't supported (e.g. in the case
        // of musl) then we can safely declare that it is NULL.
        //
        // [1] https://wiki.gentoo.org/wiki/Hardened/Introduction_to_Position_Independent_Code
        if (ret == NULL && strcmp(lbl, "_DYNAMIC") == 0) {
            ret = (ll_symbol_addr *) &L->rts_dynamic;
            if (type) {
                *type = LL_SYM_DATA;
            }
        }
        return ret;
#       elif defined(OBJFORMAT_MACHO)

        /* HACK: On OS X, all symbols are prefixed with an underscore.
                 However, dlsym wants us to omit the leading underscore from the
                 symbol name -- the dlsym routine puts it back on before
                 searching for the symbol. For now, we simply strip it off here
                 (and ONLY here).
        */
        LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "lookupSymbol: looking up %s with dlsym\n",
                                    lbl));
        LL_CHECK_FOR(L, lbl[0] == '_');
        if (type) {
            // We assume that the symbol is code since this is usually the case
            // and dlsym doesn't tell us.
            *type = LL_SYM_CODE;
        }
        return internal_dlsym(L, lbl + 1);

#       elif defined(OBJFORMAT_WASM32)
        return NULL;
#       else
#       error No OBJFORMAT_* macro set
#       endif
    } else {
        if (strcmp(lbl, "__fini_array_end") == 0) { return (ll_symbol_addr *) &L->rts_no_fini; }
        if (strcmp(lbl, "__fini_array_start") == 0) { return (ll_symbol_addr *) &L->rts_no_fini; }
        if (type) {
            // This is an assumption
            *type = pinfo->type;
        }

        if (dependent) {
            // Add dependent as symbol's owner's dependency
            ObjectCode *owner = pinfo->owner;
            if (owner) {
                // TODO: what does it mean for a symbol to not have an owner?
                insertHashSet(dependent->dependencies, (ll_w)owner);
            }
        }
        LL_ASSERT_FOR(L, pinfo->owner == NULL || pinfo->owner->L == L);
        ll_symbol_addr *result = loadSymbol(L, lbl, pinfo);
        ll_failure_inherit(dependent, pinfo->owner);
        if (!ll_failure_usable(dependent) || !ll_failure_usable(pinfo->owner))
            return NULL;
        return result;
    }
}
#endif /* OBJFORMAT_PEi386 */

ll_symbol_addr* lookupDependentSymbol(ll_linker *L, ll_symbol_name *lbl,
                                      ObjectCode *dependent, ll_sym_type *type)
{
    struct ll_failure_scope scope;
    ll_failure_begin(L, &scope, dependent);
    ll_symbol_addr *result = NULL;
    if (ll_failure_usable(dependent))
        result = lookupDependentSymbolImpl(L, lbl, dependent, type);
    if (!ll_failure_usable(dependent) || ll_failure_blocked(L)) result = NULL;
    ll_failure_end(L, &scope);
    return result;
}

/* Note [Resolving __dso_handle]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * This symbol, which is defined by the C++ ABI, would typically be defined by
 * the system's dynamic linker to act as a "handle", identifying a particular
 * loaded dynamic object to the C++ standard library for the purpose of running
 * destructors on unload. Here we behave the same way that the dynamic linker
 * would, using some address (here the start address) of the loaded object as
 * its handle.
 *
 * Note that references to __dso_handle may be relocated using
 * relocations of bounded displacement and therefore __dso_handle must not be
 * too far from the loaded object's code (hence using its start address).
 *
 * Finally, when we see a reference to __cxa_atexit in an object we take care
 * to lookup and record the address of __cxa_finalize (largely to ensure that
 * the symbol dependency is recorded) and call it with the appropriate handle
 * when the object is unloaded.
 *
 * See #20493.
 * See section 3.3.5 of the Itanium C++ ABI, version 1.83.
 */

/*
 * Load and relocate the object code for a symbol as necessary.
 * Symbol name only used for diagnostics output.
 */
ll_symbol_addr* loadSymbol(ll_linker *L, ll_symbol_name *lbl, RtsSymbolInfo *pinfo) {
    LL_ASSERT_FOR(L, pinfo != NULL);
    LL_ASSERT_FOR(L, pinfo->owner == NULL || pinfo->owner->L == L);
    LL_IF_DEBUG_FOR(L, linker_verbose,
             LL_DEBUG_FOR(L, "lookupSymbol: value of %s is %p, owned by %" PATH_FMT "\n", lbl,
                        pinfo->value,
                        pinfo->owner ? OC_INFORMATIVE_FILENAME(pinfo->owner) : WSTR("No owner, probably built-in.")));
    ObjectCode* oc = pinfo->owner;
    if (!ll_failure_usable(oc)) return NULL;

    /* Symbol can be found during linking, but hasn't been relocated. Do so now.
        See Note [runtime-linker-phases] */
    if (oc && lbl && oc->status == OBJECT_LOADED) {
        oc->status = OBJECT_NEEDED;
        LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "lookupSymbol: on-demand "
                                    "loading symbol '%s'\n", lbl));
        int r = ocTryLoad(oc);
        if (!r) {
            return NULL;
        }
    }

    return pinfo->value;
}

void
printLoadedObjects(ll_linker *L) {
    ObjectCode* oc;
    for (oc = L->gc.objects; oc; oc = oc->next) {
        if (oc->sections != NULL) {
            int i;
            printf("%" PATH_FMT "\n", OC_INFORMATIVE_FILENAME(oc));
            for (i=0; i < oc->n_sections; i++) {
                if(oc->sections[i].mapped_start != NULL || oc->sections[i].start != NULL) {
                    printf("\tsec %2d[alloc: %d; kind: %d]: %p - %p; mmaped: %p - %p\n",
                        i, oc->sections[i].alloc, oc->sections[i].kind,
                        oc->sections[i].start,
                        (void*)((uintptr_t)(oc->sections[i].start) + oc->sections[i].size),
                        oc->sections[i].mapped_start,
                        (void*)((uintptr_t)(oc->sections[i].mapped_start) + oc->sections[i].mapped_size));
                }
            }
        }
   }
}

static ll_symbol_addr* lookupSymbolImpl(ll_linker *L, ll_symbol_name* lbl)
{
    // NULL for "don't add dependent". When adding a dependency we call
    // lookupDependentSymbol directly.
    ll_symbol_addr* r = lookupDependentSymbol(L, lbl, NULL, NULL);
    if (ll_failure_blocked(L)) return NULL;
    if (!r) {
        if (!L->services.cfg.optimistic) {
          LL_LOG_ERROR_FOR(L, "^^ Could not load '%s', dependency unresolved. "
                     "See top entry above. You might consider using --optimistic-linking\n",
                     lbl);
          LL_IF_DEBUG_FOR(L, linker, printLoadedObjects(L));
          fflush(stderr);
        } else {
          // if --optimistic-linking is passed into the RTS we allow the linker
          // to optimistically continue
          LL_LOG_ERROR_FOR(L, "^^ Could not load '%s', dependency unresolved, "
                     "optimistically continuing\n",
                     lbl);
          r = (void*) 0xDEADBEEF;
        }
    }

    if (!runPendingInitializers(L)) {
        LL_LOG_ERROR_FOR(L, "lookupSymbol: Failed to run initializers.");
        return NULL;
    }
    return r;
}

ll_symbol_addr* lookupSymbol_(ll_linker *L, ll_symbol_name *lbl)
{
    struct ll_failure_scope scope;
    ll_failure_begin(L, &scope, NULL);
    ll_symbol_addr *result = lookupSymbolImpl(L, lbl);
    ll_failure_end(L, &scope);
    return result;
}

ll_symbol_addr* lookupSymbol(ll_symbol_name* lbl)
{
    ll_mutex_lock(&ll_default_linker.linker_mutex);
    ll_symbol_addr* r = lookupSymbol_(&ll_default_linker, lbl);
    ll_mutex_unlock(&ll_default_linker.linker_mutex);
    return r;
}

/* -----------------------------------------------------------------------------
 * Debugging aid: look in GHCi's object symbol tables for symbols
 * within DELTA bytes of the specified address, and show their names.
 */
#if LL_DEBUG_CODE
LL_FRIEND_API void ghci_enquire ( ll_symbol_addr* addr );

void ghci_enquire(ll_symbol_addr* addr)
{
   int   i;
   ll_symbol_name* sym;
   RtsSymbolInfo* a;
   const int DELTA = 64;
   ObjectCode* oc;

   for (oc = ll_default_linker.gc.objects; oc; oc = oc->next) {
      for (i = 0; i < oc->n_symbols; i++) {
         sym = oc->symbols[i].name;
         if (sym == NULL) continue;
         a = NULL;
         if (a == NULL) {
             ghciLookupSymbolInfo(&ll_default_linker, sym, &a);
         }
         if (a == NULL) {
             // debugBelch("ghci_enquire: can't find %s\n", sym);
         }
         else if (   a->value
                  && (char*)addr-DELTA <= (char*)a->value
                  && (char*)a->value <= (char*)addr+DELTA) {
             LL_DEBUG("%p + %3d  ==  `%s'\n", addr,
                        (int)((char*)a->value - (char*)addr), sym);
         }
      }
   }
}
#endif

pathchar*
resolveSymbolAddr (pathchar* buffer, int size,
                   ll_symbol_addr* symbol, uintptr_t* top)
{
#if defined(OBJFORMAT_PEi386)
  return resolveSymbolAddr_PEi386 (&ll_default_linker, buffer, size, symbol, top);
#else /* OBJFORMAT_PEi386 */
  (void)buffer;
  (void)size;
  (void)symbol;
  (void)top;
  return NULL;
#endif /* OBJFORMAT_PEi386 */
}

/*
 * Remove symbols from the symbol table, and free oc->symbols.
 * This operation is idempotent.
 */
static void removeOcSymbols (ObjectCode *oc)
{
    if (oc->symbols == NULL) return;

    // Remove all the mappings for the symbols within this object..
    int i;
    for (i = 0; i < oc->n_symbols; i++) {
        if (oc->symbols[i].name != NULL) {
            ghciRemoveSymbolTable(oc->L, oc->symbols[i].name, oc);
        }
    }

    LL_FREE_FOR(oc->L, oc->symbols);
    oc->symbols = NULL;
}

static void
freePreloadObjectFile (ObjectCode *oc)
{
    if (oc->image != NULL) {
        if (RTS_LINKER_USE_MMAP && oc->imageMapped) {
            ll_munmap_for_linker(oc->L, oc->image, oc->fileSize,
                                 "freePreloadObjectFile");
        } else {
            LL_FREE_FOR(oc->L, oc->image - oc->misalignment);
        }
    }

    oc->image = NULL;
    oc->fileSize = 0;
}

int ll_init_object(ObjectCode *oc)
{
    /* Native shared-library objects have no relocatable image. */
    if (oc->type == DYNAMIC_OBJECT) return 1;
    oc->format_ops = ll_format_select(oc);
    if (oc->format_ops == NULL) return 0;
    oc->formatName = oc->format_ops->name;
    if (!oc->format_ops->init(oc)) {
        if (oc->L->error_status == LL_OK)
            ll_error_set(oc->L, LL_E_FORMAT, "loadObj: invalid image in %" PATH_FMT,
                         OC_INFORMATIVE_FILENAME(oc));
        return 0;
    }
    return 1;
}

void ll_discard_object(ObjectCode *oc)
{
    if (oc->resolution_failed) {
        /* A recursive symbol lookup can export an address before loadOc
         * completes. Retained dependents may already refer to this object;
         * keep its image and dependency identity until normal unload/GC. */
        ll_linker *L = oc->L;
        const ll_status status = L->error_status;
        const bool pre_backend_error = L->error_from_pre_backend;
        char message[LL_ERROR_CAPACITY];
        memcpy(message, L->error_message, sizeof(message));
        if (L->services.host.object_registered != NULL)
            L->services.host.object_registered(L->services.host.ctx, oc);
        oc->next_loaded_object = L->gc.loaded_objects;
        L->gc.loaded_objects = oc;
        L->error_status = status;
        L->error_from_pre_backend = pre_backend_error;
        memcpy(L->error_message, message, sizeof(message));
        return;
    }
    /* Only unregistered objects whose initializers have not run come here.
     * Symbols must be removed while their names still live in the image.
     * Format teardown also needs the image and section metadata to be alive. */
    removeOcSymbols(oc);
    freeObjectCode(oc);
}

/* Note [Object unloading and finalizers]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * An ObjectCode may contain .fini_array/.dtors sections with finalizers that
 * should run when the object is unloaded. However, we must only run these
 * finalizers if the corresponding initializers (.init_array/.ctors) have
 * actually been executed.
 *
 * Archive members start in OBJECT_LOADED state and only progress to
 * OBJECT_NEEDED -> OBJECT_RESOLVED -> OBJECT_READY when a symbol from
 * them is actually required. An archive member that was never needed never
 * has its relocations applied, so its .fini_array section data still
 * contains zeros (unresolved relocation targets). Running those finalizers
 * would dereference NULL function pointers.
 *
 * When unloadObj sets an object's status to OBJECT_UNLOADED, it does so
 * regardless of the previous state, so we cannot rely on the status alone
 * to decide whether finalizers should run. Instead, we track whether
 * initializers were executed via the initializersRan flag, which is set in
 * ocRunInit after successfully running the initializers.
 */

/*
 * freeObjectCode() releases all the pieces of an ObjectCode.  It is called by
 * the GC when a previously unloaded ObjectCode has been determined to be
 * unused, and when an error occurs during loadObj().
 */
void freeObjectCode (ObjectCode *oc)
{
    LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "freeObjectCode: start\n"));

    // Run finalizers only if initializers have been run.
    // See Note [Object unloading and finalizers].
    if (oc->type == STATIC_OBJECT && oc->initializersRan) {
        if (oc->format_ops != NULL) oc->format_ops->run_fini(oc);
    }

    // See Note [Resolving __dso_handle]
    if (oc->cxa_finalize) {
        oc->cxa_finalize(oc->image);
    }

    /* A failed native load may dispose metadata while already holding the
     * linker lock, after no handle was acquired. Only a live handle requires
     * the locked native-code teardown. */
    if (oc->type == DYNAMIC_OBJECT && oc->dlopen_handle != NULL) {
#if defined(OBJFORMAT_ELF) || defined(darwin_HOST_OS)
        ll_mutex_lock_for(oc->L, &oc->L->linker_mutex);
        freeNativeCode_POSIX(oc);
        ll_mutex_unlock_for(oc->L, &oc->L->linker_mutex);
#else
        LL_FATAL_FOR(oc->L, "freeObjectCode: This shouldn't happen");
#endif
    }

    /* Teardown needs the image and section metadata alive. This is the single
     * format cleanup point, including partial initialization and PE image and
     * unwind ownership. Finalizers and __cxa_finalize have already run. */
    if (oc->format_ops != NULL) oc->format_ops->deinit(oc);
    freePreloadObjectFile(oc);

    if (oc->symbols != NULL) {
        LL_FREE_FOR(oc->L, oc->symbols);
        oc->symbols = NULL;
    }

    if (oc->extraInfos != NULL) {
        freeStrHashTable(oc->extraInfos, NULL);
        oc->extraInfos = NULL;
    }

    if (oc->sections != NULL) {
        int i;
        for (i=0; i < oc->n_sections; i++) {
            if (oc->sections[i].start != NULL) {
                switch(oc->sections[i].alloc){
#if RTS_LINKER_USE_MMAP
                case SECTION_MMAP:
                    ll_munmap_for_linker(oc->L,
                        oc->sections[i].mapped_start,
                        oc->sections[i].mapped_size,
                        "freeObjectCode");
                    break;
#endif
                case SECTION_M32:
                    // Freed by m32_allocator_free
                    break;
                case SECTION_MALLOC:
                    LL_IF_DEBUG_FOR(oc->L, zero_on_gc,
                        memset(oc->sections[i].start,
                            0x00, oc->sections[i].size));
                    LL_FREE_FOR(oc->L, oc->sections[i].start);
                    break;
                default:
                    break;
                }
            }
            if (oc->sections[i].info) {
                LL_FREE_FOR(oc->L, oc->sections[i].info);
            }
        }
        LL_FREE_FOR(oc->L, oc->sections);
    }

    freeProddableBlocks(oc->L, &oc->proddables);
    freeSegments(oc);

    /* Free symbol_extras.  On x86_64 Windows, symbol_extras are allocated
     * alongside the image, so we don't need to free. */
#if defined(NEED_SYMBOL_EXTRAS) && (!defined(x86_64_HOST_ARCH) \
                                    || !defined(mingw32_HOST_OS))
    if (RTS_LINKER_USE_MMAP) {
      if (!USE_CONTIGUOUS_MMAP && !oc->L->services.cfg.always_pic &&
          oc->symbol_extras != NULL) {
        // Freed by m32_allocator_free
      }
    }
    else {
        LL_FREE_FOR(oc->L, oc->symbol_extras);
    }
#endif

#if defined(NEED_M32)
    m32_allocator_free_for(oc->L, oc->rx_m32);
    m32_allocator_free_for(oc->L, oc->rw_m32);
#endif

    LL_FREE_FOR(oc->L, oc->fileName);
    LL_FREE_FOR(oc->L, oc->archiveMemberName);

    freeHashSet(oc->dependencies);

    LL_FREE_FOR(oc->L, oc);
}

ObjectCode*
mkOc( ll_linker *L, ObjectType type, pathchar *path, char *image, int imageSize,
      bool mapped, pathchar *archiveMemberName, int misalignment ) {
   ObjectCode* oc;


   LL_IF_DEBUG_FOR(L, linker,
                   LL_DEBUG_FOR(L, "mkOc: %" PATH_FMT "\n", path));
   oc = LL_MALLOC_FOR(L, sizeof(ObjectCode), "mkOc(oc)");
   oc->L = L;

   oc->info = NULL;
   oc->type = type;

   const struct ll_format_ops *native = ll_format_native();
   oc->formatName = native != NULL ? native->name : "unknown";
   oc->format_ops = NULL;

   oc->image = image;
   oc->fileName = pathdup(L, path);

   if (archiveMemberName) {
       oc->archiveMemberName = LL_MALLOC_FOR(
           L, (pathlen(archiveMemberName)+1) * pathsize, "loadObj");
       pathcopy(oc->archiveMemberName, archiveMemberName);
   } else {
       oc->archiveMemberName = NULL;
   }

   if (oc->archiveMemberName == NULL) {
       oc->status = OBJECT_NEEDED;
   } else {
       oc->status = OBJECT_LOADED;
   }

   oc->fileSize          = imageSize;
   oc->n_symbols         = 0;
   oc->symbols           = NULL;
   oc->n_sections        = 0;
   oc->sections          = NULL;
   oc->n_segments        = 0;
   oc->segments          = NULL;
   initProddableBlockSet(&oc->proddables);
   oc->foreign_exports   = NULL;
#if defined(NEED_SYMBOL_EXTRAS)
   oc->symbol_extras     = NULL;
   oc->symbol_extra_stride = 0;
#endif
   oc->bssBegin          = NULL;
   oc->bssEnd            = NULL;
   oc->imageMapped       = mapped;

   oc->misalignment      = misalignment;
   oc->initializersRan   = false;
   oc->resolution_failed = false;
   oc->cxa_finalize      = NULL;
   oc->extraInfos        = NULL;

   /* chain it onto the list of objects */
   oc->next              = NULL;
   oc->prev              = NULL;
   oc->next_loaded_object = NULL;
   oc->mark              = L->gc.object_code_mark_bit;
   /* this will get cleared by the caller if object is not safely unloadable */
   oc->unloadable        = true;
   oc->dependencies      = allocHashSet(L);

#if defined(NEED_M32)
   oc->rw_m32 = m32_allocator_new_for(L, false);
   oc->rx_m32 = m32_allocator_new_for(L, true);
#endif

#if defined(OBJFORMAT_ELF) && LL_ELF_HAS_SHN_XINDEX
   oc->shndx_table = SHNDX_TABLE_UNINIT(L);
#endif

   oc->nc_ranges = NULL;
   oc->dlopen_handle = NULL;

   return oc;
}

/* -----------------------------------------------------------------------------
 * Check if an object or archive is already loaded.
 *
 * Returns: 1 if the path is already loaded, 0 otherwise.
 */
ll_hs_int
isAlreadyLoaded( ll_linker *L, pathchar *path )
{
    for (ObjectCode *o = L->gc.objects; o; o = o->next) {
       if (0 == pathcmp(o->fileName, path)
           && o->status != OBJECT_UNLOADED) {
           return 1; /* already loaded */
       }
    }
    return 0; /* not loaded yet */
}

//
// Load the object file into memory.  This will not be its final resting place,
// as on 64-bit platforms we need to map its segments into the low 2Gb of the
// address space, properly aligned.
//
static ObjectCode *
preloadObjectFile (ll_linker *L, pathchar *path)
{
   int fileSize;
   struct_stat st;
   int r;
   void *image;
   ObjectCode *oc;
   int misalignment = 0;

   r = pathstat(path, &st);
   if (r == -1) {
       ll_error_set(L, LL_E_IO, "loadObj: %" PATH_FMT ": file doesn't exist", path);
       LL_LOG_ERROR_FOR(L, "loadObj: %" PATH_FMT ": file doesn't exist", path);
       return NULL;
   }

   if (st.st_size <= 0) {
       ll_error_set(L, LL_E_FORMAT, "loadObj: %" PATH_FMT ": empty object file", path);
       LL_LOG_ERROR_FOR(L, "loadObj: %" PATH_FMT ": empty object file", path);
       return NULL;
   }
   if ((uintmax_t)st.st_size > INT_MAX) {
       ll_error_set(L, LL_E_UNSUPPORTED,
                    "loadObj: %" PATH_FMT ": object file is too large", path);
       LL_LOG_ERROR_FOR(L, "loadObj: %" PATH_FMT ": object file is too large", path);
       return NULL;
   }
   fileSize = st.st_size;

#if RTS_LINKER_USE_MMAP
   int fd;

   /* On many architectures malloc'd memory isn't executable, so we need to use
    * mmap. */

#if defined(openbsd_HOST_OS)
   fd = open(path, O_RDONLY, S_IRUSR);
#else
   fd = open(path, O_RDONLY);
#endif
   if (fd == -1) {
      ll_error_set(L, LL_E_IO, "loadObj: can't open %" PATH_FMT, path);
      LL_LOG_ERROR_FOR(L, "loadObj: can't open %s", path);
      return NULL;
   }

   /* iOS does not permit to mmap with r+w+x, however while the comment for
    * this function says this is not the final resting place, for some
    * architectures / hosts (at least mach-o non-iOS -- see ocGetNames_MachO)
    * the image mmaped here in fact ends up being the final resting place for
    * the sections. And hence we need to leave r+w+x here for other hosts
    * until all hosts have been made aware of the initial image being r+w only.
    *
    * See also the misalignment logic for darwin below.
    */
#if defined(darwin_HOST_OS) || defined(openbsd_HOST_OS)
   image = ll_mmap_for_linker(L, fileSize, MEM_READ_WRITE, MAP_PRIVATE, fd, 0);
#else
   image = ll_mmap_for_linker(L, fileSize, MEM_READ_WRITE_EXECUTE, MAP_PRIVATE, fd, 0);
#endif

   if (image == NULL) {
       ll_error_set(L, LL_E_IO, "mmap: failed. errno = %d", errno);
       LL_LOG_ERROR_FOR(L, "mmap: failed. errno = %d", errno);
       close(fd);
       return NULL;
   }
   // not 32-bit yet, we'll remap later
   close(fd);

#else /* !RTS_LINKER_USE_MMAP */
   FILE *f;

   /* load the image into memory */
   /* coverity[toctou] */
   f = pathopen(path, WSTR("rb"));
   if (!f) {
       ll_error_set(L, LL_E_IO, "loadObj: can't preload `%" PATH_FMT "'", path);
       LL_LOG_ERROR_FOR(L, "loadObj: can't preload `%" PATH_FMT "'", path);
       return NULL;
   }

#  if defined(darwin_HOST_OS)

    // In a Mach-O .o file, all sections can and will be misaligned
    // if the total size of the headers is not a multiple of the
    // desired alignment. This is fine for .o files that only serve
    // as input for the static linker, but it's not fine for us,
    // as SSE (used by gcc for floating point) and Altivec require
    // 16-byte alignment.
    // We calculate the correct alignment from the header before
    // reading the file, and then we misalign image on purpose so
    // that the actual sections end up aligned again.
   misalignment = machoGetMisalignment(L, f);
   if (misalignment < 0) {
       fclose(f);
       return NULL;
   }
   image = LL_MALLOC_FOR(L, fileSize + misalignment, "loadObj(image)");
   image += misalignment;

# else /* !defined(darwin_HOST_OS) */

   image = LL_MALLOC_FOR(L, fileSize, "loadObj(image)");

#endif /* !defined(darwin_HOST_OS) */

   int n;
   n = fread ( image, 1, fileSize, f );
   fclose(f);
   if (n != fileSize) {
       ll_error_set(L, LL_E_IO, "loadObj: error whilst reading `%" PATH_FMT "'", path);
       LL_LOG_ERROR_FOR(L, "loadObj: error whilst reading `%" PATH_FMT "'", path);
       LL_FREE_FOR(L, (char *)image - misalignment);
       return NULL;
   }

#endif /* RTS_LINKER_USE_MMAP */

   LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "loadObj: preloaded image at %p\n", (void *) image));

   oc = mkOc(L, STATIC_OBJECT, path, image, fileSize,
             RTS_LINKER_USE_MMAP != 0, NULL, misalignment);
   if (!ll_init_object(oc)) {
       ll_discard_object(oc);
       return NULL;
   }
   return oc;
}

/* -----------------------------------------------------------------------------
 * Load an obj (populate the global symbol table, but don't resolve yet)
 *
 * Returns: 1 if ok, 0 on error.
 */
static ll_hs_int loadObj_ (ll_linker *L, pathchar *path)
{
   if (path == NULL) {
       ll_error_set(L, LL_E_STATE, "loadObj: object path is NULL");
       return 0;
   }
   if (!ll_failure_path_usable(L, path)) return 0;
   // Check that we haven't already loaded this object.
   // Ignore requests to load multiple times

   if (isAlreadyLoaded(L, path)) {
       LL_IF_DEBUG_FOR(L, linker,
                LL_DEBUG_FOR(L, "ignoring repeated load of %" PATH_FMT "\n", path));
       return 1; // success
   }

   // Things that look like object files (e.g. end in `.o`) may nevertheless be
   // archives, as noted in Note [Object merging] in GHC.Driver.Pipeline.Execute.
   if (isArchive(path)) {
       return loadArchive_(L, path);
   }

   ObjectCode *oc = preloadObjectFile(L, path);
   if (oc == NULL) return 0;

   if (! loadOc(oc)) {
       // Dispose failed loads; quarantined objects retain escaped addresses
       // until normal unload and GC. No initializers have run here.
       ll_discard_object(oc);
       return 0;
   }

   if (L->services.host.object_registered != NULL) {
       L->services.host.object_registered(L->services.host.ctx, oc);
   }

   oc->next_loaded_object = L->gc.loaded_objects;
   L->gc.loaded_objects = oc;
   return 1;
}

ll_hs_int loadObj (pathchar *path)
{
   ll_mutex_lock(&ll_default_linker.linker_mutex);
    ll_error_clear(&ll_default_linker);
   ll_hs_int r = loadObj_(&ll_default_linker, path);
   ll_mutex_unlock(&ll_default_linker.linker_mutex);
   return r;
}

static ll_hs_int loadOcImpl (ObjectCode* oc)
{
   int r;

   LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "start\n"));

   /* verify the in-memory image */
   if (!ll_format_require(oc)) return 0;
   r = oc->format_ops->verify(oc);
   if (!r) {
       if (oc->L->error_status == LL_OK) {
           ll_error_set(oc->L, LL_E_FORMAT,
                        "loadObj: invalid object image in %" PATH_FMT,
                        OC_INFORMATIVE_FILENAME(oc));
       }
       LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "ocVerifyImage_* failed\n"));
       return r;
   }

   /* Note [loadOc orderings]
      ~~~~~~~~~~~~~~~~~~~~~~~
      The order of `ocAllocateExtras` and `ocGetNames` matters. For MachO
      and ELF, `ocInit` and `ocGetNames` initialize a bunch of pointers based
      on the offset to `oc->image`, but `ocAllocateExtras` may relocate
      the address of `oc->image` and invalidate those pointers. So we must
      compute or recompute those pointers after `ocAllocateExtras`.

      On Windows, when we have an import library we (for now, as we don't honor
      the lazy loading semantics of the library and instead GHCi is already
      lazy) don't use the library after ocGetNames as it just populates the
      symbol table.  Allocating space for jump tables in ocAllocateExtras
      would just be a waste then as we'll be stopping further processing of the
      library in the next few steps. If necessary, the actual allocation
      happens in `ocGetNames_PEi386` simply set the correct pointers.
      */

   r = oc->format_ops->alloc_extras(oc);
   if (!r) {
       if (oc->L->error_status == LL_OK) {
           ll_error_set(oc->L, LL_E_NOMEM,
                        "loadObj: could not allocate symbol extras in %" PATH_FMT,
                        OC_INFORMATIVE_FILENAME(oc));
       }
       LL_IF_DEBUG_FOR(oc->L, linker,
                      ocDebugBelch(oc, "alloc_extras failed\n"));
       return r;
   }

   /* build the symbol list for this image */
   r = oc->format_ops->get_names(oc);
   if (!r) {
       if (oc->L->error_status == LL_OK) {
           ll_error_set(oc->L, LL_E_FORMAT,
                        "loadObj: could not read symbols in %" PATH_FMT,
                        OC_INFORMATIVE_FILENAME(oc));
       }
       LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "ocGetNames_* failed\n"));
       return r;
   }

   if (!ll_failure_usable(oc)) return 0;

   /* Loaded, but not resolved yet, ensure the OC is in a consistent state.
      If a target has requested the ObjectCode not to be resolved then honor
      this requests.  Usually this means the ObjectCode has not been initialized
      and can't be. */
   if (oc->status != OBJECT_DONT_RESOLVE) {
       if (oc->archiveMemberName == NULL) {
           oc->status = OBJECT_NEEDED;
       } else {
           oc->status = OBJECT_LOADED;
       }
   }
   LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "done\n"));

   return 1;
}

ll_hs_int loadOc(ObjectCode *oc)
{
    struct ll_failure_scope scope;
    ll_failure_begin(oc->L, &scope, oc);
    ll_hs_int result = ll_failure_usable(oc) ? loadOcImpl(oc) : 0;
    if (!ll_failure_usable(oc)) result = 0;
    ll_failure_end(oc->L, &scope);
    return result;
}

/* -----------------------------------------------------------------------------
* try to load and initialize an ObjectCode into memory
*
* Returns: 1 if ok, 0 on error.
*/
static int ocTryLoadImpl(ObjectCode* oc) {
    int r;

    if (oc->status == OBJECT_UNLOADED) return 1;
    if (!ll_failure_usable(oc)) return 0;
    if (oc->status != OBJECT_NEEDED) {
        return 1;
    }

    /*  Check for duplicate symbols by looking into `symhash`.
        Duplicate symbols are any symbols which exist
        in different ObjectCodes that have both been loaded, or
        are to be loaded by this call.

        This call is intended to have no side-effects when a non-duplicate
        symbol is re-inserted.

        We set the Address to NULL since that is not used to distinguish
        symbols. Duplicate symbols are distinguished by name and oc.
    */
    int x;
    Symbol_t symbol;
    for (x = 0; x < oc->n_symbols; x++) {
        symbol = oc->symbols[x];
        if (   symbol.name
            && !ghciInsertSymbolTable(oc->L, oc->fileName, symbol.name,
                                      symbol.addr,
                                      isSymbolWeak(oc, symbol.name),
                                      symbol.type, 0, oc)) {
            /* A nested caller can propagate this rejection through its own
             * backend. Keep its pre-dispatch origin until a fresh error is set. */
            oc->L->error_from_pre_backend = true;
            return 0;
        }
    }

    LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "resolving\n"));
    if (!ll_format_require(oc)) return 0;
    r = oc->format_ops->resolve(oc);
    if (!r) {
        if (oc->L->error_status == LL_OK) {
            ll_error_set(oc->L, LL_E_UNRESOLVED,
                         "could not resolve object %" PATH_FMT,
                         OC_INFORMATIVE_FILENAME(oc));
        }
        /* A terminal backend error may follow earlier relocation writes.
         * Missing definitions retain the ordinary retry path. */
        ll_failure_resolution(oc);
        LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "resolution failed\n"));
        return r;
    }

    if (!ll_failure_usable(oc)) return 0;
    LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "protecting mappings\n"));
#if defined(NEED_SYMBOL_EXTRAS)
    if (!ocProtectExtras(oc)) {
        ll_failure_protection(oc);
        return 0;
    }
#endif

    // We have finished loading and relocating; flush the m32 allocators to
    // setup page protections.
#if defined(NEED_M32)
    if (!m32_allocator_flush_for(oc->L, oc->rx_m32) ||
        !m32_allocator_flush_for(oc->L, oc->rw_m32)) {
        ll_failure_protection(oc);
        return 0;
    }
#endif

    LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "resolved\n"));
    oc->status = OBJECT_RESOLVED;

    return 1;
}

int ocTryLoad(ObjectCode *oc)
{
    struct ll_failure_scope scope;
    ll_failure_begin(oc->L, &scope, oc);
    int result = ocTryLoadImpl(oc);
    if (oc->status != OBJECT_UNLOADED && !ll_failure_usable(oc)) result = 0;
    ll_failure_end(oc->L, &scope);
    return result;
}

// run init/init_array/ctors/mod_init_func
int ocRunInit(ObjectCode *oc)
{
    if (oc->status == OBJECT_UNLOADED) return 1;
    if (!ll_failure_usable(oc)) return 0;
    if (oc->status != OBJECT_RESOLVED) {
        return 1;
    }

    LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "running initializers\n"));

    if (!ll_format_require(oc)) return 0;

    // See Note [Tracking foreign exports] in ForeignExports.c
    if (oc->L->services.host.before_init != NULL) {
        oc->L->services.host.before_init(oc->L->services.host.ctx, oc);
    }
    int r = oc->format_ops->run_init(oc);
    if (oc->L->services.host.after_init != NULL) {
        oc->L->services.host.after_init(oc->L->services.host.ctx, oc, r != 0);
    }

    if (!r) {
        if (oc->L->error_status == LL_OK) {
            ll_error_set(oc->L, LL_E_INIT,
                         "could not run initializers of %" PATH_FMT,
                         OC_INFORMATIVE_FILENAME(oc));
        }
        ll_failure_initialization(oc);
        return r;
    }
    oc->initializersRan = true;
    oc->status = OBJECT_READY;

    return 1;
}

int runPendingInitializers (ll_linker *L)
{
    for (ObjectCode *oc = L->gc.objects; oc; oc = oc->next) {
        if (oc->resolution_failed) continue;
        int r = ocRunInit(oc);
        if (!r) {
            LL_LOG_ERROR_FOR(L, "Could not run initializers of Object Code %" PATH_FMT ".\n", OC_INFORMATIVE_FILENAME(oc));
            LL_IF_DEBUG_FOR(L, linker, printLoadedObjects(L));
            fflush(stderr);
            return r;
        }
    }

    if (L->services.host.after_init_batch != NULL) {
        // collect any new cost centres & CCSs that were defined during runInit
        L->services.host.after_init_batch(L->services.host.ctx);
    }

    return 1;
}

/* -----------------------------------------------------------------------------
 * resolve all the currently unlinked objects in memory
 *
 * Returns: 1 if ok, 0 on error.
 */
static ll_hs_int resolveObjsImpl(ll_linker *L)
{
    if (ll_failure_active(L)) return 0;
    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "resolveObjs: start\n"));

    for (ObjectCode *oc = L->gc.objects; oc; oc = oc->next) {
        int r = ocTryLoad(oc);
        if (!r) {
            LL_LOG_ERROR_FOR(L, "Could not load Object Code %" PATH_FMT ".\n", OC_INFORMATIVE_FILENAME(oc));
            LL_IF_DEBUG_FOR(L, linker, printLoadedObjects(L));
            fflush(stderr);
            return r;
        }
    }

    if (!runPendingInitializers(L)) {
        return 0;
    }

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "resolveObjs: done\n"));
    return 1;
}

static ll_hs_int resolveObjs_(ll_linker *L)
{
    struct ll_failure_scope scope;
    ll_failure_begin(L, &scope, NULL);
    ll_hs_int result = resolveObjsImpl(L);
    ll_failure_end(L, &scope);
    return result;
}

ll_hs_int resolveObjs (void)
{
    ll_mutex_lock(&ll_default_linker.linker_mutex);
    ll_error_clear(&ll_default_linker);
    ll_hs_int r = resolveObjs_(&ll_default_linker);
    ll_mutex_unlock(&ll_default_linker.linker_mutex);
    return r;
}

/* -----------------------------------------------------------------------------
 * delete an object from the pool
 */
static ll_hs_int unloadObj_ (ll_linker *L, pathchar *path, bool just_purge)
{
    LL_ASSERT_FOR(L, L->symhash != NULL);
    LL_ASSERT_FOR(L, L->gc.objects != NULL);

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "unloadObj: %" PATH_FMT "\n", path));

    bool unloadedAnyObj = false;
    ObjectCode *prev = NULL;
    // NOTE (osa): There may be more than one object with the same file name
    // (happens when loading archive files) so we don't stop after unloading one
    for (ObjectCode *oc = L->gc.loaded_objects; oc; oc = oc->next_loaded_object) {
        if (pathcmp(oc->fileName,path) == 0) {
            oc->status = OBJECT_UNLOADED;

            // These are both idempotent, so in just_purge mode we can later
            // call unloadObj() to really unload the object.
            removeOcSymbols(oc);
            if (L->services.host.on_unload != NULL) {
                L->services.host.on_unload(L->services.host.ctx, oc);
            }

            unloadedAnyObj = true;

            if (!just_purge) {
                L->gc.n_unloaded_objects += 1;
                // Remove object code from root set
                if (prev == NULL) {
                    L->gc.loaded_objects = oc->next_loaded_object;
                } else {
                    prev->next_loaded_object = oc->next_loaded_object;
                }
            }
        } else {
            prev = oc;
        }
    }

    if (unloadedAnyObj) {
        return 1;
    } else {
        ll_error_set(L, LL_E_NOT_FOUND,
                     "unloadObj: can't find `%" PATH_FMT "' to unload", path);
        LL_LOG_ERROR_FOR(L, "unloadObj: can't find `%" PATH_FMT "' to unload", path);
        return 0;
    }
}

ll_hs_int unloadObj (pathchar *path)
{
    ll_mutex_lock(&ll_default_linker.linker_mutex);
    ll_error_clear(&ll_default_linker);
    ll_hs_int r = unloadObj_(&ll_default_linker, path, false);
    ll_mutex_unlock(&ll_default_linker.linker_mutex);
    return r;
}

ll_hs_int purgeObj (pathchar *path)
{
    ll_mutex_lock(&ll_default_linker.linker_mutex);
    ll_error_clear(&ll_default_linker);
    ll_hs_int r = unloadObj_(&ll_default_linker, path, true);
    ll_mutex_unlock(&ll_default_linker.linker_mutex);
    return r;
}

ObjectCode *lookupObjectByPath(ll_linker *L, pathchar *path) {
  for (ObjectCode *o = L->gc.objects; o; o = o->next) {
     if (0 == pathcmp(o->fileName, path)) {
         return o;
     }
  }
  return NULL;
}

OStatus getObjectLoadStatus_ (ll_linker *L, pathchar *path)
{
    ObjectCode *oc = lookupObjectByPath(L, path);
    if (oc) {
      return oc->status;
    }
    return OBJECT_NOT_LOADED;
}

OStatus getObjectLoadStatus (pathchar *path)
{
    ll_mutex_lock(&ll_default_linker.linker_mutex);
    OStatus r = getObjectLoadStatus_(&ll_default_linker, path);
    ll_mutex_unlock(&ll_default_linker.linker_mutex);
    return r;
}

/* -----------------------------------------------------------------------------
 * Section management.
 */
void
addSection (ll_linker *L, Section *s, SectionKind kind, SectionAlloc alloc,
            void* start, ll_word size,
            ll_word mapped_offset, void* mapped_start, ll_word mapped_size,
            size_t format_info_size)
{
   s->start        = start;     /* actual start of section in memory */
   s->size         = size;      /* actual size of section in memory */
   s->kind         = kind;
   s->alloc        = alloc;
   s->mapped_offset = mapped_offset; /* offset from the image of mapped_start */

   s->mapped_start = mapped_start; /* start of mmap() block */
   s->mapped_size  = mapped_size;  /* size of mmap() block */

   if (!s->info)
     s->info
       = (struct SectionFormatInfo*)LL_CALLOC_FOR(L, 1, format_info_size,
                                            "addSection(SectionFormatInfo)");

   LL_IF_DEBUG_FOR(L, linker,
            LL_DEBUG_FOR(L, "addSection: %p-%p (size %" LL_FMT_WORD "), kind %d\n",
                       start, (void*)((ll_word)start + size),
                       size, kind ));
}

void * loadNativeObj (pathchar *path, char **errmsg)
{
   LL_IF_DEBUG(linker, LL_DEBUG("loadNativeObj: path = '%" PATH_FMT "'\n", path));
   ll_mutex_lock(&ll_default_linker.linker_mutex);

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
   void *r = loadNativeObj_POSIX(&ll_default_linker, path, errmsg);
#elif defined(OBJFORMAT_PEi386)
   void *r = NULL;
   *errmsg = (char*)addDLL_PEi386(&ll_default_linker, path, (HINSTANCE*)&r);
#else
   void *r;
   UNUSED(errmsg);
   LL_FATAL("loadNativeObj: not implemented on this platform");
#endif

#if defined(OBJFORMAT_ELF)
   if (!r) {
       // Check if native object may be a linker script and try loading a native
       // object from it
       r = loadNativeObjFromLinkerScript_ELF(&ll_default_linker, errmsg);
   }
#endif

   ll_mutex_unlock(&ll_default_linker.linker_mutex);
   return r;
}

ll_hs_int unloadNativeObj_(ll_linker *L, void *handle)
{
    bool unloadedAnyObj = false;

    LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "unloadNativeObj: %p\n", handle));

    ObjectCode *prev = NULL, *next;
    for (ObjectCode *nc = L->gc.loaded_objects; nc; nc = next) {
        next = nc->next_loaded_object; // we might move nc

        if (nc->type == DYNAMIC_OBJECT && nc->dlopen_handle == handle) {
            nc->status = OBJECT_UNLOADED;
            L->gc.n_unloaded_objects += 1;

            // dynamic objects have no symbols
            LL_CHECK_FOR(L, nc->symbols == NULL);
            if (L->services.host.on_unload != NULL) {
                L->services.host.on_unload(L->services.host.ctx, nc);
            }

            // Remove object code from root set
            if (prev == NULL) {
              L->gc.loaded_objects = nc->next_loaded_object;
            } else {
              prev->next_loaded_object = nc->next_loaded_object;
            }
            unloadedAnyObj = true;
        } else {
            prev = nc;
        }
    }

    if (unloadedAnyObj) {
        return 1;
    } else {
        LL_LOG_ERROR_FOR(L, "unloadObjNativeObj_: can't find `%p' to unload", handle);
        return 0;
    }
}

ll_hs_int unloadNativeObj(void *handle) {
  ll_mutex_lock(&ll_default_linker.linker_mutex);
  ll_hs_int r = unloadNativeObj_(&ll_default_linker, handle);
  ll_mutex_unlock(&ll_default_linker.linker_mutex);
  return r;
}

/* -----------------------------------------------------------------------------
 * Segment management
 */
void
initSegment (ll_linker *L, Segment *s, void *start, size_t size,
             SegmentProt prot, int n_sections)
{
    s->start = start;
    s->size = size;
    s->prot = prot;
    s->sections_idx = (int *)LL_CALLOC_FOR(L, n_sections, sizeof(int),
                                               "initSegment(segment)");
    s->n_sections = n_sections;
}

void freeSegments (ObjectCode *oc)
{
    if (oc->segments != NULL) {
        LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "freeing %d segments\n", oc->n_segments));

        for (int i = 0; i < oc->n_segments; i++) {
            Segment *s = &oc->segments[i];

            LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "freeing segment %d at %p size %zu\n",
                                          i, s->start, s->size));

            LL_FREE_FOR(oc->L, s->sections_idx);
            s->sections_idx = NULL;

            if (0 == s->size) {
                LL_IF_DEBUG_FOR(oc->L, linker, ocDebugBelch(oc, "skipping segment of 0 size\n"));
                continue;
            } else {
#if RTS_LINKER_USE_MMAP
                ll_munmap_for_linker(oc->L, s->start, s->size, "freeSegments");
#else
                LL_FREE_FOR(oc->L, s->start);
#endif
            }
            s->start = NULL;
        }

        LL_FREE_FOR(oc->L, oc->segments);
        oc->segments = NULL;
    }
}
