/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * RTS services for the extracted runtime linker.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsSymbols.h"
#include "ll_compat.h"
#include "LinkerGlue.h"
#include "RtsUtils.h"
#include "ReportMemoryMap.h"
#include "sm/OSMem.h"
#include "GetEnv.h"
#include "ForeignExports.h"
#include "Profiling.h"
#include "StablePtr.h"
#include "CheckUnload.h"
#include "ll_host.h"

_Static_assert(sizeof(ll_word) == sizeof(StgWord), "linker word size mismatch");
_Static_assert(sizeof(void *) == SIZEOF_VOID_P, "configured pointer size mismatch");
_Static_assert(_Alignof(ll_word) == _Alignof(StgWord), "linker word alignment mismatch");
_Static_assert(((ll_word)-1) > (ll_word)0, "linker word must be unsigned");
_Static_assert(sizeof(ll_int) == sizeof(StgInt), "linker integer size mismatch");
_Static_assert(_Alignof(ll_int) == _Alignof(StgInt), "linker integer alignment mismatch");
_Static_assert(((ll_int)-1) < (ll_int)0, "linker integer must be signed");
_Static_assert(sizeof(ll_int8) == sizeof(StgInt8), "linker int8 size mismatch");
_Static_assert(_Alignof(ll_int8) == _Alignof(StgInt8), "linker int8 alignment mismatch");
_Static_assert(((ll_int8)-1) < (ll_int8)0, "linker int8 must be signed");
_Static_assert(sizeof(ll_word8) == sizeof(StgWord8), "linker word8 size mismatch");
_Static_assert(_Alignof(ll_word8) == _Alignof(StgWord8), "linker word8 alignment mismatch");
_Static_assert(((ll_word8)-1) > (ll_word8)0, "linker word8 must be unsigned");
_Static_assert(sizeof(ll_int16) == sizeof(StgInt16), "linker int16 size mismatch");
_Static_assert(_Alignof(ll_int16) == _Alignof(StgInt16), "linker int16 alignment mismatch");
_Static_assert(((ll_int16)-1) < (ll_int16)0, "linker int16 must be signed");
_Static_assert(sizeof(ll_word16) == sizeof(StgWord16), "linker word16 size mismatch");
_Static_assert(_Alignof(ll_word16) == _Alignof(StgWord16), "linker word16 alignment mismatch");
_Static_assert(((ll_word16)-1) > (ll_word16)0, "linker word16 must be unsigned");
_Static_assert(sizeof(ll_int32) == sizeof(StgInt32), "linker int32 size mismatch");
_Static_assert(_Alignof(ll_int32) == _Alignof(StgInt32), "linker int32 alignment mismatch");
_Static_assert(((ll_int32)-1) < (ll_int32)0, "linker int32 must be signed");
_Static_assert(sizeof(ll_word32) == sizeof(StgWord32), "linker word32 size mismatch");
_Static_assert(_Alignof(ll_word32) == _Alignof(StgWord32), "linker word32 alignment mismatch");
_Static_assert(((ll_word32)-1) > (ll_word32)0, "linker word32 must be unsigned");
_Static_assert(sizeof(ll_int64) == sizeof(StgInt64), "linker int64 size mismatch");
_Static_assert(_Alignof(ll_int64) == _Alignof(StgInt64), "linker int64 alignment mismatch");
_Static_assert(((ll_int64)-1) < (ll_int64)0, "linker int64 must be signed");
_Static_assert(sizeof(ll_word64) == sizeof(StgWord64), "linker word64 size mismatch");
_Static_assert(_Alignof(ll_word64) == _Alignof(StgWord64), "linker word64 alignment mismatch");
_Static_assert(((ll_word64)-1) > (ll_word64)0, "linker word64 must be unsigned");
_Static_assert(sizeof(ll_hs_int) == sizeof(HsInt), "linker HsInt size mismatch");
_Static_assert(_Alignof(ll_hs_int) == _Alignof(HsInt), "linker HsInt alignment mismatch");
_Static_assert(((ll_hs_int)-1) < (ll_hs_int)0, "linker HsInt must be signed");
_Static_assert(sizeof(ll_hs_bool) == sizeof(HsBool), "linker HsBool size mismatch");
_Static_assert(_Alignof(ll_hs_bool) == _Alignof(HsBool), "linker HsBool alignment mismatch");
_Static_assert(((ll_hs_bool)-1) < (ll_hs_bool)0, "linker HsBool must be signed");
_Static_assert(sizeof(ll_hs_ptr) == sizeof(HsPtr), "linker HsPtr size mismatch");
_Static_assert(_Alignof(ll_hs_ptr) == _Alignof(HsPtr), "linker HsPtr alignment mismatch");
_Static_assert(sizeof(ll_hs_stable_ptr) == sizeof(HsStablePtr), "linker stable pointer size mismatch");
_Static_assert(_Alignof(ll_hs_stable_ptr) == _Alignof(HsStablePtr), "linker stable pointer alignment mismatch");
_Static_assert(sizeof(ll_stg_ptr) == sizeof(StgPtr), "linker StgPtr size mismatch");
_Static_assert(_Alignof(ll_stg_ptr) == _Alignof(StgPtr), "linker StgPtr alignment mismatch");
_Static_assert(sizeof(ll_stg_bool) == sizeof(StgBool), "linker StgBool size mismatch");
_Static_assert(_Alignof(ll_stg_bool) == _Alignof(StgBool), "linker StgBool alignment mismatch");
_Static_assert(((ll_stg_bool)-1) < (ll_stg_bool)0, "linker StgBool must be signed");
_Static_assert(sizeof(ll_w) == sizeof(W_), "linker W_ size mismatch");
_Static_assert(_Alignof(ll_w) == _Alignof(W_), "linker W_ alignment mismatch");
_Static_assert(((ll_w)-1) > (ll_w)0, "linker W_ must be unsigned");
_Static_assert(sizeof(ll_symbol_name) == sizeof(SymbolName), "linker symbol name size mismatch");
_Static_assert(_Alignof(ll_symbol_name) == _Alignof(SymbolName), "linker symbol name alignment mismatch");
_Static_assert(sizeof(ll_symbol_addr *) == sizeof(SymbolAddr *), "linker symbol address pointer size mismatch");
_Static_assert(_Alignof(ll_symbol_addr *) == _Alignof(SymbolAddr *), "linker symbol address pointer alignment mismatch");

_Static_assert(sizeof(ll_sym_type) == sizeof(SymType), "linker symbol type representation mismatch");
_Static_assert(_Alignof(ll_sym_type) == _Alignof(SymType), "linker symbol type alignment mismatch");
_Static_assert(((ll_sym_type)-1 < (ll_sym_type)0) == ((SymType)-1 < (SymType)0), "linker symbol type signedness mismatch");
_Static_assert((int)LL_SYM_CODE == (int)SYM_TYPE_CODE, "LL_SYM_CODE mismatch");
_Static_assert((int)LL_SYM_DATA == (int)SYM_TYPE_DATA, "LL_SYM_DATA mismatch");
_Static_assert((int)LL_SYM_INDIRECT_DATA == (int)SYM_TYPE_INDIRECT_DATA, "LL_SYM_INDIRECT_DATA mismatch");
_Static_assert((int)LL_SYM_DUP_DISCARD == (int)SYM_TYPE_DUP_DISCARD, "LL_SYM_DUP_DISCARD mismatch");
_Static_assert((int)LL_SYM_HIDDEN == (int)SYM_TYPE_HIDDEN, "LL_SYM_HIDDEN mismatch");
_Static_assert(sizeof(ll_strength) == sizeof(SymStrength), "linker strength representation mismatch");
_Static_assert(_Alignof(ll_strength) == _Alignof(SymStrength), "linker strength alignment mismatch");
_Static_assert(((ll_strength)-1 < (ll_strength)0) == ((SymStrength)-1 < (SymStrength)0), "linker strength signedness mismatch");
_Static_assert((int)LL_STRENGTH_NORMAL == (int)STRENGTH_NORMAL, "LL_STRENGTH_NORMAL mismatch");
_Static_assert((int)LL_STRENGTH_WEAK == (int)STRENGTH_WEAK, "LL_STRENGTH_WEAK mismatch");
_Static_assert((int)LL_STRENGTH_STRONG == (int)STRENGTH_STRONG, "LL_STRENGTH_STRONG mismatch");

_Static_assert(sizeof(ll_obj_status) == sizeof(OStatus), "linker object status size mismatch");
_Static_assert(_Alignof(ll_obj_status) == _Alignof(OStatus), "linker object status alignment mismatch");
_Static_assert(((ll_obj_status)-1 < (ll_obj_status)0) == ((OStatus)-1 < (OStatus)0), "linker object status signedness mismatch");
_Static_assert((int)LL_OBJ_LOADED == (int)OBJECT_LOADED, "LL_OBJ_LOADED mismatch");
_Static_assert((int)LL_OBJ_NEEDED == (int)OBJECT_NEEDED, "LL_OBJ_NEEDED mismatch");
_Static_assert((int)LL_OBJ_RESOLVED == (int)OBJECT_RESOLVED, "LL_OBJ_RESOLVED mismatch");
_Static_assert((int)LL_OBJ_READY == (int)OBJECT_READY, "LL_OBJ_READY mismatch");
_Static_assert((int)LL_OBJ_UNLOADED == (int)OBJECT_UNLOADED, "LL_OBJ_UNLOADED mismatch");
_Static_assert((int)LL_OBJ_DONT_RESOLVE == (int)OBJECT_DONT_RESOLVE, "LL_OBJ_DONT_RESOLVE mismatch");
_Static_assert((int)LL_OBJ_NOT_LOADED == (int)OBJECT_NOT_LOADED, "LL_OBJ_NOT_LOADED mismatch");

static void *host_malloc(void *ctx STG_UNUSED, size_t n, const char *what)
{
    return stgMallocBytes(n, (char *)what);
}

static void *host_calloc(void *ctx STG_UNUSED, size_t n, size_t size,
                         const char *what)
{
    return stgCallocBytes(n, size, (char *)what);
}

static void *host_realloc(void *ctx STG_UNUSED, void *p, size_t n,
                          const char *what)
{
    return stgReallocBytes(p, n, (char *)what);
}

static void host_free(void *ctx STG_UNUSED, void *p)
{
    stgFree(p);
}

static void host_log(void *ctx STG_UNUSED, int kind, const char *fmt, va_list ap)
{
    switch (kind) {
    case LL_LOG_DEBUG:    vdebugBelch(fmt, ap); break;
    case LL_LOG_SYSERROR: vsysErrorBelch(fmt, ap); break;
    default:             verrorBelch(fmt, ap); break;
    }
}

static void host_fatal(void *ctx STG_UNUSED, const char *fmt, va_list ap)
{
    vbarf(fmt, ap);
}

static void host_mem_failure(void *ctx STG_UNUSED)
{
    reportMemoryMap();
}

static size_t host_page_size(void *ctx STG_UNUSED)
{
    return getPageSize();
}

static void host_flush_icache(void *ctx STG_UNUSED, void *p, size_t len)
{
    flushExec(len, p);
}

static void host_prog_args(void *ctx STG_UNUSED, int *argc, char ***argv,
                           int *envc, char ***envp)
{
    getProgArgv(argc, argv);
    getProgEnvv(envc, envp);
}

static void host_free_prog_env(void *ctx STG_UNUSED, int envc, char **envp)
{
    freeProgEnvv(envc, envp);
}

static void host_before_init(void *ctx STG_UNUSED, ObjectCode *oc)
{
    foreignExportsLoadingObject(oc);
}

static void host_after_init(void *ctx STG_UNUSED, ObjectCode *oc STG_UNUSED,
                            bool ok STG_UNUSED)
{
    foreignExportsFinishedLoadingObject();
}

#if defined(PROFILING)
static void host_after_init_batch(void *ctx STG_UNUSED)
{
    refreshProfilingCCSs();
}
#endif

static void host_before_native_load(void *ctx STG_UNUSED, ObjectCode *oc)
{
    foreignExportsLoadingObject(oc);

    // When dlopen() loads a profiled dynamic library, it calls the ctors which
    // will call registerCcsList() to append the defined CostCentreStacks to
    // CCS_LIST. However, another thread may be doing other things with the RTS
    // linker that transitively calls refreshProfilingCCSs() which also accesses
    // CCS_LIST. So there's a risk of data race that may lead to segfaults
    // (#24423), and we need to ensure the ctors are also protected by
    // ccs_mutex.
#if defined(PROFILING)
    ACQUIRE_LOCK(&ccs_mutex);
#endif
}

static void host_after_native_load(void *ctx STG_UNUSED,
                                   ObjectCode *oc STG_UNUSED,
                                   bool ok STG_UNUSED)
{
#if defined(PROFILING)
    RELEASE_LOCK(&ccs_mutex);
#endif
    foreignExportsFinishedLoadingObject();
}

/*
 * Release StablePtrs and free oc->stable_ptrs.
 * This operation is idempotent.
 */
static void host_on_unload(void *ctx STG_UNUSED, ObjectCode *oc)
{
    // Release any StablePtrs that were created when this
    // object module was initialized.
    struct ForeignExportsList *exports, *next;

    for (exports = oc->foreign_exports; exports != NULL; exports = next) {
        next = exports->next;
        for (int i = 0; i < exports->n_entries; i++) {
            freeStablePtr(exports->stable_ptrs[i]);
        }
        // processForeignExports allocates this array with the RTS allocator.
        stgFree(exports->stable_ptrs);
        exports->stable_ptrs = NULL;
        exports->next = NULL;
    }
    oc->foreign_exports = NULL;
}

static void host_object_registered(void *ctx STG_UNUSED, ObjectCode *oc)
{
    insertOCSectionIndices(oc);
}

static void host_init_unload_check(void *ctx STG_UNUSED)
{
    initUnloadCheck();
}

static void host_exit_unload_check(void *ctx STG_UNUSED)
{
    exitUnloadCheck();
}

static void host_populate_builtins(void *ctx STG_UNUSED, int retain_cafs,
                                   ll_builtin_fn insert, void *insert_ctx)
{
    /* populate the symbol table with stuff from the RTS */
    LL_IF_DEBUG(linker, LL_DEBUG("populating linker symbol table with built-in RTS symbols\n"));
    for (const RtsSymbolVal *sym = rtsSyms; sym->lbl != NULL; sym++) {
        LL_IF_DEBUG(linker, LL_DEBUG("initLinker: inserting rts symbol %s, %p\n", sym->lbl, sym->addr));
        insert(insert_ctx, sym->lbl, sym->addr,
               sym->strength, sym->type);
    }
    LL_IF_DEBUG(linker, LL_DEBUG("done with built-in RTS symbols\n"));

    /* Add extra symbols. rtsExtraSyms() is a weakly defined symbol in the rts,
     * that can be overrided by linking in an object with a corresponding
     * definition later. This is useful to build an external-interpreter or some
     * other process with extra symbols (mostly libc, or similar).
     * See Note [Extra RTS symbols]
     */
    LL_IF_DEBUG(linker, LL_DEBUG("populating linker symbol table with extra RTS symbols\n"));
    if(rtsExtraSyms && rtsExtraSyms() != NULL) {
        for(RtsSymbolVal *sym = rtsExtraSyms(); sym->lbl != NULL; sym++) {
            LL_IF_DEBUG(linker, LL_DEBUG("initLinker: inserting extra rts symbol %s, %p\n", sym->lbl, sym->addr));
            insert(insert_ctx, sym->lbl, sym->addr,
                   sym->strength, sym->type);
        }
    }
    LL_IF_DEBUG(linker, LL_DEBUG("done with extra RTS symbols\n"));

    // Redirect newCAF to newRetainedCAF if retain_cafs is true.
    insert(insert_ctx, MAYBE_LEADING_UNDERSCORE_STR("newCAF"),
           retain_cafs ? newRetainedCAF : newGCdCAF,
           HS_BOOL_FALSE, SYM_TYPE_CODE);
}

void ll_rts_install_host(void)
{
    const ll_compat_host_ops host = {
        .malloc_ = host_malloc,
        .calloc_ = host_calloc,
        .realloc_ = host_realloc,
        .free_ = host_free,
        .log = host_log,
        .fatal = host_fatal,
        .on_mem_failure = host_mem_failure,
        .page_size = host_page_size,
        .flush_icache = host_flush_icache,
        .prog_args = host_prog_args,
        .free_prog_env = host_free_prog_env,
        .before_init = host_before_init,
        .after_init = host_after_init,
#if defined(PROFILING)
        .after_init_batch = host_after_init_batch,
#endif
        .before_native_load = host_before_native_load,
        .after_native_load = host_after_native_load,
        .on_unload = host_on_unload,
        .object_registered = host_object_registered,
        .init_unload_check = host_init_unload_check,
        .exit_unload_check = host_exit_unload_check,
        .populate_builtins = host_populate_builtins,
    };
    const ll_compat_config cfg = {
        .near_hint = &stg_upd_frame_info,
        .mem_base = RtsFlags.MiscFlags.linkerMemBase,
        .always_pic = RtsFlags.MiscFlags.linkerAlwaysPic,
        .optimistic = RtsFlags.MiscFlags.linkerOptimistic,
        .debug_mask =
            (RtsFlags.DebugFlags.linker ? LL_DBG_LINKER : 0) |
            (RtsFlags.DebugFlags.linker_verbose ? LL_DBG_LINKER_VERBOSE : 0) |
            (RtsFlags.DebugFlags.sanity ? LL_DBG_SANITY : 0) |
            (RtsFlags.DebugFlags.zero_on_gc ? LL_DBG_ZERO_ON_FREE : 0),
    };
    ll_install_host(&host, &cfg);
    setUnloadLinkerState(ll_compat_default_gc_state());
}
