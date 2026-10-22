/* liblink.h: in-memory object linker. C99. Includes no GHC/RTS headers. */
#pragma once
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#ifdef __cplusplus
extern "C" {
#endif
#define LL_API_VERSION 1
#if defined(_WIN32)
typedef wchar_t ll_pathchar;
#else
typedef char    ll_pathchar;
#endif

typedef struct ll_linker ll_linker;   /* one linker instance; owns all state          */
typedef struct ll_object ll_object;   /* loaded object / archive member / native lib  */

typedef enum {
  LL_OK = 0,
  LL_E_IO,          /* open/stat/read/mmap of input failed                   */
  LL_E_FORMAT,      /* malformed or unrecognised object/archive              */
  LL_E_UNSUPPORTED, /* recognised, but format/arch/relocation unsupported    */
  LL_E_DUPSYM,      /* duplicate strong definition                           */
  LL_E_UNRESOLVED,  /* undefined symbol during resolve                       */
  LL_E_RELOC_RANGE, /* relocation overflow, no island possible               */
  LL_E_NOMEM,       /* allocation / address-space reservation failed         */
  LL_E_INIT,        /* initializer failed or hook vetoed                     */
  LL_E_NOT_FOUND,   /* unknown path / handle / symbol                        */
  LL_E_STATE        /* API misuse (wrong object state, re-entrancy)          */
} ll_status;

/* Values MUST equal SymType/SymStrength (rts/RtsSymbols.h) and OStatus
   (rts/include/rts/Linker.h); static-asserted in rts/LinkerGlue.c.        */
typedef enum { LL_SYM_CODE = 1<<0, LL_SYM_DATA = 1<<1, LL_SYM_INDIRECT_DATA = 1<<2,
               LL_SYM_DUP_DISCARD = 1<<3, LL_SYM_HIDDEN = 1<<4 } ll_sym_type;
typedef enum { LL_STRENGTH_NORMAL = 0, LL_STRENGTH_WEAK = 1, LL_STRENGTH_STRONG = 2 } ll_strength;
typedef enum { LL_OBJ_LOADED, LL_OBJ_NEEDED, LL_OBJ_RESOLVED, LL_OBJ_READY,
               LL_OBJ_UNLOADED, LL_OBJ_DONT_RESOLVE, LL_OBJ_NOT_LOADED } ll_obj_status;

typedef struct { const char *name; void *addr; ll_strength strength; ll_sym_type type; } ll_symbol_def;

/* ---- host services (each optional; NULL = built-in libc default) -------- */
enum { LL_LOG_ERROR = 1, LL_LOG_SYSERROR = 2, LL_LOG_DEBUG = 4 };
enum { LL_DBG_LINKER = 1u<<0, LL_DBG_LINKER_VERBOSE = 1u<<1,
       LL_DBG_SANITY = 1u<<2, LL_DBG_ZERO_ON_FREE = 1u<<3 };

typedef struct ll_host_ops {
  void *ctx;
  /* bookkeeping memory (NOT section memory: see ll_mem_ops); never returns NULL */
  void *(*malloc_)(void *ctx, size_t n, const char *what);
  void *(*calloc_)(void *ctx, size_t n, size_t sz, const char *what);
  void *(*realloc_)(void *ctx, void *p, size_t n, const char *what);
  void  (*free_)(void *ctx, void *p);
  void  (*log)(void *ctx, int kind, const char *fmt, va_list ap); /* debug/error/sysErrorBelch */
  void  (*fatal)(void *ctx, const char *fmt, va_list ap);          /* invariant broken; no return */
  void  (*on_mem_failure)(void *ctx);                              /* RTS: reportMemoryMap        */
  void  (*prog_args)(void *ctx, int *argc, char ***argv, int *envc, char ***envp); /* for init(argc,argv,envp) */
  /* argv is borrowed; release an owned envp with the returned count.
     A NULL release callback means the environment is borrowed. */
  void  (*free_prog_env)(void *ctx, int envc, char **envp);
  /* lifecycle hooks: called WITH the linker lock held (§5.7) */
  void  (*before_init)(void *ctx, ll_object *o);            /* RTS: foreignExportsLoadingObject    */
  void  (*after_init)(void *ctx, ll_object *o, bool ok);    /* RTS: foreignExportsFinishedLoading… */
  void  (*after_init_batch)(void *ctx);                     /* RTS: refreshProfilingCCSs (prof)    */
  void  (*before_native_load)(void *ctx, ll_object *o);     /* RTS: fe-loading; ACQUIRE ccs_mutex  */
  void  (*after_native_load)(void *ctx, ll_object *o, bool ok); /* RTS: RELEASE ccs_mutex; fe-done */
  void  (*on_unload)(void *ctx, ll_object *o);              /* RTS: free foreign-export StablePtrs */
} ll_host_ops;

/* ---- section memory and placement --------------------------------------- */
enum { LL_PROT_NONE = 0, LL_PROT_R = 1, LL_PROT_W = 2, LL_PROT_X = 4 };
enum { LL_RESERVE_NEAR = 1u<<0,   /* must be reachable from `near` (x86_64 ±2GiB, aarch64 ±4GiB) */
       LL_RESERVE_EXEC = 1u<<1 }; /* will become executable (MAP_JIT etc.)                       */
typedef struct ll_mem_ops {
  void   *ctx;
  void   *(*reserve)(void *ctx, size_t len, const void *near, unsigned flags, unsigned prot); /* NULL = fail */
  int     (*protect)(void *ctx, void *p, size_t len, unsigned prot);  /* 0 = ok */
  void    (*release)(void *ctx, void *p, size_t len);
  void    (*flush_icache)(void *ctx, void *p, size_t len);
  size_t  (*page_size)(void *ctx);
} ll_mem_ops;
typedef const void *(*ll_near_fn)(void *ctx, const ll_object *o);  /* default: cfg.near_hint */

typedef struct ll_config {
  unsigned    api_version;   /* = LL_API_VERSION                                  */
  const void *near_hint;     /* RTS: &stg_upd_frame_info (old LINKER_LOAD_BASE)   */
  ll_near_fn  near_fn; void *near_ctx;
  uintptr_t   mem_base;      /* RtsFlags.MiscFlags.linkerMemBase (-xm); 0 = none  */
  bool        always_pic;    /* RtsFlags.MiscFlags.linkerAlwaysPic                */
  bool        optimistic;    /* RtsFlags.MiscFlags.linkerOptimistic               */
  unsigned    debug_mask;    /* LL_DBG_*                                          */
  unsigned    flags;         /* LL_CFG_*                                          */
  uintptr_t   noexec_base;   /* LL_CFG_NO_EXEC: first synthetic exec address      */
} ll_config;
enum { LL_CFG_NO_EXEC     = 1u<<0,  /* sections in plain RW host memory, synthetic exec addrs;
                                       never mprotect, run init/fini, or dlopen            */
       LL_CFG_ALL_FORMATS = 1u<<1 };/* accept non-host formats/arches (needs NO_EXEC)      */

/* ---- lifecycle ----------------------------------------------------------- */
ll_linker  *ll_new(const ll_config *cfg, const ll_host_ops *host, const ll_mem_ops *mem); /* NULL = fail */
void        ll_destroy(ll_linker *L);       /* frees all; runs no finalizers of live objects */
const char *ll_last_error(const ll_linker *L); /* valid until the next call on L              */

/* ---- symbol sources ------------------------------------------------------ */
/* defs is BORROWED (not copied): caller guarantees lifetime >= L (rtsSyms is static). */
ll_status ll_define_symbols(ll_linker *L, const ll_pathchar *origin, const ll_symbol_def *defs, size_t n);
/* Fallback resolvers after the linker's own table, ascending prio; called with lock held,
   must not call ll_* on L. Built-ins (unless NO_EXEC): prio 100 platform compat
   (glibc stat family, _DYNAMIC, __dso_handle, FreeBSD iconv…), prio 200 dlsym(program)
   then native objects in load order (Note [RTLD_LOCAL]).                                 */
typedef void *(*ll_resolver_fn)(void *ctx, const char *name, ll_sym_type *type_out);
ll_status ll_add_resolver(ll_linker *L, ll_resolver_fn fn, void *ctx, int prio);

/* ---- loading ------------------------------------------------------------- */
ll_status ll_load_object    (ll_linker *L, const ll_pathchar *path, ll_object **out);
ll_status ll_load_object_mem(ll_linker *L, const ll_pathchar *name, const void *buf, size_t len,
                             ll_object **out);                                /* copies buf */
ll_status ll_load_archive   (ll_linker *L, const ll_pathchar *path);          /* lazy members */
ll_status ll_load_native    (ll_linker *L, const ll_pathchar *path, void **handle);
ll_status ll_resolve        (ll_linker *L);  /* resolve all NEEDED, then run pending initializers */
ll_status ll_lookup         (ll_linker *L, const char *name, void **addr, ll_sym_type *type); /* may lazy-load */
ll_status ll_lookup_native  (ll_linker *L, void *handle, const char *name, void **addr);
ll_status ll_unload         (ll_linker *L, const ll_pathchar *path, bool purge_only); /* memory freed later by GC */
ll_status ll_unload_native  (ll_linker *L, void *handle);
ll_obj_status ll_object_status(ll_linker *L, const ll_pathchar *path);

/* ---- GC / unloading support (RTS owns the policy) ------------------------ */
typedef struct { uintptr_t start, end; ll_object *obj; } ll_range;
const ll_range *ll_index_freeze(ll_linker *L, size_t *n);  /* sorted, compacted; stable until next load/free */
ll_object  *ll_roots_next  (ll_linker *L, ll_object *prev); /* old loaded_objects; NULL ends */
ll_object  *ll_objects_next(ll_linker *L, ll_object *prev); /* old objects list              */
void        ll_object_iter_deps(ll_object *o, bool (*fn)(void *ctx, ll_object *dep), void *ctx);
uintptr_t  *ll_object_mark_word(ll_object *o);             /* RTS-owned word; RTS uses atomic xchg */
bool        ll_object_unloadable(const ll_object *o);
ll_obj_status ll_object_state(const ll_object *o);
void      **ll_object_user_data(ll_object *o);             /* RTS: ForeignExportsList*, GC links */
void        ll_object_free(ll_linker *L, ll_object *o);    /* UNLOADED + unreachable only; runs fini if init ran, then __cxa_finalize */
void        ll_lock(ll_linker *L);
void        ll_unlock(ll_linker *L);

/* ---- Windows-only (LL_E_UNSUPPORTED / NULL elsewhere) -------------------- */
ll_status    ll_win_add_search_path(ll_linker *L, const ll_pathchar *dir, void **cookie);
ll_status    ll_win_remove_search_path(ll_linker *L, void *cookie);
ll_pathchar *ll_win_find_system_library(ll_linker *L, const ll_pathchar *name); /* malloc'd */
ll_pathchar *ll_symbolize(ll_linker *L, ll_pathchar *buf, int n, void *addr, uintptr_t *top);

/* ---- test / introspection ------------------------------------------------ */
typedef struct { const char *section; uintptr_t exec_addr; void *work_addr; size_t size; unsigned prot; } ll_section_info;
size_t    ll_object_sections(const ll_object *o, ll_section_info *out, size_t cap);
ll_status ll_object_symbol(const ll_object *o, const char *name, uintptr_t *exec_addr);
#ifdef __cplusplus
}
#endif
