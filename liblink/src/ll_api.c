/* Public entry points are inert until their instance-based implementation. */
#include "liblink.h"
#include "ll_visibility.h"

LL_FRIEND_API ll_linker *ll_new(const ll_config *cfg, const ll_host_ops *host,
                               const ll_mem_ops *mem)
{
    (void)cfg; (void)host; (void)mem;
    return NULL;
}

LL_FRIEND_API void ll_destroy(ll_linker *L)
{
    (void)L;
}

LL_FRIEND_API const char *ll_last_error(const ll_linker *L)
{
    (void)L;
    return "liblink handle API is not implemented";
}

LL_FRIEND_API ll_status ll_define_symbols(ll_linker *L, const ll_pathchar *origin,
                                         const ll_symbol_def *defs, size_t n)
{
    (void)L; (void)origin; (void)defs; (void)n;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_add_resolver(ll_linker *L, ll_resolver_fn fn,
                                       void *ctx, int prio)
{
    (void)L; (void)fn; (void)ctx; (void)prio;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_load_object(ll_linker *L, const ll_pathchar *path,
                                      ll_object **out)
{
    (void)L; (void)path;
    if (out != NULL) *out = NULL;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_load_object_mem(ll_linker *L, const ll_pathchar *name,
                                          const void *buf, size_t len,
                                          ll_object **out)
{
    (void)L; (void)name; (void)buf; (void)len;
    if (out != NULL) *out = NULL;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_load_archive(ll_linker *L, const ll_pathchar *path)
{
    (void)L; (void)path;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_load_native(ll_linker *L, const ll_pathchar *path,
                                      void **handle)
{
    (void)L; (void)path;
    if (handle != NULL) *handle = NULL;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_resolve(ll_linker *L)
{
    (void)L;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_lookup(ll_linker *L, const char *name, void **addr,
                                 ll_sym_type *type)
{
    (void)L; (void)name;
    if (addr != NULL) *addr = NULL;
    if (type != NULL) *type = (ll_sym_type)0;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_lookup_native(ll_linker *L, void *handle,
                                        const char *name, void **addr)
{
    (void)L; (void)handle; (void)name;
    if (addr != NULL) *addr = NULL;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_unload(ll_linker *L, const ll_pathchar *path,
                                 bool purge_only)
{
    (void)L; (void)path; (void)purge_only;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_unload_native(ll_linker *L, void *handle)
{
    (void)L; (void)handle;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_obj_status ll_object_status(ll_linker *L, const ll_pathchar *path)
{
    (void)L; (void)path;
    return LL_OBJ_NOT_LOADED;
}

LL_FRIEND_API const ll_range *ll_index_freeze(ll_linker *L, size_t *n)
{
    (void)L;
    if (n != NULL) *n = 0;
    return NULL;
}

LL_FRIEND_API ll_object *ll_roots_next(ll_linker *L, ll_object *prev)
{
    (void)L; (void)prev;
    return NULL;
}

LL_FRIEND_API ll_object *ll_objects_next(ll_linker *L, ll_object *prev)
{
    (void)L; (void)prev;
    return NULL;
}

LL_FRIEND_API void ll_object_iter_deps(ll_object *o,
                                      bool (*fn)(void *ctx, ll_object *dep),
                                      void *ctx)
{
    (void)o; (void)fn; (void)ctx;
}

LL_FRIEND_API uintptr_t *ll_object_mark_word(ll_object *o)
{
    (void)o;
    return NULL;
}

LL_FRIEND_API bool ll_object_unloadable(const ll_object *o)
{
    (void)o;
    return false;
}

LL_FRIEND_API ll_obj_status ll_object_state(const ll_object *o)
{
    (void)o;
    return LL_OBJ_NOT_LOADED;
}

LL_FRIEND_API void **ll_object_user_data(ll_object *o)
{
    (void)o;
    return NULL;
}

LL_FRIEND_API void ll_object_free(ll_linker *L, ll_object *o)
{
    (void)L; (void)o;
}

LL_FRIEND_API void ll_lock(ll_linker *L)
{
    (void)L;
}

LL_FRIEND_API void ll_unlock(ll_linker *L)
{
    (void)L;
}

LL_FRIEND_API ll_status ll_win_add_search_path(ll_linker *L,
                                              const ll_pathchar *dir,
                                              void **cookie)
{
    (void)L; (void)dir;
    if (cookie != NULL) *cookie = NULL;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_status ll_win_remove_search_path(ll_linker *L, void *cookie)
{
    (void)L; (void)cookie;
    return LL_E_UNSUPPORTED;
}

LL_FRIEND_API ll_pathchar *ll_win_find_system_library(ll_linker *L,
                                                    const ll_pathchar *name)
{
    (void)L; (void)name;
    return NULL;
}

LL_FRIEND_API ll_pathchar *ll_symbolize(ll_linker *L, ll_pathchar *buf, int n,
                                       void *addr, uintptr_t *top)
{
    (void)L; (void)buf; (void)n; (void)addr;
    if (top != NULL) *top = 0;
    return NULL;
}

LL_FRIEND_API size_t ll_object_sections(const ll_object *o, ll_section_info *out,
                                       size_t cap)
{
    (void)o; (void)out; (void)cap;
    return 0;
}

LL_FRIEND_API ll_status ll_object_symbol(const ll_object *o, const char *name,
                                        uintptr_t *exec_addr)
{
    (void)o; (void)name;
    if (exec_addr != NULL) *exec_addr = 0;
    return LL_E_UNSUPPORTED;
}
