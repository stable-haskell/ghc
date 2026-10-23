/* Failed resolution objects remain owned until normal unload and disposal. */
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "ll_failure.h"
#include "ll_path.h"

#include <string.h>

static bool find_failed_dependency(void *data, ll_word key,
                                   const void *value)
{
    bool *found = data;
    const ObjectCode *provider = (const ObjectCode *)(uintptr_t)key;
    (void)value;
    if (provider->resolution_failed) {
        *found = true;
        return false;
    }
    return true;
}

static bool mark_dependent(ObjectCode *object)
{
    if (object->resolution_failed || object->dependencies == NULL) return false;
    bool found = false;
    iterHashTable(object->dependencies, &found, find_failed_dependency);
    if (found) object->resolution_failed = true;
    return found;
}

static void mark_closure(ll_linker *L)
{
    /* A marker only changes from false to true, so cycles terminate without
     * allocating a work list or mutating the dependency sets. */
    bool changed;
    do {
        changed = false;
        for (ObjectCode *object = L->gc.objects; object != NULL;
             object = object->next)
            changed |= mark_dependent(object);
        for (ObjectCode *object = L->gc.loaded_objects; object != NULL;
             object = object->next_loaded_object)
            changed |= mark_dependent(object);
        for (struct ll_failure_scope *scope = L->failure_scopes;
             scope != NULL; scope = scope->previous)
            if (scope->object != NULL) changed |= mark_dependent(scope->object);
    } while (changed);
}

static void quarantine(ObjectCode *object)
{
    ll_linker *L = object->L;
    object->resolution_failed = true;
    /* Recursive ancestors can be absent from the registered object lists. */
    for (struct ll_failure_scope *scope = L->failure_scopes;
         scope != NULL; scope = scope->previous)
        if (scope->object != NULL) scope->object->resolution_failed = true;
    mark_closure(L);
    L->quarantine_rejected = true;
}

void ll_failure_begin(ll_linker *L, struct ll_failure_scope *scope,
                      ObjectCode *object)
{
    if (L->failure_scopes == NULL) {
        ll_error_clear(L);
        L->primary_resolution_error = LL_OK;
        L->quarantine_rejected = false;
    }
    scope->object = object;
    scope->previous = L->failure_scopes;
    L->failure_scopes = scope;
}

void ll_failure_end(ll_linker *L, struct ll_failure_scope *scope)
{
    LL_ASSERT_FOR(L, L->failure_scopes == scope);
    if (L->primary_resolution_error != LL_OK) {
        L->error_status = L->primary_resolution_error;
        L->error_from_pre_backend = false;
        memcpy(L->error_message, L->primary_resolution_message,
               sizeof(L->error_message));
    }
    L->failure_scopes = scope->previous;
}

static void record_terminal_failure(ObjectCode *object)
{
    ll_linker *L = object->L;
    if (L->failure_scopes != NULL && L->primary_resolution_error == LL_OK) {
        L->primary_resolution_error = L->error_status;
        memcpy(L->primary_resolution_message, L->error_message,
               sizeof(L->primary_resolution_message));
    }
    quarantine(object);
}

void ll_failure_resolution(ObjectCode *object)
{
    const ll_status status = object->L->error_status;
    /* A missing definition retains its established retry path. All other
     * explicit backend errors can follow partial relocation, including NOMEM;
     * without rollback they require a fresh object rather than in-place retry.
     * A propagated pre-dispatch rejection retains its original retry policy. */
    if (status != LL_OK && status != LL_E_UNRESOLVED &&
        !object->L->error_from_pre_backend)
        record_terminal_failure(object);
}

void ll_failure_protection(ObjectCode *object)
{
    ll_linker *L = object->L;
    if (L->error_status == LL_OK)
        ll_error_set(L, LL_E_IO, "could not finalize object memory protection");
    record_terminal_failure(object);
}

void ll_failure_inherit(ObjectCode *dependent, const ObjectCode *provider)
{
    if (dependent != NULL && provider != NULL && provider->resolution_failed) {
        LL_ASSERT_FOR(dependent->L, dependent->L == provider->L);
        quarantine(dependent);
    }
}

void ll_failure_dependent(ObjectCode *dependent)
{
    if (dependent != NULL && dependent->L->quarantine_rejected)
        quarantine(dependent);
}

bool ll_failure_alias(ObjectCode *dependent, const ObjectCode *provider)
{
    /* COMMON storage aliases bypass symbol lookup. Record the same lifetime
     * dependency so a later provider failure also reaches its aliases. */
    if (dependent != NULL && provider != NULL) {
        LL_ASSERT_FOR(dependent->L, dependent->L == provider->L);
        if (dependent != provider)
            insertHashSet(dependent->dependencies, (ll_w)provider);
    }
    ll_failure_inherit(dependent, provider);
    return ll_failure_usable(dependent) && ll_failure_usable(provider);
}

bool ll_failure_usable(const ObjectCode *object)
{
    if (object == NULL || !object->resolution_failed) return true;
    ll_linker *L = object->L;
    L->quarantine_rejected = true;
    if (L->failure_scopes != NULL && L->primary_resolution_error != LL_OK) {
        L->error_status = L->primary_resolution_error;
        L->error_from_pre_backend = false;
        memcpy(L->error_message, L->primary_resolution_message,
               sizeof(L->error_message));
    } else {
        const pathchar *name = OC_INFORMATIVE_FILENAME(object);
        if (name == NULL) name = WSTR("<unnamed>");
        ll_error_set(L, LL_E_STATE,
                     "object %" PATH_FMT " requires unload and reload after a resolution failure",
                     name);
    }
    return false;
}

bool ll_failure_active(ll_linker *L)
{
    for (ObjectCode *object = L->gc.objects; object != NULL; object = object->next)
        if (object->status != OBJECT_UNLOADED && !ll_failure_usable(object))
            return true;
    for (ObjectCode *object = L->gc.loaded_objects; object != NULL;
         object = object->next_loaded_object)
        if (object->status != OBJECT_UNLOADED && !ll_failure_usable(object))
            return true;
    return false;
}

bool ll_failure_path_usable(ll_linker *L, const ll_pathchar *path)
{
    for (ObjectCode *object = L->gc.loaded_objects; object != NULL;
         object = object->next_loaded_object)
        if (object->status != OBJECT_UNLOADED &&
            pathcmp(object->fileName, path) == 0 && !ll_failure_usable(object))
            return false;
    return true;
}

bool ll_failure_blocked(const ll_linker *L)
{
    return L->quarantine_rejected;
}
