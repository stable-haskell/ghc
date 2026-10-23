/* Private handling of terminal resolution and final-protection failures. */
#pragma once

#include "liblink.h"

struct _ObjectCode;

/* Frames live on the caller's stack. A NULL object denotes an API scope. */
struct ll_failure_scope {
    struct _ObjectCode *object;
    struct ll_failure_scope *previous;
};

#include "ll_begin_private.h"

void ll_failure_begin(ll_linker *L, struct ll_failure_scope *scope,
                      struct _ObjectCode *object);
void ll_failure_end(ll_linker *L, struct ll_failure_scope *scope);
/* Call only at a failed backend-resolution boundary, with its current error. */
void ll_failure_resolution(struct _ObjectCode *object);
void ll_failure_protection(struct _ObjectCode *object);
void ll_failure_inherit(struct _ObjectCode *dependent,
                        const struct _ObjectCode *provider);
bool ll_failure_alias(struct _ObjectCode *dependent,
                      const struct _ObjectCode *provider);
void ll_failure_dependent(struct _ObjectCode *dependent);
bool ll_failure_usable(const struct _ObjectCode *object);
bool ll_failure_active(ll_linker *L);
bool ll_failure_path_usable(ll_linker *L, const ll_pathchar *path);
bool ll_failure_blocked(const ll_linker *L);

#include "ll_end_private.h"
