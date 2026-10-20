/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Private host contract used while extracting the runtime linker.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>

struct _ObjectCode;

enum { LL_LOG_ERROR = 1, LL_LOG_SYSERROR = 2, LL_LOG_DEBUG = 4 };
enum { LL_DBG_LINKER = 1u << 0, LL_DBG_LINKER_VERBOSE = 1u << 1,
       LL_DBG_SANITY = 1u << 2, LL_DBG_ZERO_ON_FREE = 1u << 3 };

/* The linker supplies the insertion callback; the host supplies its builtins.
 * Symbol type and strength retain their existing integer representations. */
typedef void (*ll_builtin_fn)(void *ctx, const char *name, void *addr,
                              int strength, int type);

typedef struct ll_host_ops {
    void *ctx;
    void *(*malloc_)(void *ctx, size_t n, const char *what);
    void *(*calloc_)(void *ctx, size_t n, size_t size, const char *what);
    void *(*realloc_)(void *ctx, void *p, size_t n, const char *what);
    void (*free_)(void *ctx, void *p);
    void (*log)(void *ctx, int kind, const char *fmt, va_list ap);
    void (*fatal)(void *ctx, const char *fmt, va_list ap); /* must not return */
    void (*on_mem_failure)(void *ctx);
    size_t (*page_size)(void *ctx);
    void (*flush_icache)(void *ctx, void *p, size_t len);

    /* Acquire vectors for each initializer/finalizer pass; argv is borrowed.
     * Release envp with its returned count when free_prog_env is supplied.
     * A NULL release callback means envp is borrowed. */
    void (*prog_args)(void *ctx, int *argc, char ***argv,
                      int *envc, char ***envp);
    void (*free_prog_env)(void *ctx, int envc, char **envp);

    /* Nullable lifecycle services are called only when supplied by the host. */
    void (*before_init)(void *ctx, struct _ObjectCode *o);
    void (*after_init)(void *ctx, struct _ObjectCode *o, bool ok);
    void (*after_init_batch)(void *ctx);
    void (*before_native_load)(void *ctx, struct _ObjectCode *o);
    void (*after_native_load)(void *ctx, struct _ObjectCode *o, bool ok);
    void (*on_unload)(void *ctx, struct _ObjectCode *o);
    void (*object_registered)(void *ctx, struct _ObjectCode *o);
    void (*init_unload_check)(void *ctx);
    void (*exit_unload_check)(void *ctx);
    void (*populate_builtins)(void *ctx, int retain_cafs,
                             ll_builtin_fn insert, void *insert_ctx);
} ll_host_ops;

typedef struct ll_config {
    const void *near_hint;
    uintptr_t mem_base;
    bool always_pic;
    bool optimistic;
    unsigned debug_mask;
} ll_config;

/* Stage 1 retains one process-wide linker. Installation copies both structures
 * during RTS startup, before any linker allocation or adjustor mapping. */
extern ll_host_ops ll_host;
extern ll_config ll_cfg;
void ll_install_host(const ll_host_ops *host, const ll_config *cfg);
