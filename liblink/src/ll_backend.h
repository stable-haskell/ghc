/* Internal format and relocation contracts; addresses may differ from storage. */
#pragma once

#include "liblink.h"

typedef struct _ObjectCode ObjectCode;
typedef struct _Section Section;

typedef struct { uint8_t *work; uintptr_t exec; } ll_site;

/* Borrowed, immutable input for one synchronous apply call; never retain it.
 * The index and raw type describe the referencing file's symbol entry, not
 * the resolved provider. The name is its existing diagnostic string (possibly
 * NULL where the original relocation has no name). */
typedef struct {
    size_t symbol_index;
    const char *symbol_name;
    uint32_t format_symbol_type;
} ll_reloc_context;

struct ll_reloc_ops {
    uint16_t machine;
    int (*apply)(ObjectCode *oc, Section *s, uint32_t type, ll_site P,
                 uintptr_t S, int64_t A, ll_sym_type st,
                 const ll_reloc_context *context);
    size_t extra_bytes_per_symbol;
};

struct ll_format_ops {
    const char *name;
    bool (*sniff)(const uint8_t *img, size_t len);
    int (*verify)(ObjectCode *);
    int (*init)(ObjectCode *);
    int (*alloc_extras)(ObjectCode *);
    int (*get_names)(ObjectCode *);
    int (*resolve)(ObjectCode *);
    int (*run_init)(ObjectCode *);
    int (*run_fini)(ObjectCode *);
    void (*deinit)(ObjectCode *);
};

/* Native sites share work/exec addresses. NO_EXEC stores through work and
 * performs PC-relative arithmetic with synthetic exec addresses, including
 * GOT/PLT entries and branch islands. */
