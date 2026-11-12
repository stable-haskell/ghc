/* Private process services required by the portable COFF object backend.
 * Module identities are opaque values obtained from real host services. They
 * are never replacement SDK handles or object target addresses. */
#pragma once
#include "ll_compat.h"
#include "ll_path.h"

typedef struct _ObjectCode ObjectCode;

#include "ll_begin_private.h"

bool ll_pe_host_image_base(ll_linker *L, uintptr_t *base);
ll_symbol_addr *ll_pe_host_lookup_dlls(ll_linker *L, const ll_symbol_name *name,
                                      ObjectCode *dependent);
ll_symbol_addr *ll_pe_host_get_proc(ll_linker *L, void *module,
                                   const ll_symbol_name *name);
bool ll_pe_host_load_bfd_head(ObjectCode *oc, const char *dll_name,
                              ll_symbol_name **symbol, ll_symbol_addr **module);
bool ll_pe_host_register_unwind(ObjectCode *oc, unsigned target_arch,
                                unsigned section_index, bool *empty_table);
void ll_pe_host_unregister_unwind(ObjectCode *oc);

#include "ll_end_private.h"
