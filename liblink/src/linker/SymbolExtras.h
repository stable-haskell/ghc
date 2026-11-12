#pragma once

#include "LinkerInternals.h"
#include "ll_backend.h"

#include "ll_begin_private.h"

/* Refresh is called only when image replacement changes its address, before
 * count/first/stride are published. Formats with image-relative metadata supply
 * their original initializer, not the format vtable's verify+init wrapper. */
typedef int (*ll_extra_refresh_fn)(ObjectCode *oc);
int ocAllocateExtras(ObjectCode *oc, int count, int first, int bssSize,
                     size_t extra_stride, ll_extra_refresh_fn refresh);
bool ocProtectExtras(ObjectCode* oc);

/* Slots must have the selected payload's size/alignment; their spacing is the
 * recorded per-object stride. These writers retain native-endian field stores.
 * ARM only exposes storage: its existing section PLT owns instruction emission. */
ll_symbol_extra_x86_64 *ll_make_symbol_extra_x86_64(
    ObjectCode const *oc, unsigned long symbolNumber, uint64_t target);
/* Writes the same x64 payload through work; returns its recorded exec address
 * for GOT and island arithmetic. Native pointer writers remain compatible. */
ll_site ll_make_symbol_extra_x86_64_site(
    ObjectCode const *oc, unsigned long symbolNumber, uint64_t target);
ll_symbol_extra_riscv64 *ll_make_symbol_extra_riscv64(
    ObjectCode const *oc, unsigned long symbolNumber, uint64_t target);
/* Stores the fake GOT value in work memory and returns its execution site. */
ll_site ll_make_symbol_extra_riscv64_site(
    ObjectCode const *oc, unsigned long symbolNumber, uint64_t target);
ll_symbol_extra_powerpc *ll_make_symbol_extra_powerpc(
    ObjectCode const *oc, unsigned long symbolNumber, uint32_t target);
ll_symbol_extra_arm *ll_symbol_extra_arm_slot(
    ObjectCode const *oc, unsigned long symbolNumber);

#if defined(NEED_SYMBOL_EXTRAS)
#if defined(arm_HOST_ARCH)
SymbolExtra* makeArmSymbolExtra( ObjectCode const* oc,
                                 unsigned long symbolNumber,
                                 unsigned long target,
                                 bool fromThumb,
                                 bool toThumb );
#elif defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(riscv64_HOST_ARCH)
SymbolExtra* makeSymbolExtra( ObjectCode const* oc,
                              unsigned long symbolNumber,
                              unsigned long target );

#endif /* powerpc_HOST_ARCH || x86_64_HOST_ARCH */
#endif /* NEED_SYMBOL_EXTRAS */

#include "ll_end_private.h"
