#pragma once

#include "LinkerInternals.h"

#include "ll_begin_private.h"

/* Refresh is called only when image replacement changes its address, before
 * count/first/stride are published. Formats with image-relative metadata supply
 * their original initializer, not the format vtable's verify+init wrapper. */
typedef int (*ll_extra_refresh_fn)(ObjectCode *oc);
int ocAllocateExtras(ObjectCode *oc, int count, int first, int bssSize,
                     size_t extra_stride, ll_extra_refresh_fn refresh);
bool ocProtectExtras(ObjectCode* oc);

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
