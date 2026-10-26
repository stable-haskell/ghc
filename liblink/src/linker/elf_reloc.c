#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "elf_reloc.h"
#include "elf_plt.h"

#if defined(OBJFORMAT_ELF)

/* we currently only use this abstraction for elf/aarch64 and elf/riscv64 */
#if LL_TARGET_AARCH64 | LL_TARGET_RISCV64

bool
relocateObjectCode(ObjectCode * oc) {
    return ADD_SUFFIX(relocateObjectCode)(oc);
}


void flushInstructionCache(ObjectCode * oc){
   return ADD_SUFFIX(flushInstructionCache)(oc);
}
#endif

#endif
