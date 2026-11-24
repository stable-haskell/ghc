/* i386 ELF REL writes use host storage and target execution addresses. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#if LL_BUILD_ELF && LL_TARGET_I386
#include "elf_compat.h"
#include "LinkerInternals.h"
#include "linker/ElfTypes.h"
#include "linker/elf_reloc_i386.h"
#include <string.h>

static int
apply_i386(ObjectCode *oc, Section *section, uint32_t type, ll_site site,
           uintptr_t symbol, int64_t addend, ll_sym_type st,
           const ll_reloc_context *context)
{
    (void)section;
    (void)st;
    uint32_t S = (uint32_t)symbol, A = (uint32_t)addend;
    uint32_t P = (uint32_t)site.exec, value = S + A, output;
    uint32_t got = (uint32_t)oc->elf_info->got_exec;
    switch (type) {
    case COMPAT_R_386_NONE: return 1;
    case COMPAT_R_386_32: output = value; break;
    case COMPAT_R_386_PC32:
    case COMPAT_R_386_PLT32: output = value - P; break;
    case COMPAT_R_386_GOTOFF: output = value - got; break;
    case COMPAT_R_386_GOTPC: output = got + A - P; break;
    case COMPAT_R_386_GOT32:
    case COMPAT_R_386_GOT32X:
        LL_CHECK_FOR(oc->L, context != NULL && context->got_exec != 0);
        output = (uint32_t)context->got_exec - got + A;
        break;
    default:
        ll_error_set(oc->L, LL_E_UNSUPPORTED,
                     "%s: unhandled ELF relocation(Rel) type %" LL_FMT_WORD "\n",
                     oc->fileName, (ll_w)type);
        LL_LOG_ERROR_FOR(oc->L, "%s: unhandled ELF relocation(Rel) type %" LL_FMT_WORD "\n",
                         oc->fileName, (ll_w)type);
        return 0;
    }
    memcpy(site.work, &output, sizeof(output));
    return 1;
}

const struct ll_reloc_ops ll_elf_i386_reloc_ops = {
    .machine = LL_ELF_EM_386, .apply = apply_i386, .extra_bytes_per_symbol = 0
};
#endif
