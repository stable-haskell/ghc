/* ARM ELF REL application uses target addresses and host work storage. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#if LL_BUILD_ELF && LL_TARGET_ARM
#include "elf_compat.h"
#include "LinkerInternals.h"
#include "linker/ElfTypes.h"
#include "linker/elf_plt.h"
#include "linker/elf_reloc_arm.h"
#include "linker/util.h"
#include <string.h>

static bool
arm_veneer(ll_linker *L, Section *section, uint32_t *target, uint8_t flags)
{
    void *destination = (void *)(uintptr_t)*target;
    ll_site stub;
    if (elf_findStubSite(section, destination, flags, &stub) &&
        elf_makeStubSite(L, section, destination, NULL, flags, &stub)) return true;
    *target = (uint32_t)stub.exec;
    return false;
}

static int
apply_arm(ObjectCode *oc, Section *section, uint32_t reloc_type, ll_site site,
          uintptr_t symbol, int64_t addend, ll_sym_type st,
          const ll_reloc_context *context)
{
    (void)addend;
    (void)st;
    uint32_t P = (uint32_t)site.exec, S = (uint32_t)symbol, offset;
    int is_target_thm = 0, T = 0;
    if (context->format_symbol_type == LL_ELF_STT_FUNC) {
        is_target_thm = S & 1;
        T = is_target_thm;
        S &= ~UINT32_C(1);
    }
    if (reloc_type == 0) {
        return true;
    }
    size_t width = (reloc_type == COMPAT_R_ARM_THM_JUMP8 ||
                    reloc_type == COMPAT_R_ARM_THM_JUMP11) ? 2 : 4;
    union { uint32_t word; uint16_t halves[2]; } storage = {0};
    uint8_t *work = (uint8_t *)&storage;
    memcpy(work, site.work, width);
    uint32_t *pP = &storage.word;
    switch (reloc_type) {
       case COMPAT_R_ARM_ABS32:     /* (S + A) | T */
           // Specified by Linux ARM ABI to be equivalent to ABS32
       case COMPAT_R_ARM_TARGET1:
           *(ll_elf32_word *)work += S;
           *(ll_elf32_word *)work |= T;
           break;

       case COMPAT_R_ARM_REL32:     /* ((S + A) | T) – P */
           *(ll_elf32_word *)work += S;
           *(ll_elf32_word *)work |= T;
           *(ll_elf32_word *)work -= P;
           break;

       case COMPAT_R_ARM_BASE_PREL: /* B(S) + A – P */
       {
           int32_t A = *pP;
           // bfd used to encode sb (B(S)) as 0.
           *(uint32_t *)work += 0 + A - P;
           break;
       }

       case COMPAT_R_ARM_GOT_BREL: /* GOT(S) + A – GOT_ORG */
       {
           int32_t A = *pP;
           uintptr_t GOT_S = context->got_exec;
           *(uint32_t *)work = (uint32_t) GOT_S + A - (uint32_t)oc->elf_info->got_exec;
           break;
       }

       case COMPAT_R_ARM_CALL:
       case COMPAT_R_ARM_JUMP24:
       {
           // N.B. LLVM's LLD linker's relocation implementation is a fantastic
           // resource
           ll_word32 *word = (ll_word32 *)work;
           ll_int32 imm = (*word & ((1<<24)-1)) << 2;

           const ll_stg_bool is_blx = (*word & 0xf0000000) == 0xf0000000;
           const ll_word32 hBit = is_blx ? ((*word >> 24) & 1) : 0;
           imm |= hBit << 1;

           // Sign extend to 32 bits
           // I would have thought this would be 24 bits but LLD uses 26 here.
           // Hmm.
           int32_t A = signExtend32(26, imm);

           S = S + A; A = 0;

           ll_word32 result = ((S + A) | T) - P;

           const ll_stg_bool overflow = !isInt(26, (ll_int32) result);
           // Handle overflow and Thumb interworking
           const ll_stg_bool needs_veneer =
               (is_target_thm && reloc_type == COMPAT_R_ARM_JUMP24)
               || overflow;

           if(needs_veneer) { /* overflow or thum interworking */
               // Note [PC bias]
               // ~~~~~~~~~~~~~~
               // From the ELF for the ARM Architecture documentation:
               // > 4.6.1.1 Addends and PC-bias compensation
               // > A binary file may use REL or RELA relocations or a mixture
               // > of the two (but multiple relocations for the same address
               // > must use only one type).
               // > If the relocation is pc-relative then compensation for the
               // > PC bias (the PC value is 8 bytes ahead of the executing
               // > instruction in ARM state and 4 bytes in Thumb state) must
               // > be encoded in the relocation by the object producer.
               int32_t bias = 8;

               S += bias;
               /* try to locate an existing stub for this target */
               if (arm_veneer(oc->L, section, &S, 0)) {
                   LL_LOG_ERROR_FOR(oc->L, "Unable to create veneer for ARM_CALL\n");
                   return 0;
               }
               S -= bias;

               result = ((S + A) | T) - P;
               result &= ~1; // Clear thumb indicator bit

               LL_CHECK_FOR(oc->L, isInt(26, result)); /* X in range */
           }

           // Update the branch target
           const ll_word32 imm24 = (result & 0x03fffffc) >> 2;
           *word = (*word & ~0x00ffffff)
                 | (imm24 & 0x00ffffff);

           // Change the relocated branch into a BLX if necessary
           const ll_stg_bool switch_mode =
               is_target_thm && (reloc_type == COMPAT_R_ARM_CALL);
           if (!needs_veneer && switch_mode) {
               const ll_word32 hBit = (result & 0x2) >> 1;
               // Change instruction to BLX
               *word = (*word & ~0xFF000000) | ((0xfa | hBit) << 24);
               LL_IF_DEBUG_FOR(oc->L, linker_verbose,
                   LL_DEBUG_FOR(oc->L, "Changed BL to BLX at %p\n", site.work));
           }
           break;
       }

       case COMPAT_R_ARM_MOVT_ABS:
       case COMPAT_R_ARM_MOVW_ABS_NC:
       {
           ll_word32 *word = (ll_word32 *)work;
           ll_word32 imm12 = *word & 0xfff;
           ll_word32 imm4 = (*word >> 16) & 0xf;
           ll_int32 offset = imm4 << 12 | imm12;
           ll_word32 result = (S + offset) | T;

           if (reloc_type == COMPAT_R_ARM_MOVT_ABS)
               result = (result & 0xffff0000) >> 16;

           ll_word32 result12 = result & 0xfff;
           ll_word32 result4 = (result >> 12) & 0xf;
           *word = (*word & ~0xf0fff) | (result4 << 16) | result12;
           break;
       }

       case COMPAT_R_ARM_THM_CALL:
       case COMPAT_R_ARM_THM_JUMP24:
       {
           ll_word16 *upper = (ll_word16 *)work;
           ll_word16 *lower = (ll_word16 *)(work + 2);

           int overflow;
           int to_thm = (*lower >> 12) & 1;
           int sign = (*upper >> 10) & 1;
           int j1, j2, i1, i2;

           // Decode immediate value
           j1 = (*lower >> 13) & 1; i1 = ~(j1 ^ sign) & 1;
           j2 = (*lower >> 11) & 1; i2 = ~(j2 ^ sign) & 1;

           ll_int32 A = (sign << 24)
                        | (i1 << 23)
                        | (i2 << 22)
                        | ((*upper & 0x03ff) << 12)
                        | ((*lower & 0x07ff) << 1);

            // Sign extend 25 to 32 bits
           if (A & 0x01000000)
               A -= 0x02000000;

           S = S + A; A = 0;

           offset = ((S + A) | T) - P;
           overflow = elf_arm_thumb_branch_overflow(offset);
           sign = offset >> 31;

           if ((!is_target_thm && reloc_type == COMPAT_R_ARM_THM_JUMP24)
               || overflow) {
               // Generate veneer

               // see [PC bias] above.
               int32_t bias = 4;
               S += bias;
               // set the Thumb indicator to S, the final address should
               // carry the correct thumb indicator.
               S |= T;
               /* try to locate an existing stub for this target */
               if (arm_veneer(oc->L, section, &S, 1)) {
                   LL_LOG_ERROR_FOR(oc->L, "Unable to create veneer for ARM_THM_CALL\n");
                   return 0;
               }
               S -= bias;

               offset = ((S + A) | T) - P;

               sign = offset >> 31;
               to_thm = 1;
           } else if (!is_target_thm
                      && reloc_type == COMPAT_R_ARM_THM_CALL) {
               offset &= ~0x3;
               to_thm = 0;
           }

           // Reencode instruction
           i1 = ~(offset >> 23) & 1; j1 = sign ^ i1;
           i2 = ~(offset >> 22) & 1; j2 = sign ^ i2;
           *upper = ( (*upper & 0xf800)
                  | (sign << 10)
                  | ((offset >> 12) & 0x03ff) );
           *lower = ( (*lower & 0xd000)
                  | (j1 << 13)
                  | (to_thm << 12)
                  | (j2 << 11)
                  | ((offset >> 1) & 0x07ff) );
           break;
       }

       case COMPAT_R_ARM_THM_MOVT_ABS:
       case COMPAT_R_ARM_THM_MOVW_ABS_NC:
       {
           ll_word16 *upper = (ll_word16 *)work;
           ll_word16 *lower = (ll_word16 *)(work + 2);
           ll_int32 offset = ((*upper & 0x000f) << 12)
                           | ((*upper & 0x0400) << 1)
                           | ((*lower & 0x7000) >> 4)
                           | (*lower & 0x00ff);

           offset = (offset ^ 0x8000) - 0x8000; // Sign extend
           offset += S;
           if (reloc_type == COMPAT_R_ARM_THM_MOVW_ABS_NC)
               offset |= T;
           else if (reloc_type == COMPAT_R_ARM_THM_MOVT_ABS)
               offset >>= 16;

           *upper = ( (*upper & 0xfbf0)
                  | ((offset & 0xf000) >> 12)
                  | ((offset & 0x0800) >> 1) );
           *lower = ( (*lower & 0x8f00)
                  | ((offset & 0x0700) << 4)
                  | (offset & 0x00ff) );
           break;
       }

       case COMPAT_R_ARM_THM_JUMP8:
       {
           ll_word16 *word = (ll_word16 *)work;
           ll_word offset = *word & 0x01fe;
           offset += S - P;
           if (!is_target_thm) {
               LL_LOG_ERROR_FOR(oc->L, "%s: Thumb to ARM transition with JUMP8 relocation "
                          "not supported\n",
                          oc->fileName);
               return 0;
           }

           *word = (*word & ~0x01fe)
                 | (offset & 0x01fe);
           break;
       }

       case COMPAT_R_ARM_THM_JUMP11:
       {
           ll_word16 *word = (ll_word16 *)work;
           ll_word offset = *word & 0x0ffe;
           offset += S - P;
           if (!is_target_thm) {
               LL_LOG_ERROR_FOR(oc->L, "%s: Thumb to ARM transition with JUMP11 relocation "
                          "not supported\n",
                          oc->fileName);
               return 0;
           }

           *word = (*word & ~0x0ffe)
                 | (offset & 0x0ffe);
           break;
       }
       case COMPAT_R_ARM_GOT_PREL: {
              int32_t A = *pP;
              uintptr_t GOT_S = context->got_exec;
              LL_CHECK_FOR(oc->L, GOT_S);
              *(uint32_t *)work = (uint32_t) GOT_S + A - P;
              break;
       }
       default:
           ll_error_set(oc->L, LL_E_UNSUPPORTED,
                        "%s: unhandled ELF relocation(Rel) type %" LL_FMT_WORD "\n",
                        oc->fileName, (ll_w)reloc_type);
           LL_LOG_ERROR_FOR(oc->L, "%s: unhandled ELF relocation(Rel) type %" LL_FMT_WORD "\n",
                            oc->fileName, (ll_w)reloc_type);
           return 0;
    }
    memcpy(site.work, work, width);
    return 1;
}

const struct ll_reloc_ops ll_elf_arm_reloc_ops = {
    .machine = LL_ELF_EM_ARM, .apply = apply_arm, .extra_bytes_per_symbol = 0
};
#endif
