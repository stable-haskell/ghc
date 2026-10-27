#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "elf_compat.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#if LL_TARGET_ARM

#include "Elf.h"
#include "elf_plt.h"

#if LL_BUILD_ELF

/* three 4 byte instructions */
const size_t elf_stubSizeArm = 12;

/*
 * Compute the number of stub (PLT entries) for a given section by iterating
 * over the relocations and relocations with explicit addend and counting those
 * relocations that might require a PLT relocation.
 *
 * This will be an upper bound, and we might not use all stubs.  However by
 * calculating the number of potential stubs beforehand, we can allocate enough
 * space adjacent to the section, such that the PLT is rather close to the
 * section, and the risk of the stubs being out of reach for the instruction to
 * be relocated is minimal.
 */
bool elf_needStubForRelArm(ll_elf_target_rel * rel) {
    switch(LL_ELF32_R_TYPE(rel->r_info)) {
        case COMPAT_R_ARM_PC24:
        case COMPAT_R_ARM_CALL:
        case COMPAT_R_ARM_JUMP24:
        case COMPAT_R_ARM_THM_CALL:
        case COMPAT_R_ARM_THM_JUMP24:
        case COMPAT_R_ARM_THM_JUMP19:
            return true;
        default:
            return false;
    }
}
bool elf_needStubForRelaArm(ll_elf_target_rela * rela) {
    switch(LL_ELF32_R_TYPE(rela->r_info)) {
        case COMPAT_R_ARM_PC24:
        case COMPAT_R_ARM_CALL:
        case COMPAT_R_ARM_JUMP24:
        case COMPAT_R_ARM_THM_CALL:
        case COMPAT_R_ARM_THM_JUMP24:
        case COMPAT_R_ARM_THM_JUMP19:
            return true;
        default:
            return false;
    }
}

bool elf_makeStubArmArm(ElfStub * s);
bool elf_makeStubArmThm(ElfStub * s);
/*
  Note [The ARM/Thumb Story]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~
  Support for the ARM architecture is complicated by the fact that ARM has not
  one but several instruction encodings. The two relevant ones here are the
  original ARM encoding and Thumb, a more dense variant of ARM supporting only
  a subset of the instruction set.

  How the CPU decodes a particular instruction is determined by a mode bit. This
  mode bit is set on jump instructions, the value being determined by the low
  bit of the target address: An odd address means the target is a procedure
  encoded in the Thumb encoding whereas an even address means it's a traditional
  ARM procedure (the actual address jumped to is even regardless of the encoding
  bit).

  Interoperation between Thumb- and ARM-encoded object code (known as
  "interworking") is tricky. If the linker needs to link a call by an ARM object
  into Thumb code (or vice-versa) it will produce a jump island using stubs.
  This, however, is incompatible with GHC's tables-next-to-code since pointers
  fixed-up in this way will point to a bit of generated code, not a info
  table/Haskell closure like TNTC expects. For this reason, it is critical that
  GHC emit exclusively ARM or Thumb objects for all Haskell code.

  We still do, however, need to worry about calls to foreign code, hence the
  need for makeArmSymbolExtra.
*/

bool
elf_makeStubArmArm(ElfStub * s) {

    // We (the linker) may corrupt r12 (ip) according to the "ELF for the ARM
    // Architecture" reference.

    // movw<c> <Rd>, #<imm16> (Encoding A2) looks like:
    // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
    // [   cond  ]  0  0  1  1  0  0  0  0 [   imm4  ]
    //
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    // [    Rd   ] [              imm12              ]
    //
    // movt<c> <Rd>, #<imm16> (Encoding A1) looks like:
    // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
    // [   cond  ]  0  0  1  1  0  1  0  0 [   imm4  ]
    //
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    // [    Rd   ] [              imm12              ]
    //
    // bx<c> <Rd> (Encoding A1) looks like:
    // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
    // [   cond  ]  0  0  0  1  0  0  1  0  1  1  1  1
    //
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    //  1  1  1  1  1  1  1  1  0  0  0  1  [    Rd   ]
    //
    // The difference for the movw and movt is only bit 22.
    // We'll use 0b1110 for the condition.

    uint32_t movw_r12 = 0xe300c000;
    uint32_t movt_r12 = 0xe340c000;
    uint32_t bx_r12   = 0xe12fff1c;

    *((uint32_t*)s->addr+0) = movw_r12
                              | (((uint32_t )s->target & 0xf000) << 4)
                              |  ((uint32_t )s->target & 0x0fff);
    *((uint32_t*)s->addr+1) = movt_r12
                              | ((((uint32_t )s->target >> 16) & 0xf000) << 4)
                              |  (((uint32_t )s->target >> 16) & 0x0fff);
    *((uint32_t*)s->addr+2) = bx_r12;

    return EXIT_SUCCESS;
}

bool
elf_makeStubArmThm(ElfStub * s) {

    // movw<c> <Rd>, #<imm16> (Encoding T3) looks like:
    // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
    //  1  1  1  1  0  i  1  0  0  1  0  0 [  imm4   ]
    //
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    //  0 [ imm3 ] [   Rd    ] [        imm8         ]
    //
    // imm32 = zero_extend(imm4:i:imm3:imm8,32)
    //
    // movt<c> <Rd>, #<imm16> (Encoding T1) looks like:
    // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
    //  1  1  1  1  0  i  1  0  1  1  0  0 [  imm4   ]
    //
    // imm16 = imm4:i:imm3:imm8
    //
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    //  0 [ imm3 ] [    Rd   ] [        imm8         ]
    //
    // bx<c> <Rd> (Encoding T1) looks like:
    // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    //  0  1  0  0  0  1  1  1  0 [    Rd   ]  0  0  0

    uint32_t movw_r12 = 0xf2400c00;
    uint32_t movt_r12 = 0xf2c00c00;
    uint32_t movw = movw_r12
                              | (((uint32_t )s->target & 0xf000) << 4)
                              | (((uint32_t )s->target & 0x0800) << 15)
                              | (((uint32_t )s->target & 0x0700) << 4)
                              |  ((uint32_t )s->target & 0x00ff);
    uint32_t movt = movt_r12
                              | ((((uint32_t )s->target >> 16) & 0xf000) << 4)
                              | ((((uint32_t )s->target >> 16) & 0x0800) << 15)
                              | ((((uint32_t )s->target >> 16) & 0x0700) << 4)
                              |  (((uint32_t )s->target >> 16) & 0x00ff);
    /* Thumb-2 instructions are two successive halfwords, high half first. */
    uint16_t *insn = (uint16_t *)s->addr;
    insn[0] = (uint16_t)(movw >> 16);
    insn[1] = (uint16_t)movw;
    insn[2] = (uint16_t)(movt >> 16);
    insn[3] = (uint16_t)movt;
    insn[4] = UINT16_C(0xbf00); /* nop padding */
    insn[5] = UINT16_C(0x4760); /* bx ip */

    return EXIT_SUCCESS;
}

bool
elf_makeStubArm(ElfStub * s) {
    if((s->flags & 1) == 0)
        return elf_makeStubArmArm(s);
    else
        return elf_makeStubArmThm(s);
}

#endif // OBJECTFORMAT_ELF

#endif // arm_HOST_ARCH
