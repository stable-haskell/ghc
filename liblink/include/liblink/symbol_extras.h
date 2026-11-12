/* Fixed target layouts, independent of the running host and its long width. */
#pragma once

#include <stddef.h>
#include <stdint.h>

typedef struct ll_symbol_extra_x86_64 {
    uint64_t addr;
    /* Eight bytes include the padding used by ELF TLSGD entries. */
    uint8_t jumpIsland[8];
} ll_symbol_extra_x86_64;

typedef struct ll_symbol_extra_arm {
    uint8_t jumpIsland[16];
} ll_symbol_extra_arm;

typedef struct ll_symbol_extra_riscv64 {
    uint64_t addr;
} ll_symbol_extra_riscv64;

typedef struct ll_symbol_extra_powerpc {
    struct {
        int16_t lis_r12, hi_addr;
        int16_t ori_r12_r12, lo_addr;
        int32_t mtctr_r12;
        int32_t bctr;
    } jumpIsland;
} ll_symbol_extra_powerpc;

_Static_assert(sizeof(ll_symbol_extra_x86_64) == 16 &&
               offsetof(ll_symbol_extra_x86_64, jumpIsland) == 8,
               "x86_64 symbol extra layout must match its instruction offsets");
_Static_assert(sizeof(ll_symbol_extra_arm) == 16,
               "ARM symbol extra must contain sixteen bytes");
_Static_assert(sizeof(ll_symbol_extra_riscv64) == 8,
               "RISC-V symbol extra must contain one 64-bit address");
_Static_assert(sizeof(ll_symbol_extra_powerpc) == 16 &&
               offsetof(ll_symbol_extra_powerpc, jumpIsland.mtctr_r12) == 8 &&
               offsetof(ll_symbol_extra_powerpc, jumpIsland.bctr) == 12,
               "PowerPC symbol extra must contain four 32-bit instructions");
