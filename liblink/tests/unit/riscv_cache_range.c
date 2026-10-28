/* Check the real RISC-V cache-flush address ranges without invoking them. */
#define LL_INTERNAL_SOURCE 1
#include "ll_linker.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static uintptr_t starts[2], ends[2];
static unsigned calls;
static void record_clear(void *begin, void *end)
{
    if (calls < 2) {
        starts[calls] = (uintptr_t)begin;
        ends[calls] = (uintptr_t)end;
    }
    ++calls;
}
#define __builtin___clear_cache record_clear
#include "linker/elf_reloc_riscv64.c"
#undef __builtin___clear_cache

int main(void)
{
    _Alignas(uint64_t) unsigned char image[128] = {0};
    _Alignas(SymbolExtra) unsigned char extras[16] = {0};
    ObjectCode object = {0};
    object.image = (char *)image;
    object.misalignment = 8;
    object.fileSize = 7;
    object.symbol_extras = (SymbolExtra *)(void *)extras;
    object.symbol_extra_stride = 8;
    object.n_symbol_extras = 2;
    flushInstructionCacheRISCV64(&object);
    if (calls != 2 || starts[0] != (uintptr_t)(image + 8) ||
        ends[0] != (uintptr_t)(image + 15) ||
        starts[1] != (uintptr_t)extras || ends[1] != (uintptr_t)(extras + 16)) {
        (void)fprintf(stderr, "riscv_cache_range: incorrect code or extra bounds\n");
        return 1;
    }
    calls = 0;
    object.fileSize = 0;
    flushInstructionCacheRISCV64(&object);
    if (calls != 2 || starts[0] != ends[0] || ends[1] - starts[1] != 16) {
        (void)fprintf(stderr, "riscv_cache_range: incorrect zero-length bounds\n");
        return 1;
    }
    (void)puts("riscv_cache_range: PASS");
    return 0;
}
