/* Exercise the ARM veneer adapter with host-width stub API outputs. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "linker/elf_compat.h"
#include "linker/elf_plt.h"

#include <stdio.h>
#include <stdint.h>

#if !LL_BUILD_ELF || !LL_TARGET_ARM
#error "elf_arm_veneer requires the ARM ELF target"
#endif

static unsigned calls;
static bool miss, fail;
static uintptr_t expected_target, output_address;
static uint8_t expected_flags;

bool elf_findStub(Section *section, void **addr, uint8_t flags)
{
    (void)section;
    if ((uintptr_t)*addr != expected_target || flags != expected_flags) calls = 100;
    else ++calls;
    if (miss) return true;
    *addr = (void *)output_address;
    return false;
}

bool elf_makeStub(ll_linker *L, Section *section, void **addr,
                  void *got_addr, uint8_t flags)
{
    (void)L;
    (void)section;
    if ((uintptr_t)*addr != expected_target || flags != expected_flags || got_addr)
        calls = 100;
    else ++calls;
    if (fail) return true;
    *addr = (void *)output_address;
    return false;
}

int main(void)
{
    struct { uint32_t before, target, after; } guarded;
    for (unsigned flags = 0; flags < 2; ++flags) {
        for (unsigned mode = 0; mode < 3; ++mode) {
            guarded.before = UINT32_C(0x11223344);
            guarded.target = UINT32_C(0x12345001);
            guarded.after = UINT32_C(0x55667788);
            expected_target = guarded.target;
            expected_flags = (uint8_t)flags;
            output_address = (uintptr_t)UINT32_C(0xabcdef00);
#if UINTPTR_MAX > UINT32_MAX
            output_address |= (uintptr_t)UINT64_C(0x123400000000);
#endif
            calls = 0;
            miss = mode != 0;
            fail = mode == 2;
            bool result = elf_arm_veneer(NULL, NULL, &guarded.target, expected_flags);
            if (result != fail || calls != (miss ? 2u : 1u) ||
                guarded.before != UINT32_C(0x11223344) ||
                guarded.after != UINT32_C(0x55667788) ||
                guarded.target != (fail ? UINT32_C(0x12345001) : UINT32_C(0xabcdef00))) {
                (void)fprintf(stderr, "ARM veneer adapter failed: flags=%u mode=%u\n", flags, mode);
                return 1;
            }
        }
    }
    return 0;
}
