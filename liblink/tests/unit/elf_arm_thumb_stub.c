/* Check the real Thumb veneer emitter against assembler bytes and, on ARM,
 * execute an emitted veneer to a Thumb function. */
#define _GNU_SOURCE 1
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "linker/elf_compat.h"
#include "linker/elf_plt.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#if defined(__arm__) && defined(__linux__)
#include <sys/mman.h>
#endif

#if !LL_BUILD_ELF || !LL_TARGET_ARM
#error "elf_arm_thumb_stub requires the ARM ELF target"
#endif
#include "linker/elf_plt_arm.c"

#if defined(__arm__) && defined(__linux__)
static int destination(void) { return 73; }
#endif

int main(void)
{
    static const uint8_t expected[12] = {
        0x4a, 0xf6, 0xcd, 0x3c, 0xc1, 0xf2, 0x34, 0x2c,
        0x00, 0xbf, 0x60, 0x47
    };
    uint16_t storage[8];
    memset(storage, 0xa5, sizeof(storage));
    ElfStub stub = {.addr = &storage[1],
                    .target = (void *)(uintptr_t)UINT32_C(0x1234abcd), .flags = 1};
    if (elf_makeStubArm(&stub) || memcmp(&storage[1], expected, sizeof(expected)) ||
        storage[0] != UINT16_C(0xa5a5) || storage[7] != UINT16_C(0xa5a5)) {
        (void)fputs("Thumb veneer bytes differ from assembler\n", stderr);
        return 1;
    }
#if defined(__arm__) && defined(__linux__)
    void *code = mmap(NULL, 4096, PROT_READ | PROT_WRITE | PROT_EXEC,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (code == MAP_FAILED) return 2;
    stub.addr = code;
    stub.target = (void *)(uintptr_t)destination;
    if (elf_makeStubArm(&stub)) return 2;
    __builtin___clear_cache(code, (char *)code + 12);
    int (*call)(void) = (int (*)(void))((uintptr_t)code | 1u);
    int result = call();
    if (munmap(code, 4096) != 0 || result != 73) return 1;
#endif
    return 0;
}
