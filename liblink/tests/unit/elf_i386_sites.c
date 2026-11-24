/* Real i386 apply table with unaligned work and separate execution addresses. */
#define LL_INTERNAL_SOURCE 1
#include "ll_target.h"
#include "linker/elf_compat.h"
#include "linker/elf_reloc_i386.h"
#include "ll_path.h"
#include <stdio.h>
#include <string.h>
#if !LL_BUILD_ELF || !LL_TARGET_I386
#error "elf_i386_sites requires the i386 ELF target"
#endif
#include "linker/elf_reloc_i386.c"

int main(void)
{
    static const struct { uint32_t type, result; } cases[] = {
        {COMPAT_R_386_32, 0x10002004},
        {COMPAT_R_386_PC32, 0x1004},
        {COMPAT_R_386_PLT32, 0x1004},
        {COMPAT_R_386_GOTOFF, 0xffffe004},
        {COMPAT_R_386_GOTPC, 0x3004},
        {COMPAT_R_386_GOT32, 0x10},
        {COMPAT_R_386_GOT32X, 0x10}
    };
    ll_linker owner = {0};
    struct ll_elf_object_info info = {.got_exec = 0x10004000};
    ObjectCode oc = {.L = &owner, .elf_info = &info, .fileName = WSTR("i386-site.o")};
    const ll_reloc_context ctx = {.got_exec = 0x1000400c};
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        uint8_t bytes[8];
        memset(bytes, 0xa5, sizeof(bytes));
        ll_site site = {.work = bytes + 1, .exec = 0x10001000};
        uint32_t result;
        if (!ll_elf_i386_reloc_ops.apply(&oc, NULL, cases[i].type, site,
                                         0x10002000, 4, LL_SYM_DATA, &ctx)) return 1;
        memcpy(&result, bytes + 1, sizeof(result));
        if (result != cases[i].result || bytes[0] != 0xa5 || bytes[5] != 0xa5 ||
            bytes[6] != 0xa5 || bytes[7] != 0xa5) {
            (void)fprintf(stderr, "i386 site mismatch for type %u\n", cases[i].type);
            return 1;
        }
    }
    uint8_t unchanged = 0xa5;
    ll_site none = {.work = &unchanged, .exec = 0};
    if (!ll_elf_i386_reloc_ops.apply(&oc, NULL, COMPAT_R_386_NONE, none,
                                     0, 0, LL_SYM_DATA, NULL) || unchanged != 0xa5)
        return 1;
    return 0;
}
