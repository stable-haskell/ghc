/* Short Thumb relocations must not read past their two-byte work sites. */
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "LinkerInternals.h"
#include "linker/elf_reloc_arm.h"
#include "ll_path.h"
#include <stdlib.h>
#include <string.h>

int main(void)
{
    static const struct { uint32_t type; uint16_t before, after; } cases[] = {
        {COMPAT_R_ARM_THM_JUMP8, 0xd000, 0xd004},
        {COMPAT_R_ARM_THM_JUMP11, 0xe000, 0xe004}
    };
    ll_linker owner = {0};
    ObjectCode object = {.L = &owner, .fileName = WSTR("thumb-short.o")};
    ll_reloc_context context = {.format_symbol_type = LL_ELF_STT_FUNC};
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        uint8_t *bytes = malloc(3);
        if (!bytes) return 1;
        bytes[0] = 0xa5;
        memcpy(bytes + 1, &cases[i].before, 2);
        ll_site site = {.work = bytes + 1, .exec = 0x1000};
        int ok = ll_elf_arm_reloc_ops.apply(&object, NULL, cases[i].type, site,
                                            0x1005, 0, LL_SYM_CODE, &context);
        uint16_t result;
        memcpy(&result, bytes + 1, 2);
        if (!ok || result != cases[i].after || bytes[0] != 0xa5) {
            free(bytes);
            return 1;
        }
        free(bytes);
    }
    return 0;
}
