#define _GNU_SOURCE 1
#define LL_INTERNAL_SOURCE 1
/* Define COMPAT_R_* before LinkerInternals.h reaches the system <elf.h>. */
#include "linker/elf_compat.h"
#include "LinkerInternals.h"

#if !defined(linux_HOST_OS) || !defined(x86_64_HOST_ARCH) \
    || !defined(OBJFORMAT_ELF)
#error "elf_relocation_errors requires the generated Linux x86_64 ELF target"
#else
#include "linker/Elf.c"

#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "elf_relocation_errors_helpers.h"

int main(void)
{
    size_t page_size = test_page_size(NULL);
    void *target = map_target(page_size);
    void *far_extras = NULL;
    void *near_extras;
    if (target == NULL) {
        (void)fprintf(stderr,
                      "fixture setup failed: MAP_32BIT target allocation unavailable\n");
        return 1;
    }
    far_extras = map_far_extras(page_size, target);
    if (far_extras == NULL) {
        (void)munmap(target, page_size);
        (void)fprintf(stderr,
                      "fixture setup failed: distant extras mapping unavailable\n");
        return 1;
    }
    near_extras = (unsigned char *)target + EXTRA_OFFSET;
    if (page_size < EXTRA_OFFSET + sizeof(SymbolExtra)) {
        (void)fprintf(stderr, "fixture setup failed: target mapping is too small\n");
        (void)munmap(far_extras, page_size);
        (void)munmap(target, page_size);
        return 1;
    }

    init_owners();
    uint64_t r64_value = (uint64_t)((uintptr_t)target + SLOT_OFFSET + 0x123)
                         + UINT64_C(0x100000005);
    uint64_t pc64_value = r64_value - (uintptr_t)((unsigned char *)target
                                                   + SLOT_OFFSET);
    run_valid_64(COMPAT_R_X86_64_64, INT64_C(0x100000005), target,
                 r64_value, "R_X86_64_64 wrote the expected 64-bit value");
    run_valid_64(COMPAT_R_X86_64_PC64, INT64_C(0x100000005), target,
                 pc64_value, "R_X86_64_PC64 wrote the expected 64-bit value");
    run_valid_32(COMPAT_R_X86_64_PC32, target,
                 "valid PC32 wrote the exact 4-byte payload");
    run_valid_32(COMPAT_R_X86_64_32, target,
                 "valid 32-bit absolute wrote the exact 4-byte payload");
    run_valid_32(COMPAT_R_X86_64_32S, target,
                 "valid 32S wrote the exact 4-byte payload");

    run_range_case(COMPAT_R_X86_64_PC32, UINT64_C(0x100000000), false,
                   "PC32 overflow reported relocation range", target,
                   far_extras, near_extras, page_size);
    run_range_case(COMPAT_R_X86_64_32, UINT64_C(0x100000000), false,
                   "32-bit absolute overflow reported relocation range", target,
                   far_extras, near_extras, page_size);
    run_range_case(COMPAT_R_X86_64_32S, UINT64_C(0x100000000), false,
                   "32S overflow reported relocation range", target,
                   far_extras, near_extras, page_size);
    run_range_case(COMPAT_R_X86_64_GOTPCREL, 0x123, true,
                   "GOTPCREL overflow reported relocation range", target,
                   far_extras, near_extras, page_size);
    run_range_case(COMPAT_R_X86_64_GOTPCRELX, 0x123, true,
                   "GOTPCRELX overflow reported relocation range", target,
                   far_extras, near_extras, page_size);
    run_range_case(COMPAT_R_X86_64_REX_GOTPCRELX, 0x123, true,
                   "REX_GOTPCRELX overflow reported relocation range", target,
                   far_extras, near_extras, page_size);
    run_range_case(COMPAT_R_X86_64_PLT32, UINT64_C(0x100000000), true,
                   "PLT32 exhausted fallback reported relocation range", target,
                   far_extras, near_extras, page_size);

    run_valid_extra(COMPAT_R_X86_64_GOTPCREL, false, target, near_extras);
    run_valid_extra(COMPAT_R_X86_64_PLT32, true, target, near_extras);
    run_unsupported(UINT32_C(0x7fffffff), false,
                    "unknown RelA reported unsupported", target);
    run_unsupported(COMPAT_R_X86_64_TLSGD, true,
                    "Linux TLSGD reported unsupported", target);

    check_default_unchanged();
    for (size_t i = 0; i < OWNER_COUNT; ++i) {
        ll_mutex_close_for(&owners[i].linker.services,
                           &owners[i].linker.linker_mutex);
    }
    (void)munmap(far_extras, page_size);
    (void)munmap(target, page_size);
    if (failures != 0) return 1;
    (void)puts("legacy ELF RelA relocation errors: PASS");
    return 0;
}
#endif
