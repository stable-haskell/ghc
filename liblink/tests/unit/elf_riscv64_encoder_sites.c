/* Exercise real RISC-V encoders with distinct, inaccessible exec addresses. */
#define LL_INTERNAL_SOURCE 1
#include "ll_build.h"
#include "ll_target.h"
#include "linker/elf_compat.h"
#include <stdio.h>
#include <string.h>
#if !LL_BUILD_ELF || !LL_TARGET_RISCV64
#error "elf_riscv64_encoder_sites requires the RISC-V ELF target"
#endif
#include "linker/elf_reloc_riscv64.c"

static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "elf_riscv64_encoder_sites:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

typedef struct {
    uint32_t type;
    uint64_t input;
    int32_t value;
    uint64_t expected;
    size_t width;
    ll_status status;
} Case;

static void run_case(const Case *test, uintptr_t exec)
{
    uint8_t *paired = malloc(32);
    uint8_t *native = malloc(32);
    if (paired == NULL || native == NULL) abort();
    memset(paired, 0xa5, 32);
    for (size_t i = 0; i < 8; ++i) paired[8 + i] = (uint8_t)(test->input >> (i * 8));
    for (size_t i = 0; i < 32; ++i) native[i] = paired[i];
    ll_linker owner = {0};
    ll_elf_target_rel rel = {.r_offset = 8, .r_info = LL_ELF64_R_INFO(1, test->type)};
    ll_site site = {.work = paired + 8, .exec = exec};
    bool result = encodeAddendRISCV64Site(&owner, site, &rel, test->value);
    CHECK(result == (test->status == LL_OK ? EXIT_SUCCESS : EXIT_FAILURE), "wrong status convention");
    CHECK(owner.error_status == test->status, "wrong error status");
    for (size_t i = 0; i < test->width; ++i)
        CHECK(paired[8 + i] == (uint8_t)(test->expected >> (i * 8)), "encoded bytes differ");
    for (size_t i = 0; i < 32; ++i) {
        if (i < 8 || i >= 8 + test->width)
            CHECK(paired[i] == native[i], "adjacent bytes changed");
    }
    ll_error_clear(&owner);
    Section section = {.start = native, .size = 32, .exec_start = (uintptr_t)native};
    CHECK(encodeAddendRISCV64(&owner, &section, &rel, test->value) == result &&
          owner.error_status == test->status && memcmp(paired, native, 32) == 0,
          "native wrapper differs from site encoder");
    ll_error_clear(&owner);
    free(native);
    free(paired);
}

int main(void)
{
    const Case cases[] = {
        {LL_ELF_R_RISCV_64, 0, -1, UINT64_MAX, 8, LL_OK},
        {LL_ELF_R_RISCV_HI20, UINT32_C(0x00000297), 0x12345, UINT32_C(0x00012297), 4, LL_OK},
        {LL_ELF_R_RISCV_LO12_I, UINT32_C(0x00028313), 0x345, UINT32_C(0x34528313), 4, LL_OK},
        {LL_ELF_R_RISCV_BRANCH, UINT32_C(0x00000063), 4096, UINT32_C(0x00000063), 4, LL_E_RELOC_RANGE},
        {LL_ELF_R_RISCV_ADD8, UINT64_C(0xaaaaaa00), 7, UINT64_C(0xaaaaaa07), 1, LL_OK},
        {LL_ELF_R_RISCV_RVC_JUMP, UINT16_C(0xa001), 0, UINT16_C(0xa001), 2, LL_OK}
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        run_case(&cases[i], 1);
        run_case(&cases[i], UINTPTR_MAX);
    }
    if (failures) return 1;
    (void)puts("elf_riscv64_encoder_sites: PASS");
    return 0;
}
