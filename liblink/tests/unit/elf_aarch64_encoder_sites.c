/* Exercise the actual encoder without loading or executing object code. */
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "linker/elf_reloc_aarch64.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !LL_BUILD_ELF || !LL_TARGET_AARCH64
#error "elf_aarch64_encoder_sites requires the AArch64 ELF target"
#endif
#include "linker/elf_reloc_aarch64.c"
#include "linker/elf_plt.c"
#include "linker/elf_plt_aarch64.c"
#include "linker/elf_util.c"

static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "elf_aarch64_encoder_sites:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

typedef struct {
    uint32_t type, instruction;
    int64_t addend;
    size_t width;
    uint64_t expected;
    ll_status status;
    const char *reason;
} Case;

static void fatal(void *ctx, const char *message, va_list args)
{
    (void)ctx;
    (void)vfprintf(stderr, message, args);
    abort();
}

/* malloc supplies alignment for the unchanged eight-byte classifier reads.
 * Character stores initialize allocation storage without imposing a declared
 * instruction type; each invocation gets fresh storage. */
static uint8_t *storage(uint32_t instruction)
{
    uint8_t *bytes = malloc(32);
    if (bytes == NULL) abort();
    memset(bytes, 0xa5, 32);
    for (size_t i = 0; i < 8; ++i)
        bytes[8 + i] = i < 4 ? (uint8_t)(instruction >> (8 * i)) : 0;
    return bytes;
}

static void run_case(const Case *test, uintptr_t exec)
{
    uint8_t *paired = storage(test->instruction);
    uint8_t *native = storage(test->instruction);
    uint8_t before[32];
    memcpy(before, paired, sizeof(before));
    ll_linker owner = {0};
    owner.services.host.fatal = fatal;
    ll_elf_target_rel rel = {.r_offset = 8,
        .r_info = LL_ELF64_R_INFO(7, test->type)};
    const ll_site site = {.work = paired + 8, .exec = exec};
    const bool result = encodeAddendAarch64Site(&owner, site, &rel, test->addend);
    CHECK(result == (test->status == LL_OK ? EXIT_SUCCESS : EXIT_FAILURE),
          "paired result changed zero-success convention");
    CHECK(owner.error_status == test->status, "paired status differs");
    char diagnostic[LL_ERROR_CAPACITY];
    memcpy(diagnostic, owner.error_message, sizeof(diagnostic));
    if (test->status != LL_OK) {
        char expected[LL_ERROR_CAPACITY];
        const int n = snprintf(expected, sizeof(expected),
            "AArch64 relocation type %" PRIu32 ": %s (addend %" PRId64 ")",
            test->type, test->reason, test->addend);
        CHECK(n >= 0 && (size_t)n < sizeof(expected), "expected diagnostic truncated");
        CHECK(strcmp(diagnostic, expected) == 0, "exact failure diagnostic differs");
        CHECK(memcmp(paired, before, sizeof(before)) == 0, "failed encoding wrote storage");
    } else {
        CHECK(diagnostic[0] == '\0', "successful encoding reported an error");
        uint8_t expected[8];
        for (size_t i = 0; i < sizeof(expected); ++i)
            expected[i] = (uint8_t)(test->expected >> (8 * i));
        CHECK(memcmp(paired + 8, expected, test->width) == 0, "encoded bytes differ");
        CHECK(memcmp(paired, before, 8) == 0 &&
              memcmp(paired + 8 + test->width, before + 8 + test->width,
                     sizeof(before) - 8 - test->width) == 0, "adjacent bytes changed");
    }
    ll_error_clear(&owner);
    Section section = {.start = native, .exec_start = (uintptr_t)native, .size = 32};
    CHECK(encodeAddendAarch64(&owner, &section, &rel, test->addend) == result &&
          owner.error_status == test->status &&
          strcmp(owner.error_message, diagnostic) == 0 &&
          memcmp(paired, native, 32) == 0, "native wrapper no longer matches paired encoder");
    free(paired);
    free(native);
}

#define OK(type, instruction, addend, width, bytes) \
    {COMPAT_R_AARCH64_##type, instruction, addend, width, bytes, LL_OK, NULL}
#define BAD(type, instruction, addend, status, reason) \
    {COMPAT_R_AARCH64_##type, instruction, addend, 0, 0, status, reason}
int main(void)
{
    const Case cases[] = {
        OK(ABS64, 0, -1, 8, UINT64_MAX),
        OK(PREL64, 0, INT64_MIN, 8, UINT64_C(0x8000000000000000)),
        OK(ABS32, 0, INT32_MIN, 4, UINT32_C(0x80000000)),
        OK(ABS32, 0, INT32_MAX, 4, UINT32_C(0x7fffffff)),
        OK(PREL32, 0, -4, 4, UINT32_C(0xfffffffc)),
        OK(ABS16, 0, INT16_MIN, 2, UINT16_C(0x8000)),
        OK(PREL16, 0, INT16_MAX, 2, UINT16_C(0x7fff)),
        OK(ADR_PREL_PG_HI21, 0x90000003, -4096, 4, 0xf0ffffe3),
        OK(ADR_PREL_PG_HI21, 0x90000003, 4096, 4, 0xb0000003),
        OK(ADR_GOT_PAGE, 0x90000003, -4096, 4, 0xf0ffffe3),
        OK(JUMP26, 0x14000000, 4, 4, 0x14000001),
        OK(CALL26, 0x94000000, -4, 4, 0x97ffffff),
        OK(ADD_ABS_LO12_NC, 0x91000022, 0xabc, 4, 0x912af022),
        OK(LDST8_ABS_LO12_NC, 0x39000022, 0xabc, 4, 0x392af022),
        OK(LDST16_ABS_LO12_NC, 0x79000022, 0x246, 4, 0x79048c22),
        OK(LDST32_ABS_LO12_NC, 0xb9000022, 0x48c, 4, 0xb9048c22),
        OK(LDST64_ABS_LO12_NC, 0xf9000022, 0x918, 4, 0xf9048c22),
        OK(LDST128_ABS_LO12_NC, 0x3dc00022, 0x120, 4, 0x3dc04822),
        OK(LD64_GOT_LO12_NC, 0xf9000022, 0x918, 4, 0xf9048c22),
        BAD(ABS32, 0, INT64_C(2147483648), LL_E_RELOC_RANGE, "value is out of range"),
        BAD(PREL32, 0, -INT64_C(2147483649), LL_E_RELOC_RANGE, "value is out of range"),
        BAD(ABS16, 0, 32768, LL_E_RELOC_RANGE, "value is out of range"),
        BAD(PREL16, 0, -32769, LL_E_RELOC_RANGE, "value is out of range"),
        BAD(ADR_PREL_PG_HI21, 0x90000003, INT64_C(4294967296), LL_E_RELOC_RANGE,
            "value is out of range"),
        BAD(ADR_GOT_PAGE, 0x90000003, -INT64_C(4294971392), LL_E_RELOC_RANGE,
            "value is out of range"),
        BAD(JUMP26, 0x14000000, INT64_C(134217728), LL_E_RELOC_RANGE,
            "value is out of range"),
        BAD(CALL26, 0x94000000, -INT64_C(134217732), LL_E_RELOC_RANGE,
            "value is out of range"),
        BAD(LD64_GOT_LO12_NC, 0xf9000022, 1, LL_E_RELOC_RANGE,
            "GOT value is not eight-byte aligned"),
        BAD(LDST64_ABS_LO12_NC, 0x39000022, 8, LL_E_FORMAT,
            "relocation scale does not match the instruction"),
        {UINT32_MAX, 0, -7, 0, 0, LL_E_UNSUPPORTED, "unsupported relocation type"}
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        run_case(&cases[i], 1);
        run_case(&cases[i], UINTPTR_MAX);
    }
    if (failures != 0) return 1;
    (void)puts("elf_aarch64_encoder_sites: PASS (30 cases, two inaccessible exec values, native equivalence)");
    return 0;
}
