/* This unit is a template: the native test driver inserts the current
 * checkInt, read32le, write32le, and setUType definitions at the marker. */
#define LL_INTERNAL_SOURCE 1

#include "LinkerInternals.h"
#include "linker/util.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>

typedef uint32_t inst_t;

/* Keep the extracted helpers local to this unit even on a native RISC-V host
 * whose support archive contains the backend definitions. */
#define checkInt riscv_u_type_checkInt
#define read32le riscv_u_type_read32le
#define write32le riscv_u_type_write32le
#define setUType riscv_u_type_setUType

/* RISCV_U_TYPE_HELPER_DEFINITIONS */

struct encoding_case {
    int32_t value;
    uint32_t initial;
    uint32_t expected;
};

int main(void)
{
    static const struct encoding_case cases[] = {
        { INT32_MIN, UINT32_C(0x00000037), UINT32_C(0x80000037) },
        { -4097, UINT32_C(0x00000593), UINT32_C(0xfffff593) },
        { -4096, UINT32_C(0x00000abc), UINT32_C(0xfffffabc) },
        { -2049, UINT32_C(0x00000037), UINT32_C(0xfffff037) },
        { -2048, UINT32_C(0x00000593), UINT32_C(0x00000593) },
        { -1, UINT32_C(0x00000abc), UINT32_C(0x00000abc) },
        { 0, UINT32_C(0x00000037), UINT32_C(0x00000037) },
        { 2047, UINT32_C(0x00000593), UINT32_C(0x00000593) },
        { 2048, UINT32_C(0x00000abc), UINT32_C(0x00001abc) },
        { INT32_C(0x12345000), UINT32_C(0x00000593), UINT32_C(0x12345593) },
        { INT32_MAX - 2048, UINT32_C(0x00000abc), UINT32_C(0x7ffffabc) },
        { INT32_MAX, UINT32_C(0x00000037), UINT32_C(0x80000037) }
    };
    ll_linker linker;
    memset(&linker, 0, sizeof(linker));

    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        inst_t instruction = cases[i].initial;
        (void)setUType(&linker, &instruction, cases[i].value);
        uint32_t actual = read32le(&instruction);
        if (linker.error_status != LL_OK) {
            fprintf(stderr, "case %zu (%d): linker status is %d, expected LL_OK\n",
                    i, cases[i].value, linker.error_status);
            return 1;
        }
        if (actual != cases[i].expected) {
            fprintf(stderr, "case %zu (%d): got %08x, expected %08x\n",
                    i, cases[i].value, actual, cases[i].expected);
            return 1;
        }
    }

    puts("riscv U-type helper test: PASS (12 instruction-word cases)");
    return 0;
}
