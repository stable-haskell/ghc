#ifndef HSC2HS_CROSS_ASM_C_H
#define HSC2HS_CROSS_ASM_C_H

#include <stdint.h>
#include <stddef.h>
#include <limits.h>

#define TEST_COMPLEX_STR "hsc2hs-\"quote\"-\\slash-\n99-end"
static const char kStaticArrayStr[] = "array-decay-ok";
#define TEST_MAP_FAILED ((void *)-1)

/* High-bit UTF-8 + lone invalid UTF-8 continuation bytes (0xFF, 0xFE, 0x80).
 * Tests both:
 *   1. `#const_str` byte-exact escaping of non-ASCII bytes; and
 *   2. `ASM.hs` reading `_hsc_make.s` under `LC_ALL=C` without `getLocaleEncoding`
 *      throwing `invalid byte sequence`! */
#define TEST_NON_UTF8_BYTES "raw-\xff\xfe\x80-ok"
const char g_force_asm_bytes[] = "asm-\xed\x95\x9c-\xff\xfe-bytes";

enum test_status_e {
    TS_OK           = 0,
    _TS_LEADING     = 7,
    TS_2_WORDS      = 42,
    TS_NEG_ERR      = -13
};

struct test_layout_s {
    uint8_t  tag;
    uint32_t u_bf : 5;
    int32_t  s_bf : 7;
    double   val;
    void    *ptr;
};

#endif
