/* Portable checks of the production PAGE21 field writer, without executing code. */
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "macho_page21_generated.h"

/* Decode the architecture's split 21-bit page immediate independently of the
 * writer's shifts of the byte displacement. Sign conversion uses subtraction. */
static int64_t decode_pages(uint32_t instruction)
{
    const uint32_t immlo = (instruction >> 29) & UINT32_C(3);
    const uint32_t immhi = (instruction >> 5) & UINT32_C(0x7ffff);
    const uint32_t bits = immlo | (immhi << 2);
    return (bits & UINT32_C(0x100000)) != 0
        ? (int64_t)bits - INT64_C(0x200000) : (int64_t)bits;
}

static bool matches(uint32_t original, uint32_t encoded, int64_t pages)
{
    return decode_pages(encoded) == pages &&
           (encoded & ~UINT32_C(0x60ffffe0)) ==
           (original & ~UINT32_C(0x60ffffe0));
}

static bool check(uint32_t original, int64_t pages, int64_t low)
{
    const int64_t bytes = pages * INT64_C(4096) + low;
    const uint32_t actual = macho_page21_encode(original, bytes);
    if (!matches(original, actual, pages)) {
        (void)fprintf(stderr, "PAGE21 pages=%" PRId64 " low=%" PRId64
                      " original=%08" PRIx32 " encoded=%08" PRIx32 "\n",
                      pages, low, original, actual);
        return false;
    }
    return true;
}

int main(void)
{
    static const struct { int64_t pages; uint32_t word; } boundaries[] = {
        {-1048576, UINT32_C(0x90800000)},
        {-1048575, UINT32_C(0xb0800000)},
        {-3, UINT32_C(0xb0ffffe0)}, {-2, UINT32_C(0xd0ffffe0)},
        {-1, UINT32_C(0xf0ffffe0)}, {0, UINT32_C(0x90000000)},
        {1, UINT32_C(0xb0000000)}, {2, UINT32_C(0xd0000000)},
        {3, UINT32_C(0xf0000000)},
        {1048574, UINT32_C(0xd07fffe0)},
        {1048575, UINT32_C(0xf07fffe0)}
    };
    static const uint32_t originals[] = {
        UINT32_C(0x90000000), UINT32_C(0x9000001f),
        UINT32_C(0x9f00001f), UINT32_C(0xffffffff)
    };
    static const int64_t lows[] = {0, 1, 4095};
    uint64_t checks = 0;
    /* Discriminating negative controls for the independent checker. */
    if (matches(UINT32_C(0x90000000), UINT32_C(0xb0000000), 0) ||
        matches(UINT32_C(0x90000000), UINT32_C(0x90000001), 0)) {
        (void)fputs("PAGE21 checker accepted changed immediate/register bits\n", stderr);
        return 1;
    }
    for (size_t i = 0; i < sizeof(boundaries) / sizeof(boundaries[0]); ++i) {
        const int64_t pages = boundaries[i].pages;
        if (macho_page21_encode(UINT32_C(0x90000000), pages * INT64_C(4096)) !=
            boundaries[i].word) {
            (void)fprintf(stderr, "PAGE21 fixed boundary %zu differs\n", i);
            return 1;
        }
        ++checks;
        for (size_t w = 0; w < sizeof(originals) / sizeof(originals[0]); ++w)
            for (size_t lo = 0; lo < sizeof(lows) / sizeof(lows[0]); ++lo) {
                if (!check(originals[w], pages, lows[lo])) return 1;
                ++checks;
            }
    }
    /* Every representable signed 21-bit page delta, with four distinct
     * surrounding bit patterns. The range guard itself is outside this unit. */
    for (int64_t pages = -INT64_C(1048576); pages < INT64_C(1048576); ++pages)
        for (size_t w = 0; w < sizeof(originals) / sizeof(originals[0]); ++w) {
            if (!check(originals[w], pages, 0)) return 1;
            ++checks;
        }
    (void)printf("macho_page21_encoding: PASS (%" PRIu64
                 " encodings; positive/negative/boundary/bit-preservation cases)\n", checks);
    return 0;
}
