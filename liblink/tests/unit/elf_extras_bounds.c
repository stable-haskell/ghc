/* Scalar bounds controls include the actual ELF verifier implementation. */
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include <stdio.h>
#include <stdint.h>
#include "linker/ElfVerify.c"
#ifndef EXPECTED_EXTRA_STRIDE
#error "supply the independently specified target payload width"
#endif
_Static_assert(LL_TARGET_SYMBOL_EXTRA_SIZE == EXPECTED_EXTRA_STRIDE,
               "target payload width differs from its fixed record");
static size_t page_size(void *ctx) { return *(const size_t *)ctx; }
static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "elf_extras_bounds:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)
int main(void)
{
    size_t page = 4096;
    ll_linker owner = {0};
    owner.services.host.ctx = &page;
    owner.services.host.page_size = page_size;
    ObjectCode oc = {.L = &owner};
    const uintmax_t width = EXPECTED_EXTRA_STRIDE;
    CHECK(elfExtrasFit(&oc, 64, 0, 0), "empty extras must fit");
    CHECK(elfExtrasFit(&oc, 64, 0, 1), "ordinary symbol table must fit");
    const uintmax_t divisor = width != 0 ? width : 1;
    const uintmax_t separate_budget = !RTS_LINKER_USE_MMAP ? (uintmax_t)INT_MAX - 64
        : USE_CONTIGUOUS_MMAP ? (uintmax_t)INT_MAX - 4096 : (uintmax_t)INT_MAX;
    if (width != 0) {
        const uintmax_t limit_count = separate_budget / divisor;
        CHECK(elfExtrasFit(&oc, 64, 0, limit_count), "exact separate extras boundary rejected");
        CHECK(!elfExtrasFit(&oc, 64, 0, limit_count + 1), "oversized extras accepted");
        CHECK(!elfExtrasFit(&oc, 64, 0, UINTMAX_MAX), "multiplication overflow accepted");
    } else {
        CHECK(elfExtrasFit(&oc, 64, 0, UINTMAX_MAX), "zero payload consumed symbol storage");
    }
    owner.services.cfg.always_pic = true;
    const uintmax_t budget = (uintmax_t)INT_MAX - (RTS_LINKER_USE_MMAP ? 4096 : 64);
    if (width != 0) {
        const uintmax_t limit_count = budget / divisor;
        CHECK(elfExtrasFit(&oc, 64, 0, limit_count), "exact combined boundary rejected");
        CHECK(!elfExtrasFit(&oc, 64, 0, limit_count + 1), "combined overflow accepted");
    } else {
        CHECK(elfExtrasFit(&oc, 64, 0, UINTMAX_MAX), "zero payload changed combined size");
    }
    page = 0;
    if (RTS_LINKER_USE_MMAP)
        CHECK(!elfExtrasFit(&oc, 64, 0, 1), "invalid page size accepted");
    else
        CHECK(elfExtrasFit(&oc, 64, 0, 1), "non-mmap bound consulted page size");
    if (failures != 0) return 1;
    (void)puts("elf_extras_bounds: PASS");
    return 0;
}
