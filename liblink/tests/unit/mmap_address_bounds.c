#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_linker.h"
#include "linker/MMap.h"

#include "../../src/linker/MMap.c"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

#if defined(mingw32_HOST_OS) || !RTS_LINKER_USE_MMAP
#error "mmap_address_bounds requires a POSIX RTS_LINKER_USE_MMAP configuration"
#endif

static unsigned failures;

#define CHECK(condition, ...)                                                \
    do {                                                                     \
        if (!(condition)) {                                                  \
            fprintf(stderr, "FAIL: ");                                      \
            fprintf(stderr, __VA_ARGS__);                                    \
            fputc('\n', stderr);                                             \
            failures++;                                                      \
        }                                                                    \
    } while (0)

static void test_zero_base(void)
{
    ll_linker owner = {0};
    struct MemoryRegion *region;

    owner.mmap_32bit_base = NULL;
    region = nearImage(&owner);
    CHECK(region == &owner.mmap_region, "zero base returned another owner's region");
    CHECK((uintptr_t)region->start == 0, "zero base start is 0x%" PRIxPTR,
          (uintptr_t)region->start);
    CHECK((uintptr_t)region->end == UINT32_C(0x80000000),
          "zero base end is 0x%" PRIxPTR, (uintptr_t)region->end);
    CHECK((uintptr_t)region->last == 0, "zero base last is 0x%" PRIxPTR,
          (uintptr_t)region->last);
}

static void test_ordinary_base(void)
{
    ll_linker owner = {0};
    const uintptr_t base = UINT32_C(0x10000000);
    struct MemoryRegion *region;

    owner.mmap_32bit_base = (void *)base;
    region = nearImage(&owner);
    CHECK((uintptr_t)region->start == base, "ordinary start is 0x%" PRIxPTR,
          (uintptr_t)region->start);
    CHECK((uintptr_t)region->end == base + UINT32_C(0x80000000),
          "ordinary end is 0x%" PRIxPTR, (uintptr_t)region->end);
    CHECK((uintptr_t)region->last == base, "ordinary last is 0x%" PRIxPTR,
          (uintptr_t)region->last);
}

static void test_last_representable_end(void)
{
    ll_linker owner = {0};
    const uintptr_t span = UINT32_C(0x80000000);
    const uintptr_t base = UINTPTR_MAX - span;
    struct MemoryRegion *region;

    owner.mmap_32bit_base = (void *)base;
    region = nearImage(&owner);
    CHECK((uintptr_t)region->start == base, "upper-bound start is 0x%" PRIxPTR,
          (uintptr_t)region->start);
    CHECK((uintptr_t)region->end == UINTPTR_MAX,
          "upper-bound end is 0x%" PRIxPTR, (uintptr_t)region->end);
}

static void test_cached_region(void)
{
    ll_linker owner = {0};
    const uintptr_t original_base = UINT32_C(0x20000000);
    const uintptr_t progressed = original_base + UINT32_C(0x1234);
    struct MemoryRegion *first;
    struct MemoryRegion *second;

    owner.mmap_32bit_base = (void *)original_base;
    first = nearImage(&owner);
    first->last = (void *)progressed;
    owner.mmap_32bit_base = (void *)UINT32_C(0x40000000);
    second = nearImage(&owner);
    CHECK(second == first, "cached region changed its owner-local address");
    CHECK((uintptr_t)second->start == original_base,
          "cached start was recomputed as 0x%" PRIxPTR, (uintptr_t)second->start);
    CHECK((uintptr_t)second->end == original_base + UINT32_C(0x80000000),
          "cached end was recomputed as 0x%" PRIxPTR, (uintptr_t)second->end);
    CHECK((uintptr_t)second->last == progressed,
          "cached last hint was reset to 0x%" PRIxPTR, (uintptr_t)second->last);
}

static void test_default_owner_isolation(void)
{
    ll_linker *owner = &ll_default_linker;
    const uintptr_t base = UINT32_C(0x30000000);
    struct MemoryRegion saved_region = owner->mmap_region;
    void *saved_base = owner->mmap_32bit_base;
    ll_linker local = {0};
    struct MemoryRegion *default_region;

    owner->mmap_region = (struct MemoryRegion){0};
    owner->mmap_32bit_base = (void *)base;
    local.mmap_32bit_base = (void *)UINT32_C(0x50000000);
    default_region = nearImage(owner);
    (void)nearImage(&local);

    CHECK(default_region == &owner->mmap_region,
          "default linker returned another owner's region");
    CHECK((uintptr_t)default_region->start == base,
          "default linker start is 0x%" PRIxPTR,
          (uintptr_t)default_region->start);
    CHECK((uintptr_t)default_region->end == base + UINT32_C(0x80000000),
          "default linker end is 0x%" PRIxPTR,
          (uintptr_t)default_region->end);
    CHECK((uintptr_t)local.mmap_region.start == UINT32_C(0x50000000),
          "local linker start changed to 0x%" PRIxPTR,
          (uintptr_t)local.mmap_region.start);

    owner->mmap_region = saved_region;
    owner->mmap_32bit_base = saved_base;
}

int main(void)
{
    test_zero_base();
    test_ordinary_base();
    test_last_representable_end();
    test_cached_region();
    test_default_owner_isolation();
    if (failures != 0) {
        fprintf(stderr, "mmap_address_bounds: %u failure(s)\n", failures);
        return 1;
    }
    puts("mmap_address_bounds: PASS");
    return 0;
}
