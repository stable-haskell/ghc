/* Actual allocator arithmetic/failure controls, without executable memory. */
#define LL_INTERNAL_SOURCE 1
#include "ll_noexec.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static bool fail_heap;
static void *test_calloc(size_t count, size_t bytes)
{
    return fail_heap ? NULL : calloc(count, bytes);
}
#define calloc test_calloc
#include "ll_noexec.c"
#undef calloc

static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "noexec_storage:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

static void reject(ll_linker *L, size_t size, size_t alignment, ll_status status)
{
    const uintptr_t next_address = L->noexec_next;
    ll_noexec_block block = {.allocation = (void *)(uintptr_t)1,
                            .site = {.work = (void *)(uintptr_t)2, .exec = 3}};
    CHECK(!ll_noexec_allocate(L, size, alignment, &block), "invalid allocation succeeded");
    CHECK(L->error_status == status && L->noexec_next == next_address &&
          block.allocation == (void *)(uintptr_t)1 &&
          block.site.work == (void *)(uintptr_t)2 && block.site.exec == 3,
          "failure changed output/next_address or lost status");
}

int main(void)
{
    ll_linker owner = {0};
    owner.services.cfg.flags = LL_CFG_NO_EXEC;
    owner.noexec_next = UINT32_C(0x10000001);
    ll_noexec_block a, b;
    if (!ll_noexec_allocate(&owner, 37, 64, &a)) return 1;
    CHECK(a.site.exec == UINT32_C(0x10001000) &&
          owner.noexec_next == UINT32_C(0x10002000) &&
          ((uintptr_t)a.site.work & 63) == 0 &&
          (uintptr_t)a.site.work != a.site.exec, "work/exec alignment differs");
    for (size_t i = 0; i < 37; ++i) CHECK(a.site.work[i] == 0, "heap was not zeroed");
    if (!ll_noexec_allocate(&owner, 0, 1, &b)) { free(a.allocation); return 1; }
    CHECK(b.site.work != NULL &&
          b.site.exec == UINT32_C(0x10002000) &&
          owner.noexec_next == UINT32_C(0x10003000), "zero extent lost its unique address");
    free(a.allocation);
    free(b.allocation);
    reject(&owner, 1, 0, LL_E_FORMAT);
    reject(&owner, 1, 3, LL_E_FORMAT);
    reject(&owner, SIZE_MAX, 16, LL_E_NOMEM);
    fail_heap = true;
    reject(&owner, 16, 8, LL_E_NOMEM);
    fail_heap = false;
    owner.noexec_next = UINTPTR_MAX - 8191;
    reject(&owner, 8192, 1, LL_E_NOMEM);
    owner.noexec_next = UINTPTR_MAX;
    reject(&owner, 1, 4096, LL_E_NOMEM);
    owner.noexec_next = 0;
    reject(&owner, 1, 8, LL_E_STATE);
#if UINTPTR_MAX > UINT32_MAX && (LL_TARGET_I386 || LL_TARGET_ARM)
    owner.noexec_next = UINT64_C(0xfffff000);
    if (!ll_noexec_allocate(&owner, 4096, 1, &a)) return 1;
    CHECK(a.site.exec == UINT32_C(0xfffff000) &&
          owner.noexec_next == UINT64_C(0x100000000),
          "last 32-bit execution page was rejected");
    free(a.allocation);
    reject(&owner, 1, 1, LL_E_NOMEM);
    owner.noexec_next = UINT64_C(0xfffff000);
    reject(&owner, 4097, 1, LL_E_NOMEM);
#elif UINTPTR_MAX > UINT32_MAX
    owner.noexec_next = UINT64_C(0x7ffffffffffff000);
    if (!ll_noexec_allocate(&owner, 8192, 16, &a)) return 1;
    CHECK(a.site.exec == UINT64_C(0x7ffffffffffff000) &&
          owner.noexec_next == UINT64_C(0x8000000000001000),
          "unsigned execution domain did not cross INT64_MAX");
    free(a.allocation);
#endif
    if (failures != 0) return 1;
    (void)puts("noexec_storage: PASS (alignment, zero, failures, unsigned range)");
    return 0;
}
