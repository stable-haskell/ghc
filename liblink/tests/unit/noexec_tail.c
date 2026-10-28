/* Actual heap reservation with aligned tail; no object admission or execution. */
#define LL_INTERNAL_SOURCE 1
#include "ll_noexec.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static bool fail_heap;
static size_t calls;
static void *test_calloc(size_t n, size_t size)
{
    ++calls;
    return fail_heap ? NULL : calloc(n, size);
}
#define calloc test_calloc
#include "ll_noexec.c"
#undef calloc
static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "noexec_tail:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)
static size_t page_size(void *ctx) { return *(size_t *)ctx; }
static void reject(ll_linker *owner, size_t size, size_t align,
                   size_t tail, size_t tail_align, ll_status status, bool allocates)
{
    const uintptr_t next = owner->noexec_next;
    const size_t prior_calls = calls;
    ll_noexec_block out = {.allocation = (void *)(uintptr_t)1,
        .site = {.work = (void *)(uintptr_t)2, .exec = 3}};
    size_t offset = 17;
    CHECK(!ll_noexec_allocate_tail(owner, size, align, tail, tail_align, &out, &offset),
          "invalid reservation succeeded");
    CHECK(owner->error_status == status && owner->noexec_next == next &&
          out.allocation == (void *)(uintptr_t)1 && out.site.work == (void *)(uintptr_t)2 &&
          out.site.exec == 3 && offset == 17, "failure changed output/address or lost status");
    CHECK(calls == prior_calls + (allocates ? 1 : 0), "invalid extent reached allocation");
}
int main(void)
{
    for (size_t page = 4096; page <= 16384; page *= 4) {
        ll_linker owner = {0};
        owner.services.cfg.flags = LL_CFG_NO_EXEC;
        owner.services.host.ctx = &page;
        owner.services.host.page_size = page_size;
        owner.noexec_next = UINT32_C(0x10000001);
        const uintptr_t initial_next = owner.noexec_next;
        const size_t initial_calls = calls;
        ll_noexec_block sentinel = {.allocation = (void *)(uintptr_t)1,
            .site = {.work = (void *)(uintptr_t)2, .exec = 3}};
        size_t sentinel_offset = 17;
        CHECK(!ll_noexec_allocate_tail(NULL, 5, 1, 20, 4, &sentinel, &sentinel_offset) &&
              sentinel.allocation == (void *)(uintptr_t)1 &&
              sentinel.site.work == (void *)(uintptr_t)2 && sentinel.site.exec == 3 &&
              sentinel_offset == 17 && calls == initial_calls,
              "NULL owner changed outputs or attempted allocation");
        CHECK(!ll_noexec_allocate_tail(&owner, 5, 1, 20, 4, NULL, &sentinel_offset) &&
              owner.error_status == LL_E_STATE && sentinel_offset == 17 &&
              owner.noexec_next == initial_next && calls == initial_calls,
              "NULL block output changed address/offset or attempted allocation");
        CHECK(!ll_noexec_allocate_tail(&owner, 5, 1, 20, 4, &sentinel, NULL) &&
              owner.error_status == LL_E_STATE &&
              sentinel.allocation == (void *)(uintptr_t)1 &&
              sentinel.site.work == (void *)(uintptr_t)2 && sentinel.site.exec == 3 &&
              owner.noexec_next == initial_next && calls == initial_calls,
              "NULL offset output changed block/address or attempted allocation");
        ll_noexec_block block;
        size_t offset = 0;
        CHECK(ll_noexec_allocate_tail(&owner, 5, 1, 40, 4, &block, &offset),
              "ordinary payload/tail failed");
        CHECK(offset == 8 && block.site.exec == UINT32_C(0x10001000) &&
              owner.noexec_next == UINT32_C(0x10002000) &&
              (uintptr_t)(block.site.work + offset) % 4 == 0 &&
              (block.site.exec + offset) % 4 == 0, "paired tail alignment/layout differs");
        for (size_t i = 0; i < offset + 40; ++i)
            CHECK(block.site.work[i] == 0, "payload/padding/tail was not zeroed");
        free(block.allocation);
        CHECK(ll_noexec_allocate_tail(&owner, 4097, 8192, 20, 4, &block, &offset) &&
              offset == 4100 && (uintptr_t)block.site.work % 8192 == 0 &&
              block.site.exec % 8192 == 0, "payload alignment was not retained");
        free(block.allocation);
        ll_linker plain = owner;
        ll_noexec_block other;
        CHECK(ll_noexec_allocate_tail(&owner, 3, 1, 0, 0, &block, &offset) &&
              ll_noexec_allocate(&plain, 3, 1, &other) && offset == 3 &&
              block.site.exec == other.site.exec && owner.noexec_next == plain.noexec_next,
              "zero tail changed payload-only layout");
        free(block.allocation);
        free(other.allocation);
        reject(&owner, 5, 3, 20, 4, LL_E_FORMAT, false);
        reject(&owner, 5, 0, 20, 4, LL_E_FORMAT, false);
        reject(&owner, 5, 1, 20, 3, LL_E_FORMAT, false);
        reject(&owner, SIZE_MAX, 1, 20, 4, LL_E_NOMEM, false);
        reject(&owner, SIZE_MAX - 3, 1, 4, 4, LL_E_NOMEM, false);
        owner.noexec_next = UINTPTR_MAX - 4095;
        reject(&owner, 5, 1, 20, 4, LL_E_NOMEM, false);
        owner.noexec_next = UINT32_C(0x10000000);
        fail_heap = true;
        reject(&owner, 5, 1, 20, 4, LL_E_NOMEM, true);
        fail_heap = false;
    }
    if (failures != 0) return 1;
    (void)puts("noexec_tail: PASS (alignment, extents, failures, host page independence)");
    return 0;
}
