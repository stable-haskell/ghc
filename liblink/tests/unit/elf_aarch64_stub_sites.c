/* Data-only paired ELF stub addresses through the actual generic/A64 bodies. */
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "linker/elf_plt.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !LL_BUILD_ELF || !LL_TARGET_AARCH64
#error "elf_aarch64_stub_sites requires the AArch64 ELF target"
#endif
#include "linker/elf_plt.c"
#include "linker/elf_plt_aarch64.c"

static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "elf_aarch64_stub_sites:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

static void *allocate(void *ctx, size_t count, size_t size, const char *tag)
{
    (void)tag;
    void *p = calloc(count, size);
    if (p == NULL) abort();
    ++*(size_t *)ctx;
    return p;
}

static void release(void *ctx, void *p)
{
    if (p != NULL) {
        --*(size_t *)ctx;
        free(p);
    }
}

static void fatal(void *ctx, const char *message, va_list args)
{
    (void)ctx;
    (void)vfprintf(stderr, message, args);
    abort();
}

static void check_veneer(const uint8_t *work, uint64_t target)
{
    uint32_t words[5];
    memcpy(words, work, sizeof(words));
    const uint32_t opcodes[5] = {
        UINT32_C(0xd2e00010), UINT32_C(0xf2c00010),
        UINT32_C(0xf2a00010), UINT32_C(0xf2800010), UINT32_C(0xd61f0200)
    };
    uint64_t decoded = 0;
    for (size_t i = 0; i < 4; ++i) {
        CHECK((words[i] & ~UINT32_C(0x001fffe0)) == opcodes[i],
              "veneer opcode/register/halfword differs");
        decoded = (decoded << 16) | ((words[i] >> 5) & UINT32_C(0xffff));
    }
    CHECK(decoded == target && words[4] == opcodes[4], "veneer target or branch differs");
}

static void check_mode(bool separate)
{
    enum { PREFIX = 32, CAPACITY = 4 * 20, SUFFIX = 8 };
    uint32_t backing[(PREFIX + CAPACITY + SUFFIX) / sizeof(uint32_t)];
    uint8_t *bytes = (uint8_t *)backing;
    memset(backing, 0xa5, sizeof(backing));
    struct ll_elf_section_info info = {.stub_offset = bytes + PREFIX,
                                      .stub_size = CAPACITY};
    Section section = {.start = bytes, .size = PREFIX, .elf_info = &info,
        .exec_start = separate ? UINT32_C(0x10000000) : (uintptr_t)bytes};
    size_t allocations = 0;
    ll_linker owner = {0};
    owner.services.host.ctx = &allocations;
    owner.services.host.calloc_ = allocate;
    owner.services.host.free_ = release;
    owner.services.host.fatal = fatal;
    void *target = (void *)(uintptr_t)UINT64_C(0x123456789abcdef0);
    void *other = (void *)(uintptr_t)UINT64_C(0x700012345678);
    ll_site site = {.work = bytes, .exec = 17};
    CHECK(elf_findStubSite(&section, target, 0, &site) == EXIT_FAILURE &&
          site.work == bytes && site.exec == 17 && allocations == 0,
          "empty lookup changed output or allocated");
    CHECK(elf_makeStubSite(&owner, &section, target, NULL, 0, &site) == EXIT_SUCCESS,
          "paired creation failed");
    CHECK(site.work == bytes + PREFIX && site.exec == section.exec_start + PREFIX &&
          info.nstubs == 1 && allocations == 1, "paired creation lost address or count");
    check_veneer(site.work, (uintptr_t)target);
    ll_site found = {.work = bytes, .exec = 19};
    CHECK(elf_findStubSite(&section, target, 0, &found) == EXIT_SUCCESS &&
          found.work == site.work && found.exec == site.exec && allocations == 1,
          "repeat lookup did not reuse the original stub");
    void *legacy = target;
    CHECK(elf_findStub(&section, &legacy, 0) == EXIT_SUCCESS && legacy == site.work,
          "legacy lookup no longer returns native work address");
    found = (ll_site){.work = bytes, .exec = 19};
    CHECK(elf_findStubSite(&section, other, 0, &found) == EXIT_FAILURE &&
          found.work == bytes && found.exec == 19, "target miss changed output");
    CHECK(elf_findStubSite(&section, target, 1, &found) == EXIT_FAILURE &&
          found.work == bytes && found.exec == 19, "flag miss changed output");
    CHECK(elf_makeStubSite(&owner, &section, target, NULL, 1, &found) == EXIT_SUCCESS &&
          found.work == bytes + PREFIX + 20 &&
          found.exec == section.exec_start + PREFIX + 20, "flag identity lost its slot");
    CHECK(elf_makeStubSite(&owner, &section, other, NULL, 0, &found) == EXIT_SUCCESS &&
          found.work == bytes + PREFIX + 40 &&
          found.exec == section.exec_start + PREFIX + 40, "distinct target lost its slot");
    check_veneer(found.work, (uintptr_t)other);
    legacy = target;
    CHECK(elf_makeStub(&owner, &section, &legacy, NULL, 0) == EXIT_SUCCESS &&
          legacy == bytes + PREFIX + 60 &&
          memcmp(legacy, site.work, 20) == 0, "native creation bytes/order changed");
    CHECK(elf_findStubSite(&section, target, 0, &found) == EXIT_SUCCESS &&
          found.work == site.work && found.exec == site.exec,
          "lookup no longer selects the first matching target/flags");
    CHECK(info.nstubs == 4 && allocations == 4 &&
          info.stubs->target == target && info.stubs->flags == 0 &&
          info.stubs->next->flags == 1 && info.stubs->next->next->target == other,
          "stub identity/order/count changed");
    for (size_t i = 0; i < PREFIX; ++i) CHECK(bytes[i] == 0xa5, "section prefix changed");
    for (size_t i = PREFIX + CAPACITY; i < sizeof(backing); ++i)
        CHECK(bytes[i] == 0xa5, "section suffix changed");
    /* Fixture-owned nodes are released explicitly. This test does not qualify
     * the independent legacy whole-list disposal path. */
    while (info.stubs != NULL) {
        ElfStub *node = info.stubs;
        info.stubs = node->next;
        release(&allocations, node);
    }
    CHECK(allocations == 0, "fixture nodes leaked");
}

int main(void)
{
    check_mode(false);
    check_mode(true);
    if (failures != 0) return 1;
    (void)puts("elf_aarch64_stub_sites: PASS (paired/native addresses, identities, bytes, misses)");
    return 0;
}
