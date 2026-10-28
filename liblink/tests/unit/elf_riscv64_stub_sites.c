/* Data-only RISC-V veneer emission through generic paired and native paths. */
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "linker/elf_plt.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !LL_BUILD_ELF || !LL_TARGET_RISCV64
#error "elf_riscv64_stub_sites requires the RISC-V ELF target"
#endif
#include "linker/elf_plt.c"
#include "linker/elf_plt_riscv64.c"

static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "elf_riscv64_stub_sites:%d: %s\n", __LINE__, message); \
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
    if (p != NULL) { --*(size_t *)ctx; free(p); }
}

static void check_words(const uint8_t *work)
{
    uint32_t words[3];
    memcpy(words, work, sizeof(words));
    CHECK(words[0] == UINT32_C(0x00002f97) &&
          words[1] == UINT32_C(0x008fbf83) &&
          words[2] == UINT32_C(0x000f8067),
          "veneer does not encode the execution-relative GOT address");
}

static void check_mode(bool separate)
{
    enum { PREFIX = 32, CAPACITY = 24, SUFFIX = 8 };
    uint32_t backing[(PREFIX + CAPACITY + SUFFIX) / sizeof(uint32_t)];
    uint8_t *bytes = (uint8_t *)backing;
    memset(bytes, 0xa5, sizeof(backing));
    struct ll_elf_section_info info = {.stub_offset = bytes + PREFIX, .stub_size = CAPACITY};
    Section section = {.start = bytes, .size = PREFIX, .elf_info = &info,
        .exec_start = separate ? UINT32_C(0x10000000) : (uintptr_t)bytes};
    size_t allocations = 0;
    ll_linker owner = {0};
    owner.services.host.ctx = &allocations;
    owner.services.host.calloc_ = allocate;
    owner.services.host.free_ = release;
    void *target = (void *)(uintptr_t)UINT32_C(0x12345678);
    uintptr_t exec = section.exec_start + PREFIX;
    void *got = (void *)(exec + UINT32_C(0x2008));
    ll_site site = {.work = bytes, .exec = 17};
    CHECK(elf_findStubSite(&section, target, 0, &site) == EXIT_FAILURE &&
          site.work == bytes && site.exec == 17, "missing lookup changed its output");
    CHECK(elf_makeStubSite(&owner, &section, target, got, 0, &site) == EXIT_SUCCESS &&
          site.work == bytes + PREFIX && site.exec == exec, "paired stub site differs");
    check_words(site.work);
    ll_site found = {0};
    CHECK(elf_findStubSite(&section, target, 0, &found) == EXIT_SUCCESS &&
          found.work == site.work && found.exec == site.exec && allocations == 1,
          "target/flags lookup failed to reuse its stub");
    CHECK(elf_findStubSite(&section, target, 1, &found) == EXIT_FAILURE,
          "different flags reused the stub");
    for (size_t i = 0; i < PREFIX; ++i) CHECK(bytes[i] == 0xa5, "prefix changed");
    for (size_t i = PREFIX + 12; i < sizeof(backing); ++i)
        CHECK(bytes[i] == 0xa5, "unallocated bytes changed");
    ll_site negative = {0};
    uintptr_t negative_exec = exec + 12;
    void *negative_got = (void *)(negative_exec - UINT32_C(0x1ff8));
    CHECK(elf_makeStubSite(&owner, &section, target, negative_got, 1, &negative)
              == EXIT_SUCCESS && negative.exec == negative_exec,
          "negative displacement stub creation failed");
    uint32_t negative_words[3];
    memcpy(negative_words, negative.work, sizeof(negative_words));
    CHECK(negative_words[0] == UINT32_C(0xffffef97) &&
          negative_words[1] == UINT32_C(0x008fbf83) &&
          negative_words[2] == UINT32_C(0x000f8067),
          "negative GOT displacement was encoded incorrectly");
    elf_freeStubs(&owner, &section);
    CHECK(allocations == 0 && info.nstubs == 0 && info.stubs == NULL,
          "stub cleanup retained metadata");
    void *legacy = target;
    got = (void *)((uintptr_t)(bytes + PREFIX) + UINT32_C(0x2008));
    CHECK(elf_makeStub(&owner, &section, &legacy, got, 0) == EXIT_SUCCESS &&
          legacy == bytes + PREFIX, "legacy creation lost the work address");
    check_words(legacy);
    elf_freeStubs(&owner, &section);
    CHECK(allocations == 0, "legacy cleanup retained metadata");
}

int main(void)
{
    check_mode(false);
    check_mode(true);
    if (failures) return 1;
    (void)puts("elf_riscv64_stub_sites: PASS");
    return 0;
}
