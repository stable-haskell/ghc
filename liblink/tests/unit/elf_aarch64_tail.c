/* Low-level actual section builder; this does not admit A64 NO_EXEC objects. */
#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "LinkerInternals.h"
#include "ll_noexec.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !LL_BUILD_ELF || !LL_TARGET_AARCH64
#error "elf_aarch64_tail requires the AArch64 ELF target"
#endif
static bool fail_heap;
static size_t heap_allocations;
static void *heap_calloc(size_t count, size_t size)
{
    if (fail_heap) return NULL;
    void *p = calloc(count, size);
    if (p != NULL) ++heap_allocations;
    return p;
}
#define calloc heap_calloc
#include "ll_noexec.c"
#undef calloc
#include "linker/Elf.c"
static unsigned failures;
#define CHECK(test, message) do { if (!(test)) { \
    (void)fprintf(stderr, "elf_aarch64_tail:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)
static size_t page_size(void *ctx) { return *(size_t *)ctx; }
static void run_case(size_t page, SectionKind kind, bool bss, size_t size,
                      bool fail, bool branches)
{
    ll_linker owner = {0};
    ll_compat_host_ops ops = {.ctx = &page, .page_size = page_size};
    ll_compat_config cfg = {.flags = LL_CFG_NO_EXEC, .noexec_base = 0x10000000};
    ll_install_host_for(&owner, &ops, &cfg);
    char image[8] = {1, 2, 3, 4, 5, 6, 7, 8};
    ll_elf_target_shdr headers[2] = {{0}, {.sh_size = size, .sh_addralign = 1,
        .sh_type = bss ? LL_ELF_SHT_NOBITS : LL_ELF_SHT_PROGBITS}};
    ll_elf_target_rela rels[3] = {
        {.r_info = LL_ELF64_R_INFO(1, COMPAT_R_AARCH64_CALL26)},
        {.r_info = LL_ELF64_R_INFO(1, COMPAT_R_AARCH64_JUMP26)},
        {.r_info = LL_ELF64_R_INFO(1, COMPAT_R_AARCH64_ABS64)}};
    ElfRelocationATable table = {.targetSectionIndex = 1, .relocations = rels,
                                 .n_relocations = 3};
    struct ll_elf_object_info info = {.sectionHeader = headers,
                                    .relaTable = branches ? &table : NULL};
    Section sections[2] = {{0}};
    ObjectCode oc = {.L = &owner, .image = image, .sections = sections,
                     .n_sections = 2, .elf_info = &info};
    const uintptr_t next = owner.noexec_next;
    const size_t before = heap_allocations;
    fail_heap = fail;
    const int result = noexec_section_ELF(&oc, 1, kind, bss);
    fail_heap = false;
    Section *s = &sections[1];
    if (fail) {
        CHECK(!result && owner.error_status == LL_E_NOMEM &&
              owner.noexec_next == next && heap_allocations == before &&
              s->start == NULL && s->elf_info == NULL && s->mapped_start == NULL,
              "failed section reservation published storage or metadata");
        return;
    }
    CHECK(result && heap_allocations == before + 1, "section did not reserve one owner block");
    if (!result) return;
    const size_t capacity = branches && kind != SECTIONKIND_OTHER && !bss && size ? 40 : 0;
    CHECK(s->alloc == SECTION_NOEXEC && s->mapped_start != NULL &&
          s->size == size && s->exec_start == next &&
          s->exec_start != (uintptr_t)s->start, "section ownership or paired address differs");
    CHECK(s->elf_info->stub_size == capacity && s->elf_info->nstubs == 0 &&
          s->elf_info->stubs == NULL, "stub capacity or initial metadata differs");
    for (size_t i = 0; i < size; ++i)
        CHECK(((uint8_t *)s->start)[i] == (bss ? 0 : (uint8_t)image[i]),
              "payload copy/zero fill differs");
    if (capacity != 0) {
        CHECK(s->elf_info->stub_offset == (uint8_t *)s->start + 8 &&
              (uintptr_t)s->elf_info->stub_offset % 4 == 0 &&
              (s->exec_start + 8) % 4 == 0, "tail padding or alignment differs");
        for (size_t i = size; i < 8 + capacity; ++i)
            CHECK(((uint8_t *)s->start)[i] == 0, "padding or tail was not zeroed");
        ll_site stub;
        CHECK(elf_makeStubSite(&owner, s, (void *)(uintptr_t)0x12345678, NULL, 0, &stub)
              == EXIT_SUCCESS && stub.work == s->elf_info->stub_offset &&
              stub.exec == s->exec_start + 8, "actual emitter did not use reserved pair");
        ll_site second;
        CHECK(elf_makeStubSite(&owner, s, (void *)(uintptr_t)0x87654321, NULL, 0, &second)
              == EXIT_SUCCESS && second.work == stub.work + 20 &&
              second.exec == stub.exec + 20 && s->elf_info->nstubs == 2,
              "second counted veneer exceeded or lost its reserved slot");
        ll_site reused;
        CHECK(elf_findStubSite(s, (void *)(uintptr_t)0x12345678, 0, &reused)
              == EXIT_SUCCESS && reused.work == stub.work && reused.exec == stub.exec &&
              s->elf_info->nstubs == 2, "first veneer lookup did not reuse its slot");
        CHECK(memcmp(s->start, image, size) == 0, "veneer emission changed payload");
        for (size_t i = size; i < 8; ++i)
            CHECK(((uint8_t *)s->start)[i] == 0, "veneer emission changed padding");
        /* Nodes are fixture-owned here. The pending list-disposal repair is
         * deliberately not applied or qualified by this storage control. */
        ElfStub *node = s->elf_info->stubs;
        while (node != NULL) {
            ElfStub *next_node = node->next;
            LL_FREE_FOR(&owner, node);
            node = next_node;
        }
    } else CHECK(s->elf_info->stub_offset == NULL, "ineligible section acquired stub storage");
    free(s->mapped_start);
    --heap_allocations;
    LL_FREE_FOR(&owner, s->elf_info);
    freeProddableBlocks(&owner, &oc.proddables);
    CHECK(heap_allocations == before, "heap owner was not released");
}
int main(void)
{
    for (size_t page = 4096; page <= 16384; page *= 4) {
        run_case(page, SECTIONKIND_CODE_OR_RODATA, false, 5, false, true);
        run_case(page, SECTIONKIND_CODE_OR_RODATA, false, 5, false, false);
        run_case(page, SECTIONKIND_CODE_OR_RODATA, false, 5, true, true);
        run_case(page, SECTIONKIND_OTHER, false, 5, false, true);
        run_case(page, SECTIONKIND_RWDATA, true, 5, false, true);
        run_case(page, SECTIONKIND_CODE_OR_RODATA, false, 0, false, true);
    }
    if (failures != 0) return 1;
    (void)puts("elf_aarch64_tail: PASS (section bytes, capacity, paired tail, ownership, failures)");
    return 0;
}
