/* Replay one ELF32 golden using distinct work storage and execution addresses. */
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/ElfTypes.h"
#include "linker/elf_reloc_arm.h"
#include "linker/elf_reloc_i386.h"
#include "linker/elf_plt.h"
#include "ll_path.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void *allocate(void *ctx, size_t count, size_t size, const char *what)
{
    (void)ctx; (void)what;
    return calloc(count, size);
}
static void release(void *ctx, void *p) { (void)ctx; free(p); }
static unsigned nibble(char c)
{
    if (c >= '0' && c <= '9') return (unsigned)(c - '0');
    if (c >= 'a' && c <= 'f') return (unsigned)(c - 'a' + 10);
    abort();
}
static size_t decode(uint8_t *dst, const char *text)
{
    size_t n = strlen(text);
    if (n % 2 != 0 || n > 2 * 65536) abort();
    for (size_t i = 0; i < n / 2; ++i)
        dst[i] = (uint8_t)((nibble(text[2*i]) << 4) | nibble(text[2*i+1]));
    return n / 2;
}
static void emit(const uint8_t *bytes, size_t size)
{
    for (size_t i = 0; i < size; ++i) (void)printf("%02x", bytes[i]);
    (void)putchar('\n');
}
int main(int argc, char **argv)
{
    if (argc != 11) return 2;
    uint32_t type = (uint32_t)strtoul(argv[1], NULL, 0);
    uintptr_t exec = (uintptr_t)strtoull(argv[2], NULL, 0);
    uintptr_t symbol = (uintptr_t)strtoull(argv[3], NULL, 0);
    uintptr_t got = (uintptr_t)strtoull(argv[4], NULL, 0);
    uintptr_t slot = (uintptr_t)strtoull(argv[5], NULL, 0);
    uint32_t symbol_type = (uint32_t)strtoul(argv[6], NULL, 0);
    size_t offset = (size_t)strtoul(argv[7], NULL, 0);
    size_t stub_offset = (size_t)strtoul(argv[9], NULL, 0);
    size_t size = strlen(argv[8]) / 2, stub_size = strlen(argv[10]) / 2;
    if (size > 65536 || stub_size > 65536 || stub_offset > 65536 ||
        offset > size || size - offset < 4) return 2;
    size_t extent = size > stub_offset + stub_size ? size : stub_offset + stub_size;
    uint8_t *work = calloc(1, extent + 16);
    if (!work) return 2;
    if (decode(work, argv[8]) != size || decode(work + stub_offset, argv[10]) != stub_size) abort();
    ll_linker owner = {0};
    owner.services.host.calloc_ = allocate;
    owner.services.host.free_ = release;
    struct ll_elf_object_info object_info = {.got_exec = got};
    struct ll_elf_section_info section_info = {.stub_offset = work + stub_offset,
                                               .stub_size = stub_size};
    Section section = {.start = (char *)work, .exec_start = exec - offset,
                       .size = size, .elf_info = &section_info};
    ObjectCode object = {.L = &owner, .elf_info = &object_info, .fileName = WSTR("golden.o")};
    ll_site site = {.work = work + offset, .exec = exec};
    ll_reloc_context context = {.format_symbol_type = symbol_type, .got_exec = slot};
    uint32_t addend;
    memcpy(&addend, site.work, sizeof(addend));
#if LL_TARGET_I386
    int ok = ll_elf_i386_reloc_ops.apply(&object, &section, type, site,
                                         symbol, addend, LL_SYM_DATA, &context);
#elif LL_TARGET_ARM
    int ok = ll_elf_arm_reloc_ops.apply(&object, &section, type, site,
                                        symbol, addend, LL_SYM_DATA, &context);
#else
#error "elf32_apply requires i386 or ARM"
#endif
    if (!ok) { free(work); return 1; }
    emit(work, size);
    emit(work + stub_offset, stub_size);
#if LL_TARGET_ARM
    elf_freeStubs(&owner, &section);
#endif
    free(work);
    return 0;
}
