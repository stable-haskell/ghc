/* Capture COFF x64 relocation bytes without executing object code. */
#define main archive_status_original_main
#include "../unit/archive_error_status.c"
#undef main
#include "Linker.c"
#include "linker/PEi386.h"
#include "linker/PEi386Services.h"
#include "ll_format.h"
#include <inttypes.h>

static uintptr_t capture_image_base;
#ifndef LL_CAPTURE_REAL_IMAGE_BASE
bool capture_pe_image_base(ll_linker *owner, uintptr_t *base)
{
    (void)owner;
    *base = capture_image_base;
    return true;
}
#endif

static void print_bytes(const uint8_t *bytes, size_t size)
{
    for (size_t i = 0; i < size; ++i) (void)printf("%02x", bytes[i]);
}

int main(int argc, char **argv)
{
    bool noexec = argc == 3 && strcmp(argv[1], "--noexec") == 0;
    if (argc != 2 && !noexec) {
        (void)fprintf(stderr, "usage: coff_capture [--noexec] object\n");
        return 2;
    }
    const char *path = argv[noexec ? 2 : 1];
    FILE *input = fopen(path, "rb");
    if (input == NULL || fseek(input, 0, SEEK_END) != 0) return 1;
    long length = ftell(input);
    if (length <= 0 || length > 1024 * 1024 || fseek(input, 0, SEEK_SET) != 0) return 1;
    size_t size = (size_t)length;
    ll_linker owner = {0};
    ll_compat_host_ops ops = make_ops(&host_a);
    ll_compat_config config = {.always_pic = true,
        .flags = noexec ? LL_CFG_NO_EXEC | LL_CFG_ALL_FORMATS : 0,
        .noexec_base = noexec ? UINT64_C(0x0fffff00) : 0};
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
    char *image = LL_MALLOC_FOR(&owner, size, "COFF capture image");
    if (image == NULL || fread(image, 1, size, input) != size || fclose(input) != 0) return 1;
    ObjectCode *object = mkOc(&owner, STATIC_OBJECT, (pathchar *)path, image,
                              (int)size, false, NULL, 0);
    if (object == NULL) return 1;
    if (!ll_init_object(object)) {
        (void)fprintf(stderr, "COFF selection failed: %d %s\n", owner.error_status, owner.error_message);
        return 1;
    }
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    if (!object->format_ops->verify(object) || !object->format_ops->get_names(object)) {
        (void)fprintf(stderr, "COFF preparation failed: %d %s\n", owner.error_status, owner.error_message);
        return 1;
    }
    Section *site_section = NULL;
    COFF_reloc record = {0};
    for (int i = 0; i < object->n_sections; ++i) {
        Section *section = &object->sections[i];
        if (noexec && section->size != 0 && section->start != NULL &&
            (section->alloc != SECTION_NOEXEC ||
             (uintptr_t)section->start == section->exec_start)) return 1;
        if (section->coff_info != NULL && section->coff_info->noRelocs != 0) {
            if (site_section != NULL || section->coff_info->noRelocs != 1) return 1;
            site_section = section;
            record = section->coff_info->relocs[0];
        }
    }
    if (site_section == NULL || site_section->size > 4096 ||
        record.VirtualAddress < site_section->coff_info->virtualAddr) return 1;
    size_t offset = (size_t)(record.VirtualAddress - site_section->coff_info->virtualAddr);
    size_t width = record.Type == 1 ? 8 : 4;
    if (offset > site_section->size || width > site_section->size - offset) return 1;
    uint8_t before[4096];
    memcpy(before, site_section->start, site_section->size);
    uintptr_t section_address = site_section->exec_start;
    uintptr_t target = record.Type == 1 ? UINT64_C(0x123456789abcdef0) : section_address + 0x100;
    capture_image_base = target - 0x200;
    const char *name = strstr(path, "import-thunk.o") != NULL
                     ? "__imp_ll_fixture_import" : "ll_fixture_target";
    ll_sym_type type = name[0] == '_' ? LL_SYM_INDIRECT_DATA : LL_SYM_DATA;
    if (!ghciInsertSymbolTable(&owner, (pathchar *)"COFF capture", name,
                               (void *)target, false, type, 0, NULL)) return 1;
    bool resolved = object->format_ops->resolve(object);
    if (!resolved) {
        (void)fprintf(stderr, "COFF resolution failed: %d %s\n", owner.error_status, owner.error_message);
        return 1;
    }
    (void)printf("{\"schema\":1,\"type\":%u,\"section_address\":\"0x%" PRIxPTR
                 "\",\"target\":\"0x%" PRIxPTR "\",\"image_base\":\"0x%" PRIxPTR
                 "\",\"offset\":%zu,\"before\":\"", (unsigned)record.Type,
                 section_address, target, capture_image_base, offset);
    print_bytes(before, site_section->size);
    (void)printf("\",\"after\":\"");
    print_bytes(site_section->start, site_section->size);
    (void)puts("\"}");
    ghciRemoveSymbolTable(&owner, name, NULL);
    removeOcSymbols(object);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    freeObjectCode(object);
    ll_exit_linker(&owner);
    return failures != 0;
}
