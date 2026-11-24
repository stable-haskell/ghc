/* Capture Mach-O relocations in private heap storage without executing code. */
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/MachOTypes.h"
#include "ll_format.h"
#include "ll_path.h"
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static size_t page_size(void *ctx)
{
    (void)ctx;
    long page = sysconf(_SC_PAGESIZE);
    return page > 0 ? (size_t)page : 0;
}

static char *read_image(const char *path, int *size)
{
    FILE *file = fopen(path, "rb");
    if (file == NULL) return NULL;
    if (fseek(file, 0, SEEK_END) != 0) { fclose(file); return NULL; }
    long length = ftell(file);
    if (length <= 0 || length > INT_MAX || fseek(file, 0, SEEK_SET) != 0) {
        fclose(file); return NULL;
    }
    char *image = malloc((size_t)length);
    if (image == NULL) { fclose(file); return NULL; }
    bool ok = fread(image, 1, (size_t)length, file) == (size_t)length;
    int closed = fclose(file);
    if (!ok || closed != 0) { free(image); return NULL; }
    *size = (int)length;
    return image;
}

static void bytes(const void *data, size_t count)
{
    const unsigned char *p = data;
    putchar('"');
    for (size_t i = 0; i < count; ++i) printf("%02x", p[i]);
    putchar('"');
}

int main(int argc, char **argv)
{
    bool far = argc == 3 && strcmp(argv[1], "--far") == 0;
    bool layout = argc == 7 && strcmp(argv[1], "--layout") == 0;
    if ((!far && !layout && argc != 2) || (far && argc != 3)) {
        fputs("usage: macho_noexec_capture [--far] OBJECT | --layout OBJECT SECTION GOT EXTRAS TARGET\n", stderr);
        return 2;
    }
    const char *path = argv[far || layout ? 2 : 1];
    int size = 0;
    char *image = read_image(path, &size);
    if (image == NULL) { fputs("cannot read object\n", stderr); return 2; }
    ll_linker owner = {0};
    ll_compat_config cfg = {.flags = LL_CFG_NO_EXEC | LL_CFG_ALL_FORMATS,
                            .noexec_base = 0x10000000};
    ll_compat_host_ops host = {.page_size = page_size};
    ll_install_host_for(&owner, &host, &cfg);
    ll_init_linker(&owner, 0);
    ObjectCode *oc = mkOc(&owner, STATIC_OBJECT, (pathchar *)path, image,
                          size, false, NULL, 0);
    if (oc == NULL) return 2;
    int result = 2;
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    if (!ll_init_object(oc) || oc->format_ops != &ll_format_macho || !loadOc(oc)) goto done;
    if (layout) {
        /* Reproduce the saved normal layout for a literal byte comparison.
         * Separate captures exercise the allocator's unmodified layout. */
        int section_index = -1;
        for (int i = 0; i < oc->n_sections; ++i) {
            if (oc->sections[i].size == 0) continue;
            if (section_index >= 0) goto done;
            section_index = i;
        }
        if (section_index < 0) goto done;
        uintptr_t section_exec = (uintptr_t)strtoull(argv[3], NULL, 0);
        uintptr_t got_exec = (uintptr_t)strtoull(argv[4], NULL, 0);
        uintptr_t extras_exec = (uintptr_t)strtoull(argv[5], NULL, 0);
        uintptr_t previous = oc->sections[section_index].exec_start;
        oc->sections[section_index].exec_start = section_exec;
        for (size_t i = 0; i < oc->macho_info->n_macho_symbols; ++i) {
            MachOSymbol *symbol = &oc->macho_info->macho_symbols[i];
            if ((symbol->nlist->n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_SECT &&
                symbol->nlist->n_sect == section_index + 1)
                symbol->addr = (void *)(section_exec + (uintptr_t)symbol->addr - previous);
            if (symbol->got_addr != NULL)
                symbol->got_exec = got_exec + ((uintptr_t)symbol->got_addr -
                                               (uintptr_t)oc->macho_info->got_start);
        }
        oc->macho_info->got_exec = got_exec;
        oc->symbol_extras_exec = extras_exec;
    }
    uintptr_t anchor = 0;
    for (int i = 0; i < oc->n_sections; ++i)
        if (oc->sections[i].size != 0) { anchor = oc->sections[i].exec_start; break; }
    if (anchor == 0) goto done;
    const uintptr_t target = layout ? (uintptr_t)strtoull(argv[6], NULL, 0) :
        (far ? (uintptr_t)UINT64_C(0x700000000000) : anchor + 0x3000);
    if (!ghciInsertSymbolTable(&owner, WSTR("macho capture"), "_ll_fixture_target",
            (void *)target, LL_STRENGTH_STRONG, LL_SYM_CODE, 0, NULL)) goto done;
    if (!oc->format_ops->resolve(oc)) goto done;
    printf("{\"target\":\"0x%" PRIxPTR "\",\"sections\":[", target);
    for (int i = 0; i < oc->n_sections; ++i) {
        Section *section = &oc->sections[i];
        if (i) putchar(',');
        printf("{\"index\":%d,\"size\":%zu,\"work\":\"0x%" PRIxPTR
               "\",\"exec\":\"0x%" PRIxPTR "\",\"bytes\":", i,
               (size_t)section->size, (uintptr_t)section->start, section->exec_start);
        bytes(section->start, section->size);
        struct ll_macho_section_info *info = section->macho_info;
        if (info != NULL && info->stub_size != 0) {
            printf(",\"stub_work\":\"0x%" PRIxPTR "\",\"stub_exec\":\"0x%" PRIxPTR
                   "\",\"stub_bytes\":", (uintptr_t)info->stub_offset,
                   section->exec_start + ((uintptr_t)info->stub_offset - (uintptr_t)section->start));
            bytes(info->stub_offset, info->stub_size);
            fputs(",\"stubs\":[", stdout);
            bool comma = false;
            for (MachOStub *stub = info->stubs; stub != NULL; stub = stub->next) {
                if (comma) putchar(',');
                comma = true;
                printf("{\"work\":\"0x%" PRIxPTR "\",\"target\":\"0x%" PRIxPTR "\"}",
                       (uintptr_t)stub->addr, (uintptr_t)stub->target);
            }
            putchar(']');
        }
        putchar('}');
    }
    fputs("],\"symbols\":[", stdout);
    for (size_t i = 0; i < oc->macho_info->n_macho_symbols; ++i) {
        MachOSymbol *symbol = &oc->macho_info->macho_symbols[i];
        if (i) putchar(',');
        printf("{\"exec\":\"0x%" PRIxPTR "\",\"got_exec\":\"0x%" PRIxPTR "\"}",
               (uintptr_t)symbol->addr, symbol->got_exec);
    }
    printf("],\"got\":{\"work\":\"0x%" PRIxPTR "\",\"exec\":\"0x%" PRIxPTR
           "\",\"bytes\":", (uintptr_t)oc->macho_info->got_start, oc->macho_info->got_exec);
    bytes(oc->macho_info->got_start, oc->macho_info->got_size);
    printf("},\"extras\":{\"work\":\"0x%" PRIxPTR "\",\"exec\":\"0x%" PRIxPTR
           "\",\"stride\":%zu,\"bytes\":", (uintptr_t)oc->symbol_extras,
           oc->symbol_extras_exec, oc->symbol_extra_stride);
    bytes(oc->symbol_extras, oc->n_symbol_extras * oc->symbol_extra_stride);
    puts("}}");
    result = ferror(stdout) ? 2 : 0;
done:
    if (result != 0) fprintf(stderr, "macho capture: %s\n", owner.error_message);
    ll_discard_object(oc);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    return result;
}
