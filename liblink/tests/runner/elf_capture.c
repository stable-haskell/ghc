/* Capture native-address ELF backend results without running initializers. */
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/ElfTypes.h"
#include "linker/MMap.h"
#include "ll_format.h"
#include "ll_path.h"

#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if !defined(x86_64_HOST_ARCH) || defined(mingw32_HOST_OS)
#error "elf_capture currently requires a POSIX x86_64 host configuration"
#endif

#define CAPTURE_LIMIT ((size_t)64 * 1024 * 1024)

typedef struct {
    uintptr_t function_address;
} CaptureHost;

static size_t page_size(void *ctx)
{
    (void)ctx;
    const long size = sysconf(_SC_PAGESIZE);
    if (size <= 0) abort();
    return (size_t)size;
}

static void memory_failure(void *ctx)
{
    (void)ctx;
    (void)fputs("elf_capture: host mapping failed\n", stderr);
}

static void populate_builtins(void *ctx, int retain_cafs,
                             ll_builtin_fn insert, void *insert_ctx)
{
    (void)ctx;
    (void)retain_cafs;
    /* Inputs are resolver values only. These addresses are never accessed. */
    insert(insert_ctx, "external_data", (void *)(uintptr_t)0x12345000,
           LL_STRENGTH_STRONG, LL_SYM_DATA);
}

static char *read_image(const char *path, int *size)
{
    FILE *file = fopen(path, "rb");
    if (file == NULL) return NULL;
    if (fseek(file, 0, SEEK_END) != 0) {
        (void)fclose(file);
        return NULL;
    }
    const long bytes = ftell(file);
    if (bytes <= 0 || (uintmax_t)bytes > CAPTURE_LIMIT || bytes > INT_MAX ||
        fseek(file, 0, SEEK_SET) != 0) {
        (void)fclose(file);
        return NULL;
    }
    char *image = malloc((size_t)bytes);
    if (image == NULL) {
        (void)fclose(file);
        return NULL;
    }
    const bool complete = fread(image, 1, (size_t)bytes, file) == (size_t)bytes &&
                          !ferror(file);
    const int closed = fclose(file);
    if (!complete || closed != 0) {
        free(image);
        return NULL;
    }
    *size = (int)bytes;
    return image;
}

static void json_string(const char *text)
{
    (void)putchar('"');
    for (const unsigned char *p = (const unsigned char *)text; *p != 0; ++p) {
        if (*p == '"' || *p == '\\') {
            (void)putchar('\\');
            (void)putchar(*p);
        } else if (*p < 0x20) {
            (void)printf("\\u%04x", (unsigned)*p);
        } else {
            (void)putchar(*p);
        }
    }
    (void)putchar('"');
}

static void json_bytes(const void *memory, size_t size)
{
    const unsigned char *bytes = memory;
    (void)putchar('"');
    for (size_t i = 0; i < size; ++i) (void)printf("%02x", (unsigned)bytes[i]);
    (void)putchar('"');
}

static void emit_capture(const ObjectCode *object, const CaptureHost *host,
                         uint8_t *const *before, const void *extras_before,
                         size_t extras_size, bool resolved)
{
    (void)printf("{\"schema\":\"liblink-elf-native-address-v1\","
                 "\"resolved\":%s,\"initializers_ran\":%s,\"status\":%d,"
                 "\"error\":", resolved ? "true" : "false",
                 object->initializersRan ? "true" : "false",
                 object->L->error_status);
    json_string(object->L->error_message);
    (void)printf(",\"host_symbols\":{\"external_data\":\"0x12345000\","
                 "\"external_function\":\"0x%" PRIxPTR "\","
                 "\"external_function_alias\":\"0x%" PRIxPTR "\"},"
                 "\"sections\":[", host->function_address, host->function_address);
    bool comma = false;
    for (int i = 0; i < object->n_sections; ++i) {
        const Section *section = &object->sections[i];
        if (section->start == NULL || section->size == 0) continue;
        if (comma) (void)putchar(',');
        comma = true;
        (void)printf("{\"index\":%d,\"address\":\"0x%" PRIxPTR
                     "\",\"size\":%zu,\"before\":", i,
                     (uintptr_t)section->start, (size_t)section->size);
        json_bytes(before[i], (size_t)section->size);
        (void)fputs(",\"after\":", stdout);
        json_bytes(section->start, (size_t)section->size);
        (void)putchar('}');
    }
    (void)printf("],\"extras\":{\"address\":\"0x%" PRIxPTR
                 "\",\"first\":%lu,\"count\":%lu,\"stride\":%zu,"
                 "\"jump_offset\":%zu,\"before\":",
                 (uintptr_t)object->symbol_extras, object->first_symbol_extra,
                 object->n_symbol_extras, object->symbol_extra_stride,
                 offsetof(ll_symbol_extra_x86_64, jumpIsland));
    json_bytes(extras_before, extras_size);
    (void)fputs(",\"after\":", stdout);
    json_bytes(object->symbol_extras, extras_size);
    (void)puts("}}");
}

int main(int argc, char **argv)
{
    const bool far_function = argc == 3 && strcmp(argv[1], "--far-function") == 0;
    if ((!far_function && argc != 2) || (argc == 3 && !far_function)) {
        (void)fputs("usage: elf_capture [--far-function] OBJECT\n", stderr);
        return 2;
    }
    const char *path = argv[far_function ? 2 : 1];
    int image_size = 0;
    char *image = read_image(path, &image_size);
    if (image == NULL) {
        (void)fputs("elf_capture: could not read bounded input image\n", stderr);
        return 2;
    }

    CaptureHost state = {far_function ? (uintptr_t)UINT64_C(0x700000000000) : 0};
    ll_linker owner = {0};
    ll_compat_host_ops host = {.ctx = &state, .page_size = page_size,
        .on_mem_failure = memory_failure, .populate_builtins = populate_builtins};
    ll_compat_config config = {.always_pic = true};
    ll_install_host_for(&owner, &host, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
    ObjectCode *object = mkOc(&owner, STATIC_OBJECT, (pathchar *)path, image,
                              image_size, false, NULL, 0);
    uint8_t **before = NULL;
    void *extras_before = NULL;
    int result = 2;
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    if (!ll_init_object(object) || object->format_ops != &ll_format_elf ||
        !loadOc(object)) goto done;
    if (object->n_sections < 0) goto done;
    before = calloc(object->n_sections == 0 ? 1 : (size_t)object->n_sections,
                    sizeof(*before));
    if (before == NULL) goto done;
    for (int i = 0; i < object->n_sections; ++i) {
        const Section *section = &object->sections[i];
        if (section->start == NULL || section->size == 0) continue;
        if (section->size > CAPTURE_LIMIT) goto done;
        before[i] = malloc((size_t)section->size);
        if (before[i] == NULL) goto done;
        memcpy(before[i], section->start, (size_t)section->size);
    }
    if (object->symbol_extra_stride != 0 &&
        (size_t)object->n_symbol_extras > CAPTURE_LIMIT / object->symbol_extra_stride)
        goto done;
    const size_t extras_size = object->symbol_extra_stride * object->n_symbol_extras;
    if (extras_size != 0) {
        extras_before = malloc(extras_size);
        if (extras_before == NULL) goto done;
        memcpy(extras_before, object->symbol_extras, extras_size);
    }
    /* Give ordinary PC-relative controls an in-range resolver input once the
     * host has chosen section addresses. This input is recorded in the capture;
     * it is not a value observed from a relocated field. Far mode keeps its
     * explicit input. Neither address is called or dereferenced. */
    if (!far_function) {
        for (int i = 0; i < object->n_sections; ++i) {
            if (object->sections[i].start != NULL && object->sections[i].size != 0) {
                state.function_address = (uintptr_t)object->sections[i].start + 0x1000;
                break;
            }
        }
        if (state.function_address == 0) goto done;
    }
    static const char *const names[] = {"external_function", "external_function_alias"};
    for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); ++i) {
        if (!ghciInsertSymbolTable(&owner, WSTR("elf_capture"), names[i],
                                  (void *)state.function_address,
                                  LL_STRENGTH_STRONG, LL_SYM_CODE, 0, NULL))
            goto done;
    }
    /* This unregistered object reaches the real backend relocation operation.
     * It never enters the initializer/finalizer pass or executes loaded code. */
    const bool resolved = object->format_ops->resolve(object) != 0;
    emit_capture(object, &state, before, extras_before, extras_size, resolved);
    result = fflush(stdout) != 0 || ferror(stdout) ? 2 : (resolved ? 0 : 1);
done:
    if (result == 2)
        (void)fprintf(stderr, "elf_capture: setup failed: %s\n", owner.error_message);
    if (before != NULL) {
        for (int i = 0; i < object->n_sections; ++i) free(before[i]);
    }
    free(before);
    free(extras_before);
    ll_discard_object(object);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    return result;
}
