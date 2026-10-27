/* Capture ELF backend work/exec layout without running initializers. */
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/ElfTypes.h"
#include "linker/MMap.h"
#include "ll_format.h"
#include "ll_path.h"
#include "ll_noexec.h"

#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if !LL_BUILD_ELF || !(LL_TARGET_X86_64 || LL_TARGET_AARCH64) || defined(mingw32_HOST_OS) || SIZEOF_VOID_P != 8
#error "elf_capture requires the ELF x64/AArch64 backend on a 64-bit POSIX host"
#endif

#define CAPTURE_LIMIT ((size_t)64 * 1024 * 1024)
#if LL_TARGET_AARCH64
#define CAPTURE_DATA_ADDRESS UINT32_C(0x123450f0)
#else
#define CAPTURE_DATA_ADDRESS UINT32_C(0x12345000)
#endif

typedef struct {
    uintptr_t function_address;
    uintptr_t data_address;
    uintptr_t stub_addresses[64];
    bool stub_live[64];
    size_t stub_count;
    size_t stub_releases;
} CaptureHost;

/* Track only metadata allocated by the real ELF stub creator. */
static void *probe_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    CaptureHost *host = ctx;
    void *p = calloc(count, size);
    if (p == NULL) abort();
    if (what != NULL && strcmp(what, "makeStub") == 0) {
        if (host->stub_count >= 64) abort();
        host->stub_addresses[host->stub_count] = (uintptr_t)p;
        host->stub_live[host->stub_count++] = true;
    }
    return p;
}

static void probe_free(void *ctx, void *p)
{
    CaptureHost *host = ctx;
    if (p != NULL) {
        const uintptr_t address = (uintptr_t)p;
        for (size_t i = 0; i < host->stub_count; ++i) {
            if (host->stub_live[i] && host->stub_addresses[i] == address) {
                host->stub_live[i] = false;
                ++host->stub_releases;
                break;
            }
        }
    }
    free(p);
}

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
    /* AArch64 data is defined after layout so PREL32 has an in-range input. */
#if LL_TARGET_AARCH64
    (void)insert;
    (void)insert_ctx;
#else
    insert(insert_ctx, "external_data", (void *)((CaptureHost *)ctx)->data_address,
           LL_STRENGTH_STRONG, LL_SYM_DATA);
#endif
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
                         size_t extras_size, uint8_t *const *stubs_before, bool resolved)
{
    (void)printf("{\"schema\":\"liblink-elf-native-address-v1\","
                 "\"resolved\":%s,\"initializers_ran\":%s,\"status\":%d,"
                 "\"error\":", resolved ? "true" : "false",
                 object->initializersRan ? "true" : "false",
                 object->L->error_status);
    json_string(object->L->error_message);
    (void)printf(",\"noexec\":%s,\"host_symbols\":{\"external_data\":\"0x%" PRIxPTR "\","
                 "\"external_function\":\"0x%" PRIxPTR "\","
                 "\"external_function_alias\":\"0x%" PRIxPTR "\","
                 "\"external_second_function\":\"0x%" PRIxPTR "\"},"
                 "\"sections\":[", ll_noexec_enabled(object->L) ? "true" : "false",
                 host->data_address, host->function_address, host->function_address,
                 host->function_address + 0x1000);
    bool comma = false;
    for (int i = 0; i < object->n_sections; ++i) {
        const Section *section = &object->sections[i];
        if (section->start == NULL || section->size == 0) continue;
        if (comma) (void)putchar(',');
        comma = true;
        (void)printf("{\"index\":%d,\"address\":\"0x%" PRIxPTR
                     "\",\"exec_address\":\"0x%" PRIxPTR
                     "\",\"size\":%zu,\"before\":", i,
                     (uintptr_t)section->start, section->exec_start, (size_t)section->size);
        json_bytes(before[i], (size_t)section->size);
        (void)fputs(",\"after\":", stdout);
        json_bytes(section->start, (size_t)section->size);
#if LL_TARGET_AARCH64
        const struct ll_elf_section_info *info = section->elf_info;
        (void)printf(",\"stubs\":{\"address\":\"0x%" PRIxPTR
                     "\",\"size\":%zu,\"before\":", (uintptr_t)info->stub_offset,
                     info->stub_size);
        json_bytes(stubs_before[i], info->stub_size);
        (void)fputs(",\"after\":", stdout);
        json_bytes(info->stub_offset, info->stub_size);
        (void)fputs(",\"entries\":[", stdout);
        bool first_stub = true;
        for (const ElfStub *stub = info->stubs; stub != NULL; stub = stub->next) {
            if (!first_stub) (void)putchar(',');
            first_stub = false;
            (void)printf("{\"address\":\"0x%" PRIxPTR "\",\"target\":\"0x%" PRIxPTR
                         "\",\"flags\":%u}", (uintptr_t)stub->addr,
                         (uintptr_t)stub->target, (unsigned)stub->flags);
        }
        (void)fputs("]}", stdout);
#else
        (void)stubs_before;
#endif
        (void)putchar('}');
    }
    (void)printf("],\"extras\":{\"address\":\"0x%" PRIxPTR
                 "\",\"exec_address\":\"0x%" PRIxPTR
                 "\",\"first\":%lu,\"count\":%lu,\"stride\":%zu,"
                 "\"jump_offset\":%zu,\"before\":",
                 (uintptr_t)object->symbol_extras, object->symbol_extras_exec,
                 object->first_symbol_extra,
                 object->n_symbol_extras, object->symbol_extra_stride,
                 offsetof(ll_symbol_extra_x86_64, jumpIsland));
    json_bytes(extras_before, extras_size);
    (void)fputs(",\"after\":", stdout);
    json_bytes(object->symbol_extras, extras_size);
    (void)printf("},\"got\":{\"address\":\"0x%" PRIxPTR
                 "\",\"exec_address\":\"0x%" PRIxPTR "\",\"size\":%zu,\"after\":",
                 (uintptr_t)object->elf_info->got_start, object->elf_info->got_exec,
                 object->elf_info->got_size);
    json_bytes(object->elf_info->got_start, object->elf_info->got_size);
    (void)puts("}}");
}

int main(int argc, char **argv)
{
    bool far_function = false, noexec = false, negative_data = false;
    int next = 1;
    while (next < argc && argv[next][0] == '-') {
        if (strcmp(argv[next], "--far-function") == 0) far_function = true;
        else if (strcmp(argv[next], "--noexec") == 0) noexec = true;
        else if (strcmp(argv[next], "--negative-data") == 0) negative_data = true;
        else break;
        ++next;
    }
    if (next != argc - 1) {
        (void)fputs("usage: elf_capture [--noexec] [--far-function] [--negative-data] OBJECT\n", stderr);
        return 2;
    }
#if !LL_TARGET_AARCH64
    if (negative_data) {
        (void)fputs("elf_capture: --negative-data requires the AArch64 target\n", stderr);
        return 2;
    }
#endif
    const char *path = argv[next];
    int image_size = 0;
    char *image = read_image(path, &image_size);
    if (image == NULL) {
        (void)fputs("elf_capture: could not read bounded input image\n", stderr);
        return 2;
    }

    CaptureHost state = {.function_address = far_function
        ? (uintptr_t)UINT64_C(0x700000000000) : 0,
        .data_address = CAPTURE_DATA_ADDRESS};
    ll_linker owner = {0};
    ll_compat_host_ops host = {.ctx = &state, .calloc_ = probe_calloc,
        .free_ = probe_free, .page_size = page_size,
        .on_mem_failure = memory_failure, .populate_builtins = populate_builtins};
    ll_compat_config config = {.always_pic = true,
        .flags = noexec ? LL_CFG_NO_EXEC | LL_CFG_ALL_FORMATS : 0,
        .noexec_base = UINT32_C(0x10000000)};
    ll_install_host_for(&owner, &host, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
    ObjectCode *object = mkOc(&owner, STATIC_OBJECT, (pathchar *)path, image,
                              image_size, false, NULL, 0);
    uint8_t **before = NULL;
    uint8_t **stubs_before = NULL;
    void *extras_before = NULL;
    int result = 2;
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    if (noexec && !ghciInsertSymbolTable(&owner, WSTR("elf_capture"), "external_data",
            (void *)(uintptr_t)CAPTURE_DATA_ADDRESS, LL_STRENGTH_STRONG,
            LL_SYM_DATA, 0, NULL)) goto done;
    if (!ll_init_object(object) || object->format_ops != &ll_format_elf ||
        !loadOc(object)) goto done;
    if (object->n_sections < 0) goto done;
    before = calloc(object->n_sections == 0 ? 1 : (size_t)object->n_sections,
                    sizeof(*before));
    if (before == NULL) goto done;
    stubs_before = calloc(object->n_sections == 0 ? 1 : (size_t)object->n_sections,
                          sizeof(*stubs_before));
    if (stubs_before == NULL) goto done;
    for (int i = 0; i < object->n_sections; ++i) {
        const Section *section = &object->sections[i];
        if (section->start == NULL || section->size == 0) continue;
        if (section->size > CAPTURE_LIMIT) goto done;
        before[i] = malloc((size_t)section->size);
        if (before[i] == NULL) goto done;
        memcpy(before[i], section->start, (size_t)section->size);
#if LL_TARGET_AARCH64
        const size_t size = section->elf_info->stub_size;
        if (size > CAPTURE_LIMIT) goto done;
        if (size != 0) {
            stubs_before[i] = malloc(size);
            if (stubs_before[i] == NULL) goto done;
            memcpy(stubs_before[i], section->elf_info->stub_offset, size);
        }
#endif
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
    uintptr_t anchor = 0;
    for (int i = 0; i < object->n_sections; ++i) {
        const Section *section = &object->sections[i];
        if (section->start == NULL || section->size == 0) continue;
#if LL_TARGET_AARCH64
        if (section->kind == SECTIONKIND_OTHER) continue;
#endif
        anchor = section->exec_start;
        break;
    }
    if (anchor == 0) goto done;
    if (!far_function) state.function_address = anchor + 0x1000;
#if LL_TARGET_AARCH64
    const uintptr_t data_page = anchor & ~(uintptr_t)0xfff;
    if (negative_data && data_page < 0x1000) goto done;
    state.data_address = negative_data ? data_page - 0x1000 + 0xf0
                                       : data_page + 0x20f0;
    if (!ghciInsertSymbolTable(&owner, WSTR("elf_capture"), "external_data",
            (void *)state.data_address, LL_STRENGTH_STRONG,
            LL_SYM_DATA, 0, NULL)) goto done;
#endif
    static const char *const names[] = {"external_function", "external_function_alias",
                                        "external_second_function"};
    for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); ++i) {
        if (!ghciInsertSymbolTable(&owner, WSTR("elf_capture"), names[i],
                                  (void *)(state.function_address + (i == 2 ? 0x1000 : 0)),
                                  LL_STRENGTH_STRONG, LL_SYM_CODE, 0, NULL))
            goto done;
    }
    /* This unregistered object reaches the real backend relocation operation.
     * It never enters the initializer/finalizer pass or executes loaded code. */
    const bool resolved = object->format_ops->resolve(object) != 0;
    emit_capture(object, &state, before, extras_before, extras_size, stubs_before, resolved);
    result = fflush(stdout) != 0 || ferror(stdout) ? 2 : (resolved ? 0 : 1);
done:
    if (result == 2)
        (void)fprintf(stderr, "elf_capture: setup failed: %s\n", owner.error_message);
    if (before != NULL) {
        for (int i = 0; i < object->n_sections; ++i) free(before[i]);
    }
    if (stubs_before != NULL) {
        for (int i = 0; i < object->n_sections; ++i) free(stubs_before[i]);
    }
    free(stubs_before);
    free(before);
    free(extras_before);
    const ll_status prior_status = owner.error_status;
    const bool prior_pre_backend = owner.error_from_pre_backend;
    char prior_message[LL_ERROR_CAPACITY];
    memcpy(prior_message, owner.error_message, sizeof(prior_message));
    ll_discard_object(object);
    const bool error_preserved = owner.error_status == prior_status &&
        owner.error_from_pre_backend == prior_pre_backend &&
        memcmp(owner.error_message, prior_message, sizeof(prior_message)) == 0;
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    if (!error_preserved) {
        (void)fputs("elf_capture: object cleanup changed the primary error\n", stderr);
        return 2;
    }
    if (state.stub_count != state.stub_releases) {
        (void)fprintf(stderr, "elf_capture: %zu stub metadata records retained\n",
                      state.stub_count - state.stub_releases);
        return 2;
    }
    return result;
}
