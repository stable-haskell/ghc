#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/PEi386.h"
#include "linker/M32Reservations.h"
#include "linker/MMap.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

_Static_assert(sizeof(IMAGE_SYMBOL) == 18, "COFF symbol record must be 18 bytes");
_Static_assert(sizeof(IMAGE_RELOCATION) == 10, "COFF relocation record must be 10 bytes");
_Static_assert((80 + 2 * sizeof(IMAGE_SYMBOL)) % sizeof(DWORD) == 0,
               "COFF string table must be DWORD aligned");

#if !defined(mingw32_HOST_OS) || !defined(x86_64_HOST_ARCH)
#error "pei386_common_ownership requires native Windows x86_64"
#endif

enum { HOST_ALLOCATION_CAPACITY = 2048 };

typedef struct {
    void *allocations[HOST_ALLOCATION_CAPACITY];
    const char *allocation_tags[HOST_ALLOCATION_CAPACITY];
    size_t allocation_count;
    void *common_bss;
    unsigned common_bss_host_frees;
    unsigned fatal_calls;
} Fixture;

static ll_linker owner;
static Fixture fixture;
static unsigned failures;

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "pei386_common_ownership:%d: %s\n", \
                      __LINE__, message); \
        ++failures; \
    } \
} while (0)

/* Source-included implementations make the fixture use the current PE loader,
 * ObjectCode disposal, and the real Windows M32 reservation/mapping path. */
#ifdef PEI386_TEST_M32_ALLOC
#define m32_alloc_for PEI386_TEST_M32_ALLOC
#endif
#ifdef PEI386_TEST_GETPROC
#define GetProcAddress PEI386_TEST_GETPROC
#endif
#ifdef PEI386_TEST_RTLADD
#define RtlAddFunctionTable PEI386_TEST_RTLADD
#endif
#ifdef PEI386_TEST_RTLDELETE
#define RtlDeleteFunctionTable PEI386_TEST_RTLDELETE
#endif
#include "linker/PEi386.c"
#include "linker/PEi386Host.c"
#ifdef PEI386_TEST_RTLDELETE
#undef RtlDeleteFunctionTable
#endif
#ifdef PEI386_TEST_RTLADD
#undef RtlAddFunctionTable
#endif
#ifdef PEI386_TEST_GETPROC
#undef GetProcAddress
#endif
#ifdef PEI386_TEST_M32_ALLOC
#undef m32_alloc_for
#endif
#include "ll_format.c"
#include "Linker.c"
#include "linker/M32Alloc.c"
#include "linker/M32Reservations.c"
#include "linker/MMap.c"

static void remember_allocation(void *pointer, const char *what)
{
    CHECK(pointer != NULL, "host allocator returned NULL");
    CHECK(fixture.allocation_count < HOST_ALLOCATION_CAPACITY,
          "host allocation ledger is full");
    if (pointer != NULL && fixture.allocation_count < HOST_ALLOCATION_CAPACITY)
    {
        fixture.allocations[fixture.allocation_count] = pointer;
        fixture.allocation_tags[fixture.allocation_count++] = what;
    }
}

static void *host_malloc(void *ctx, size_t bytes, const char *what)
{
    (void)ctx;
    (void)what;
    void *pointer = malloc(bytes == 0 ? 1 : bytes);
    remember_allocation(pointer, what);
    if (pointer == NULL) ExitProcess(98);
    return pointer;
}

static void *host_calloc(void *ctx, size_t count, size_t bytes,
                         const char *what)
{
    (void)ctx;
    void *pointer = calloc(count == 0 ? 1 : count, bytes == 0 ? 1 : bytes);
    remember_allocation(pointer, what);
    if (pointer == NULL) ExitProcess(98);
    return pointer;
}

static void *host_realloc(void *ctx, void *pointer, size_t bytes,
                          const char *what)
{
    (void)ctx;
    if (pointer == NULL) return host_malloc(ctx, bytes, what);
    for (size_t i = 0; i < fixture.allocation_count; ++i) {
        if (fixture.allocations[i] == pointer) {
            void *resized = realloc(pointer, bytes == 0 ? 1 : bytes);
            if (resized != NULL) fixture.allocations[i] = resized;
            if (resized == NULL) ExitProcess(98);
            return resized;
        }
    }
    CHECK(false, "realloc pointer is outside the host allocation ledger");
    ExitProcess(98);
    return NULL;
}

static void host_free(void *ctx, void *pointer)
{
    (void)ctx;
    if (pointer == NULL) return;
    if (pointer == fixture.common_bss) {
        ++fixture.common_bss_host_frees;
        (void)fprintf(stderr,
                      "pei386_common_ownership: PE COMMON M32 BSS passed to host_free\n");
        /* Deliberately do not call free() for an address outside this ledger. */
        return;
    }
    for (size_t i = 0; i < fixture.allocation_count; ++i) {
        if (fixture.allocations[i] == pointer) {
            fixture.allocations[i] = fixture.allocations[--fixture.allocation_count];
            fixture.allocation_tags[i] = fixture.allocation_tags[fixture.allocation_count];
            free(pointer);
            return;
        }
    }
    CHECK(false, "host_free pointer is outside the host allocation ledger");
}

static void host_log(void *ctx, int kind, const char *format, va_list args)
{
    char message[1024];
    (void)ctx;
    (void)kind;
    int written = vsnprintf(message, sizeof(message), format, args);
    CHECK(written >= 0 && (size_t)written < sizeof(message),
          "linker log formatting failed or truncated");
}

static void host_fatal(void *ctx, const char *format, va_list args)
{
    char message[1024];
    (void)ctx;
    (void)vsnprintf(message, sizeof(message), format, args);
    ++fixture.fatal_calls;
    (void)fprintf(stderr, "unexpected linker fatal: %s\n", message);
    ExitProcess(97);
}

static size_t host_page_size(void *ctx)
{
    SYSTEM_INFO info;
    (void)ctx;
    GetSystemInfo(&info);
    return (size_t)info.dwPageSize;
}

static void host_memory_failure(void *ctx)
{
    (void)ctx;
}

static void host_register_object(void *ctx, struct _ObjectCode *registered)
{
    ll_linker *linker = ctx;
    ObjectCode *oc = registered;
    oc->prev = NULL;
    oc->next = linker->gc.objects;
    if (oc->next != NULL) oc->next->prev = oc;
    linker->gc.objects = oc;
}

static void install_owner(void)
{
    ll_compat_host_ops ops = {0};
    ll_compat_config config = {0};
    ops.ctx = &owner;
    ops.malloc_ = host_malloc;
    ops.calloc_ = host_calloc;
    ops.realloc_ = host_realloc;
    ops.free_ = host_free;
    ops.log = host_log;
    ops.fatal = host_fatal;
    ops.on_mem_failure = host_memory_failure;
    ops.page_size = host_page_size;
    ops.object_registered = host_register_object;
    config.near_hint = GetModuleHandleW(NULL);
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
}

static bool write_object_file(pathchar *path, uint32_t common_size,
                              bool with_common, const char *common_name)
{
    enum {
        FILE_HEADER_OFFSET = 0,
        SECTION_HEADER_OFFSET = sizeof(IMAGE_FILE_HEADER),
        RAW_DATA_OFFSET = sizeof(IMAGE_FILE_HEADER) + sizeof(IMAGE_SECTION_HEADER),
        RELOCATION_OFFSET = RAW_DATA_OFFSET + sizeof(uint64_t),
        SYMBOL_TABLE_OFFSET = (RELOCATION_OFFSET + sizeof(IMAGE_RELOCATION) + 3) & ~3,
        COFF_SYMBOL_COUNT = 2,
        STRING_TABLE_OFFSET = SYMBOL_TABLE_OFFSET
            + COFF_SYMBOL_COUNT * sizeof(IMAGE_SYMBOL),
        COMMON_OBJECT_SIZE = STRING_TABLE_OFFSET + sizeof(uint32_t),
        EMPTY_OBJECT_SIZE = sizeof(IMAGE_FILE_HEADER) + sizeof(uint32_t)
    };
    size_t image_size = with_common ? COMMON_OBJECT_SIZE : EMPTY_OBJECT_SIZE;
    if (common_name == NULL ||
        strlen(common_name) > sizeof(((IMAGE_SYMBOL *)0)->N.ShortName))
        return false;
    uint8_t *image = calloc(1, image_size);
    if (image == NULL) return false;

    IMAGE_FILE_HEADER header = {0};
    header.Machine = IMAGE_FILE_MACHINE_AMD64;
    header.NumberOfSections = with_common ? 1 : 0;
    header.PointerToSymbolTable = (DWORD)(with_common
        ? SYMBOL_TABLE_OFFSET : sizeof(IMAGE_FILE_HEADER));
    header.NumberOfSymbols = with_common ? COFF_SYMBOL_COUNT : 0;
    memcpy(image + FILE_HEADER_OFFSET, &header, sizeof(header));

    if (with_common) {
        IMAGE_SECTION_HEADER section = {0};
        memcpy(section.Name, ".data", sizeof(".data") - 1);
        section.SizeOfRawData = sizeof(uint64_t);
        section.PointerToRawData = RAW_DATA_OFFSET;
        section.PointerToRelocations = RELOCATION_OFFSET;
        section.NumberOfRelocations = 1;
        section.Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA
            | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE
            | IMAGE_SCN_ALIGN_8BYTES;
        memcpy(image + SECTION_HEADER_OFFSET, &section, sizeof(section));

        const uint64_t addend = 4;
        memcpy(image + RAW_DATA_OFFSET, &addend, sizeof(addend));
        IMAGE_RELOCATION relocation = {0};
        relocation.VirtualAddress = 0;
        relocation.SymbolTableIndex = 0;
        relocation.Type = IMAGE_REL_AMD64_ADDR64;
        memcpy(image + RELOCATION_OFFSET, &relocation, sizeof(relocation));

        IMAGE_SYMBOL symbol = {0};
        memcpy(symbol.N.ShortName, common_name, strlen(common_name));
        symbol.Value = common_size;
        symbol.SectionNumber = IMAGE_SYM_UNDEFINED;
        symbol.Type = 0;
        symbol.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
        symbol.NumberOfAuxSymbols = 0;
        memcpy(image + SYMBOL_TABLE_OFFSET, &symbol, sizeof(symbol));

        IMAGE_SYMBOL metadata = {0};
        memcpy(metadata.N.ShortName, "absmeta", 7);
        metadata.SectionNumber = IMAGE_SYM_ABSOLUTE;
        metadata.StorageClass = IMAGE_SYM_CLASS_STATIC;
        memcpy(image + SYMBOL_TABLE_OFFSET + sizeof(IMAGE_SYMBOL),
               &metadata, sizeof(metadata));
    }
    const DWORD string_table_size = sizeof(DWORD);
    memcpy(image + (with_common ? STRING_TABLE_OFFSET
                                : sizeof(IMAGE_FILE_HEADER)),
           &string_table_size, sizeof(string_table_size));

    FILE *file = _wfopen(path, L"wb");
    bool written = file != NULL && fwrite(image, 1, image_size, file) == image_size;
    if (file != NULL && fclose(file) != 0) written = false;
    free(image);
    return written;
}

static bool make_temp_path(pathchar *path, size_t capacity)
{
    wchar_t directory[MAX_PATH];
    DWORD length = GetTempPathW(MAX_PATH, directory);
    if (length == 0 || length >= MAX_PATH || capacity < MAX_PATH) return false;
    return GetTempFileNameW(directory, L"pco", 0, path) != 0;
}

static RtsSymbolInfo *lookup_common(void)
{
    return lookupStrHashTable(owner.symhash, (ll_symbol_name *)"common1");
}

static bool load_and_resolve(pathchar *path, uint32_t size, bool with_common)
{
    if (!make_temp_path(path, MAX_PATH)) {
        CHECK(false, "could not create temporary COFF path");
        return false;
    }
    if (!write_object_file(path, size, with_common, "common1")) {
        CHECK(false, "could not write temporary COFF object");
        (void)DeleteFileW(path);
        return false;
    }
    ll_error_clear(&owner);
    int loaded = loadObj_(&owner, path);
    CHECK(loaded != 0, "actual loadObj_ rejected the synthetic COFF object");
    if (!loaded || owner.error_status != LL_OK) {
        (void)fprintf(stderr, "load error %d: %s\n", owner.error_status,
                      owner.error_message);
        (void)DeleteFileW(path);
        return false;
    }
    ll_error_clear(&owner);
    int resolved = resolveObjs_(&owner);
    CHECK(resolved != 0,
          "actual resolveObjs_ failed for the synthetic COFF object");
    if (!resolved || owner.error_status != LL_OK) {
        (void)fprintf(stderr, "resolve error %d: %s\n", owner.error_status,
                      owner.error_message);
        (void)DeleteFileW(path);
        return false;
    }
    return true;
}

static bool take_object_for_free(pathchar *path)
{
    ObjectCode *oc = lookupObjectByPath(&owner, path);
    if (oc == NULL) {
        CHECK(false, "registered ObjectCode was missing by path");
        return false;
    }
    CHECK(unloadObj_(&owner, path, false) != 0,
          "actual unloadObj_ did not unload the fixture object");

    ObjectCode **link = &owner.gc.objects;
    while (*link != NULL && *link != oc) link = &(*link)->next;
    CHECK(*link == oc, "unloaded ObjectCode was missing from GC list");
    if (*link != oc) return false;
    *link = oc->next;
    if (oc->next != NULL) oc->next->prev = oc->prev;
    oc->next = NULL;
    oc->prev = NULL;
    /* This mirrors the RTS CheckUnload disposal step after unloadObj_. */
    freeObjectCode(oc);
    if (owner.gc.n_unloaded_objects != 0) --owner.gc.n_unloaded_objects;
    return true;
}

static bool run_common_lifetime_case(void)
{
    pathchar provider_path[MAX_PATH];
    pathchar reuse_path[MAX_PATH];
    pathchar empty_path[MAX_PATH];

    if (!load_and_resolve(provider_path, 64, true)) return false;
    ObjectCode *provider = lookupObjectByPath(&owner, provider_path);
    RtsSymbolInfo *common = lookup_common();
    CHECK(provider != NULL && provider->status == OBJECT_READY,
          "provider did not reach OBJECT_READY through resolveObjs_");
    CHECK(common != NULL && common->owner == provider,
          "provider COMMON definition was not registered");
    CHECK(common != NULL && common->value != NULL && common->size == 64,
          "provider COMMON address or size was wrong");
    if (provider == NULL || common == NULL || common->value == NULL) return false;
    fixture.common_bss = common->value;
    CHECK(provider->sections[provider->n_sections - 1].alloc == SECTION_M32,
          "global COMMON section was not classified as M32-owned");
    CHECK(provider->sections[provider->n_sections - 1].start == common->value,
          "global COMMON section did not own the exported storage");
    for (size_t i = 0; i < 64; ++i)
        CHECK(((uint8_t *)common->value)[i] == 0,
              "new global COMMON storage was not zero-filled");
    uint64_t provider_relocation = 0;
    memcpy(&provider_relocation, provider->sections[0].start,
           sizeof(provider_relocation));
    CHECK(provider_relocation == (uint64_t)(uintptr_t)common->value + 4,
          "provider ADDR64 relocation did not point into COMMON storage");

    memset(common->value, 0xa6, 64);
    if (!load_and_resolve(reuse_path, 32, true)) return false;
    ObjectCode *reuse = lookupObjectByPath(&owner, reuse_path);
    CHECK(reuse != NULL && reuse->status == OBJECT_READY,
          "smaller COMMON consumer did not reach OBJECT_READY");
    CHECK(reuse != NULL && reuse->sections[reuse->n_sections - 1].start == NULL
              && reuse->sections[reuse->n_sections - 1].alloc == SECTION_NOMEM,
          "reused COMMON consumer allocated replacement BSS storage");
    uint64_t reuse_relocation = 0;
    if (reuse != NULL)
        memcpy(&reuse_relocation, reuse->sections[0].start,
               sizeof(reuse_relocation));
    CHECK(reuse_relocation == (uint64_t)(uintptr_t)common->value + 4,
          "consumer ADDR64 relocation did not reuse the provider address");

    if (!load_and_resolve(empty_path, 0, false)) return false;
    ObjectCode *empty = lookupObjectByPath(&owner, empty_path);
    CHECK(empty != NULL && empty->status == OBJECT_READY,
          "empty COFF object did not reach OBJECT_READY");
    CHECK(empty != NULL && empty->sections[0].start == NULL
              && empty->sections[0].alloc == SECTION_NOMEM,
          "empty object unexpectedly acquired a BSS allocation");

    if (empty != NULL) {
        CHECK(take_object_for_free(empty_path), "empty object disposal failed");
        (void)DeleteFileW(empty_path);
    }
    if (reuse != NULL) {
        CHECK(take_object_for_free(reuse_path), "reused COMMON disposal failed");
        (void)DeleteFileW(reuse_path);
    }
    CHECK(fixture.common_bss_host_frees == 0,
          "COMMON M32 BSS reached host_free during consumer disposal");
    for (size_t i = 0; i < 64; ++i)
        CHECK(((uint8_t *)common->value)[i] == 0xa6,
              "disposing a reuse/empty object changed provider COMMON bytes");

    if (provider != NULL) {
        CHECK(take_object_for_free(provider_path), "provider disposal failed");
        (void)DeleteFileW(provider_path);
    }
    CHECK(fixture.common_bss_host_frees == 0,
          "freeObjectCode sent M32 COMMON storage to host_free");
    return true;
}

static void release_test_linker_auxiliary_state(void)
{
    freeHashTable(owner.loaded_dll_cache.hash, NULL);
    owner.loaded_dll_cache.hash = NULL;
    while (owner.opened_dlls != NULL) {
        OpenedDLL *opened = owner.opened_dlls;
        owner.opened_dlls = opened->next;
        LL_FREE_FOR(&owner, opened->name);
        LL_FREE_FOR(&owner, opened);
    }
}

static bool is_expected_metadata_residue(const char *tag)
{
    return strcmp(tag, "ocVerifyImage_PEi386(oc->info->str_tab)") == 0
        || strcmp(tag, "cstring_from_section_symbol_name") == 0
        || strcmp(tag, "ocVerifyImage_PEi386(section->info->relocs)") == 0;
}

static void check_and_release_known_metadata_residue(void)
{
    size_t str_tabs = 0, section_names = 0, relocation_copies = 0;
    for (size_t i = 0; i < fixture.allocation_count; ++i) {
        const char *tag = fixture.allocation_tags[i];
        if (!is_expected_metadata_residue(tag)) {
            CHECK(false, "normal freeObjectCode left unexpected host allocation live");
            continue;
        }
        if (strcmp(tag, "ocVerifyImage_PEi386(oc->info->str_tab)") == 0)
            ++str_tabs;
        else if (strcmp(tag, "cstring_from_section_symbol_name") == 0)
            ++section_names;
        else
            ++relocation_copies;
    }
    /* Known PE teardown gap: each object leaks its copied COFF string table;
     * each nonempty fixture additionally leaks the section name and relocation
     * copy. Observe these exact allocations after normal disposal, then release
     * them as harness-owned residue so the ledger can still detect surprises. */
    CHECK(str_tabs == 3, "unexpected copied COFF string-table residue count");
    CHECK(section_names == 2, "unexpected PE section-name residue count");
    CHECK(relocation_copies == 2, "unexpected PE relocation-copy residue count");
    (void)fprintf(stderr,
                  "known PE metadata residue after normal disposal: str_tab=%zu section_name=%zu relocs=%zu (harness cleanup follows)\n",
                  str_tabs, section_names, relocation_copies);
    while (fixture.allocation_count != 0) {
        size_t i = --fixture.allocation_count;
        free(fixture.allocations[i]);
        fixture.allocations[i] = NULL;
        fixture.allocation_tags[i] = NULL;
    }
}

int main(void)
{
    install_owner();
    (void)run_common_lifetime_case();
    CHECK(fixture.fatal_calls == 0, "linker fatal callback was called");
    CHECK(fixture.common_bss_host_frees == 0,
          "a COMMON M32 address was passed to host_free");
    ghciRemoveSymbolTable(&owner, (ll_symbol_name *)"__image_base__", NULL);
    ll_exit_linker(&owner);
    CHECK(owner.m32_free_page_pool == NULL &&
              owner.m32_free_page_pool_size == 0,
          "linker exit did not release the M32 free-page pool");
    CHECK(owner.m32_reservations == NULL,
          "linker exit retained an M32 OS reservation");
    release_test_linker_auxiliary_state();
    check_and_release_known_metadata_residue();
    CHECK(fixture.allocation_count == 0,
          "harness cleanup left host allocations live");
    if (failures != 0) return 1;
    (void)puts("pei386_common_ownership: PASS");
    return 0;
}
