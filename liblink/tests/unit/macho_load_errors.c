#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/MachO.h"
#include "linker/MMap.h"

#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#if !defined(OBJFORMAT_MACHO) || !(defined(aarch64_HOST_ARCH) || \
                                   defined(arm64_HOST_ARCH) || \
                                   defined(x86_64_HOST_ARCH))
#error "macho_load_errors requires a native supported Mach-O target"
#endif

enum {
    HOST_CAPACITY = 2048,
    MAP_CAPACITY = 64,
    IMAGE_CAPACITY = 4096,
    PAYLOAD_OFFSET = 2048,
    PAYLOAD_SIZE = 32
};

typedef struct {
    void *owned[HOST_CAPACITY];
    const char *what[HOST_CAPACITY];
    size_t live;
    size_t allocs;
    size_t frees;
    unsigned logs;
    unsigned fatals;
    const char *fail_tag;
    unsigned fail_ordinal;
    unsigned matching_callocs;
    unsigned injected_failures;
} Host;

typedef struct {
    void *base;
    size_t size;
    bool active;
    bool section_map;
} Mapping;

typedef struct {
    size_t allocs;
    size_t frees;
    size_t live;
    StrHashTable *symhash;
    ObjectCode *objects;
    ObjectCode *loaded;
    size_t active_maps;
} Snapshot;

typedef enum {
    SYMBOLS_NONE,
    SYMBOLS_ZERO,
    SYMBOLS_EXPORT,
    SYMBOLS_COMMON
} SymbolMode;

typedef enum {
    PROFILE_EMPTY,
    PROFILE_REGULAR,
    PROFILE_CLASSES
} SectionProfile;

typedef struct {
    SectionProfile sections;
    SymbolMode symbols;
    uint32_t common_size;
} FixtureSpec;

typedef enum {
    MAP_FAIL_NONE,
    MAP_FAIL_SEGMENTS,
    MAP_FAIL_SECTION
} MapFailure;

static Host owner_host;
static Host default_host;
static ll_linker owner;
static Mapping mappings[MAP_CAPACITY];
static size_t page_size;
static unsigned map_attempts;
static unsigned unmap_calls;
static MapFailure map_failure;
static unsigned failures;
static bool stop_after_failed_reinit;
static unsigned names_after_failed_reinit;
static const char default_sentinel[] = "macho-load-default-error-sentinel";

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "macho_load_errors:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void *test_mmap_anon_for_linker(ll_linker *L, size_t bytes);
static void *test_mmap_for_linker(ll_linker *L, size_t bytes,
                                  MemoryAccess access, uint32_t flags,
                                  int fd, int offset);
static void test_munmap_for_linker(ll_linker *L, void *addr, size_t bytes,
                                   const char *caller);

#define ll_mmap_anon_for_linker test_mmap_anon_for_linker
#define ll_mmap_for_linker test_mmap_for_linker
#define ll_munmap_for_linker test_munmap_for_linker
#define ocGetNames_MachO actual_ocGetNames_MachO
#include "linker/MachO.c"
#undef ocGetNames_MachO
#include "linker/SymbolExtras.c"
#include "Linker.c"
#undef ll_mmap_anon_for_linker
#undef ll_mmap_for_linker
#undef ll_munmap_for_linker

int ocGetNames_MachO(ObjectCode *oc)
{
    if (stop_after_failed_reinit &&
        (oc->info == NULL ||
         (oc->info->n_macho_symbols > 0 && oc->info->macho_symbols == NULL))) {
        ++names_after_failed_reinit;
        return 0;
    }
    return actual_ocGetNames_MachO(oc);
}

static size_t active_mappings(void)
{
    size_t count = 0;
    for (size_t i = 0; i < MAP_CAPACITY; ++i)
        if (mappings[i].active) ++count;
    return count;
}

static bool mapping_contains(const void *ptr)
{
    uintptr_t address = (uintptr_t)ptr;
    for (size_t i = 0; i < MAP_CAPACITY; ++i) {
        uintptr_t base = (uintptr_t)mappings[i].base;
        if (mappings[i].active && address >= base &&
            address - base < mappings[i].size)
            return true;
    }
    return false;
}

static void *map_real(size_t bytes, bool section_map)
{
    if (page_size == 0 || bytes > SIZE_MAX - (page_size - 1)) return NULL;
    size_t rounded = ((bytes + page_size - 1) / page_size) * page_size;
    if (rounded == 0) rounded = page_size;
    void *base = mmap(NULL, rounded, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANON, -1, 0);
    if (base == MAP_FAILED) return NULL;
    for (size_t i = 0; i < MAP_CAPACITY; ++i) {
        if (!mappings[i].active) {
            mappings[i] = (Mapping){base, rounded, true, section_map};
            return base;
        }
    }
    CHECK(false, "mapping ledger is full");
    (void)munmap(base, rounded);
    return NULL;
}

static void *test_mmap_anon_for_linker(ll_linker *L, size_t bytes)
{
    (void)L;
    ++map_attempts;
    if (map_failure == MAP_FAIL_SEGMENTS) {
        map_failure = MAP_FAIL_NONE;
        return NULL;
    }
    return map_real(bytes, false);
}

static void *test_mmap_for_linker(ll_linker *L, size_t bytes,
                                  MemoryAccess access, uint32_t flags,
                                  int fd, int offset)
{
    (void)L;
    (void)access;
    (void)flags;
    (void)fd;
    (void)offset;
    ++map_attempts;
    if (map_failure == MAP_FAIL_SECTION) {
        map_failure = MAP_FAIL_NONE;
        return NULL;
    }
    return map_real(bytes, true);
}

static void test_munmap_for_linker(ll_linker *L, void *addr, size_t bytes,
                                   const char *caller)
{
    (void)L;
    (void)caller;
    if (page_size == 0 || bytes > SIZE_MAX - (page_size - 1)) {
        CHECK(false, "unmap range cannot be rounded to a page");
        return;
    }
    size_t unmapped_bytes = ((bytes + page_size - 1) / page_size) * page_size;
    for (size_t i = 0; i < MAP_CAPACITY; ++i) {
        uintptr_t base = (uintptr_t)mappings[i].base;
        uintptr_t start = (uintptr_t)addr;
        if (mappings[i].active && start >= base && unmapped_bytes <= mappings[i].size
            && start - base <= mappings[i].size - unmapped_bytes) {
            if (munmap(addr, unmapped_bytes) != 0)
                CHECK(false, "munmap failed for a tracked mapping");
            size_t prefix = (size_t)(start - base);
            size_t suffix = mappings[i].size - prefix - unmapped_bytes;
            if (prefix == 0 && suffix == 0) {
                mappings[i].active = false;
            } else if (prefix == 0) {
                mappings[i].base = (void *)(start + unmapped_bytes);
                mappings[i].size = suffix;
            } else if (suffix == 0) {
                mappings[i].size = prefix;
            } else {
                bool recorded = false;
                for (size_t j = 0; j < MAP_CAPACITY; ++j) {
                    if (!mappings[j].active) {
                        mappings[j] = (Mapping){(void *)(start + unmapped_bytes), suffix,
                                                true, mappings[i].section_map};
                        mappings[i].size = prefix;
                        recorded = true;
                        break;
                    }
                }
                CHECK(recorded, "mapping ledger cannot represent split unmap");
            }
            ++unmap_calls;
            return;
        }
    }
    (void)fprintf(stderr, "unmap miss: %p + %zu (rounded %zu) caller=%s\n",
                  addr, bytes, unmapped_bytes, caller != NULL ? caller : "?");
    for (size_t i = 0; i < MAP_CAPACITY; ++i)
        if (mappings[i].active)
            (void)fprintf(stderr, "  tracked: %p + %zu section=%d\n",
                          mappings[i].base, mappings[i].size,
                          mappings[i].section_map ? 1 : 0);
    CHECK(false, "unmap did not match a live mapping");
}

static void remember(Host *host, void *ptr, const char *what)
{
    CHECK(ptr != NULL, "host allocation returned NULL unexpectedly");
    CHECK(host->live < HOST_CAPACITY, "owner allocation ledger overflow");
    if (ptr != NULL && host->live < HOST_CAPACITY) {
        host->owned[host->live] = ptr;
        host->what[host->live++] = what;
        ++host->allocs;
    }
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    Host *host = ctx;
    void *ptr = malloc(size == 0 ? 1 : size);
    remember(host, ptr, what);
    return ptr;
}

static void *host_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    Host *host = ctx;
    if (host->fail_tag != NULL && what != NULL
        && strcmp(host->fail_tag, what) == 0) {
        ++host->matching_callocs;
        if (host->matching_callocs == host->fail_ordinal) {
            ++host->injected_failures;
            return NULL;
        }
    }
    void *ptr = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    remember(host, ptr, what);
    return ptr;
}

static void *host_realloc(void *ctx, void *ptr, size_t size, const char *what)
{
    Host *host = ctx;
    if (ptr == NULL) return host_malloc(ctx, size, what);
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == ptr) {
            void *next = realloc(ptr, size == 0 ? 1 : size);
            if (next != NULL) host->owned[i] = next;
            return next;
        }
    }
    CHECK(false, "realloc pointer was not owned by this host");
    return NULL;
}

static void host_free(void *ctx, void *ptr)
{
    Host *host = ctx;
    if (ptr == NULL) return;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == ptr) {
            host->owned[i] = host->owned[--host->live];
            host->what[i] = host->what[host->live];
            ++host->frees;
            free(ptr);
            return;
        }
    }
    CHECK(false, "free pointer was not owned by this host");
}

static void host_log(void *ctx, int kind, const char *fmt, va_list ap)
{
    Host *host = ctx;
    char message[1024];
    int n = vsnprintf(message, sizeof(message), fmt, ap);
    CHECK(n >= 0 && (size_t)n < sizeof(message), "host log was truncated");
    (void)kind;
    ++host->logs;
}

static _Noreturn void host_fatal(void *ctx, const char *fmt, va_list ap)
{
    Host *host = ctx;
    char message[1024];
    (void)vsnprintf(message, sizeof(message), fmt, ap);
    ++host->fatals;
    (void)fprintf(stderr, "unexpected linker fatal: %s\n", message);
    _Exit(98);
}

static size_t host_page_size(void *ctx)
{
    (void)ctx;
    return page_size;
}

static void host_mem_failure(void *ctx) { (void)ctx; }

static ll_compat_host_ops make_ops(Host *host)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = host;
    ops.malloc_ = host_malloc;
    ops.calloc_ = host_calloc;
    ops.realloc_ = host_realloc;
    ops.free_ = host_free;
    ops.log = host_log;
    ops.fatal = host_fatal;
    ops.page_size = host_page_size;
    ops.on_mem_failure = host_mem_failure;
    return ops;
}

static Snapshot snapshot(void)
{
    return (Snapshot){owner_host.allocs, owner_host.frees, owner_host.live,
                      owner.symhash, owner.gc.objects, owner.gc.loaded_objects,
                      active_mappings()};
}

static void check_rollback(Snapshot before, const char *label)
{
    Snapshot after = snapshot();
    if (after.allocs - before.allocs != after.frees - before.frees ||
        after.live != before.live) {
        (void)fprintf(stderr, "macho_load_errors: %s allocation delta alloc=%zu free=%zu live=%zu->%zu\n",
                      label, after.allocs - before.allocs,
                      after.frees - before.frees, before.live, after.live);
        for (size_t i = 0; i < owner_host.live; ++i)
            (void)fprintf(stderr, "  live allocation: %s %p\n",
                          owner_host.what[i], owner_host.owned[i]);
        CHECK(false, "owner allocations were not fully rolled back");
    }
    if (after.active_maps != before.active_maps) {
        (void)fprintf(stderr, "macho_load_errors: %s mapping count %zu->%zu\n",
                      label, before.active_maps, after.active_maps);
        for (size_t i = 0; i < MAP_CAPACITY; ++i)
            if (mappings[i].active)
                (void)fprintf(stderr, "  live mapping: %p + %zu section=%d\n",
                              mappings[i].base, mappings[i].size,
                              mappings[i].section_map ? 1 : 0);
    }
    CHECK(after.symhash == before.symhash && after.objects == before.objects
              && after.loaded == before.loaded, label);
    CHECK(after.active_maps == before.active_maps, label);
}

static void set_name(char dst[16], const char *src)
{
    size_t n = strlen(src);
    if (n > 16) n = 16;
    memset(dst, 0, 16);
    memcpy(dst, src, n);
}

static unsigned char *make_image(FixtureSpec spec)
{
    unsigned char *image = map_real(IMAGE_CAPACITY, false);
    if (image == NULL) return NULL;
    memset(image, 0, IMAGE_CAPACITY);
    MachOHeader *header = (MachOHeader *)image;
    MachOSegmentCommand *segment = (MachOSegmentCommand *)(header + 1);
    uint32_t nsections = spec.sections == PROFILE_EMPTY ? 0
                       : spec.sections == PROFILE_CLASSES ? 3 : 1;
    uint32_t nsymbols = spec.symbols == SYMBOLS_EXPORT ||
                        spec.symbols == SYMBOLS_COMMON ? 1 : 0;
    size_t segment_command_size = sizeof(*segment)
                                + nsections * sizeof(MachOSection);
    size_t sym_command_size = spec.symbols == SYMBOLS_NONE ? 0
                            : sizeof(MachOSymtabCommand);
    header->magic = MH_MAGIC_64;
    header->filetype = MH_OBJECT;
    header->ncmds = sym_command_size == 0 ? 1 : 2;
    header->sizeofcmds = (uint32_t)(segment_command_size + sym_command_size);

    segment->cmd = LC_SEGMENT_64;
    segment->cmdsize = (uint32_t)segment_command_size;
    segment->nsects = nsections;
    MachOSection *sections = (MachOSection *)(segment + 1);
    for (uint32_t i = 0; i < nsections; ++i) {
        const char *segname = "__DATA";
        const char *sectname = "__data";
        uint32_t type = S_REGULAR;
        if (spec.sections == PROFILE_CLASSES && i == 0) {
            segname = "__TEXT";
            sectname = "__text";
        } else if (spec.sections == PROFILE_CLASSES && i == 2) {
            sectname = "__zerofill";
            type = S_GB_ZEROFILL;
        }
        set_name(sections[i].segname, segname);
        set_name(sections[i].sectname, sectname);
        sections[i].addr = UINT64_C(0x1000) + (uint64_t)i * UINT64_C(0x100);
        sections[i].size = PAYLOAD_SIZE;
        sections[i].offset = PAYLOAD_OFFSET + i * 64;
        sections[i].align = 3;
        sections[i].flags = type;
        if (type != S_GB_ZEROFILL)
            memset(image + sections[i].offset, (int)(0x40 + i), PAYLOAD_SIZE);
    }

    if (spec.symbols != SYMBOLS_NONE) {
        MachOSymtabCommand *symcmd = (MachOSymtabCommand *)
            ((unsigned char *)segment + segment_command_size);
        symcmd->cmd = LC_SYMTAB;
        symcmd->cmdsize = sizeof(*symcmd);
        symcmd->nsyms = nsymbols;
        symcmd->symoff = (uint32_t)(sizeof(*header) + header->sizeofcmds);
        symcmd->stroff = symcmd->symoff + nsymbols * sizeof(MachONList);
        symcmd->strsize = 1;
        if (nsymbols != 0) {
            symcmd->strsize = (uint32_t)sizeof("\0macho_load_symbol\0");
            MachONList *symbol = (MachONList *)(image + symcmd->symoff);
            symbol->n_un.n_strx = 1;
            if (spec.symbols == SYMBOLS_EXPORT) {
                symbol->n_type = N_SECT | N_EXT;
                symbol->n_sect = 1;
                symbol->n_value = sections[0].addr;
            } else {
                symbol->n_type = N_UNDF | N_EXT;
                symbol->n_value = spec.common_size != 0 ? spec.common_size : 64;
            }
            memcpy(image + symcmd->stroff, "\0macho_load_symbol\0",
                   sizeof("\0macho_load_symbol\0"));
        }
    }
    return image;
}

static ObjectCode *make_object(FixtureSpec spec, const char *name)
{
    unsigned char *image = make_image(spec);
    CHECK(image != NULL, "fixture image allocation failed");
    if (image == NULL) return NULL;
    ObjectCode *oc = mkOc(&owner, STATIC_OBJECT, (pathchar *)name,
                          (char *)image, IMAGE_CAPACITY, true, NULL, 0);
    CHECK(oc != NULL, "mkOc failed");
    if (oc == NULL) test_munmap_for_linker(&owner, image, IMAGE_CAPACITY,
                                           "make_object failure");
    return oc;
}

static void arm_alloc_failure(const char *tag, unsigned ordinal)
{
    owner_host.fail_tag = tag;
    owner_host.fail_ordinal = ordinal;
    owner_host.matching_callocs = 0;
    owner_host.injected_failures = 0;
}

static void clear_alloc_failure(void)
{
    owner_host.fail_tag = NULL;
    owner_host.fail_ordinal = 0;
}

static void run_alloc_failure(const char *label, FixtureSpec spec,
                              const char *tag, unsigned ordinal,
                              bool fail_init)
{
    Snapshot before = snapshot();
    ObjectCode *oc = make_object(spec, label);
    if (oc == NULL) return;
    ll_error_clear(&owner);
    if (fail_init) arm_alloc_failure(tag, ordinal);
    int initialized = ll_init_object(oc);
    if (fail_init) {
        CHECK(initialized == 0, label);
        CHECK(owner.error_status == LL_E_NOMEM, label);
        CHECK(owner.error_message[0] != '\0', label);
    } else {
        CHECK(initialized != 0, label);
        arm_alloc_failure(tag, ordinal);
        int loaded = initialized ? loadOc(oc) : 1;
        CHECK(loaded == 0, label);
        CHECK(owner.error_status == LL_E_NOMEM, label);
        CHECK(owner.error_message[0] != '\0', label);
    }
    clear_alloc_failure();
    CHECK(owner_host.injected_failures == 1, "requested allocation was not failed once");
    CHECK(owner_host.matching_callocs >= ordinal,
          "allocation failure ordinal was not reached");
    CHECK(owner_host.fatals == 0, "allocation failure reached fatal callback");
    ll_discard_object(oc);
    check_rollback(before, label);
}

static void run_reinit_allocation_failure(const char *label, const char *tag,
                                          bool fail_symbol_array)
{
    Snapshot before = snapshot();
    unsigned attempts_before = map_attempts;
    ObjectCode *oc = make_object(
        (FixtureSpec){PROFILE_REGULAR, SYMBOLS_EXPORT, 0}, label);
    if (oc == NULL) return;

    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0,
          "initial Mach-O metadata initialization failed before injection");
    CHECK(oc->info != NULL &&
              (!fail_symbol_array ||
               (oc->info->n_macho_symbols == 1 && oc->info->macho_symbols != NULL)),
          "initial Mach-O metadata was not complete before injection");
    void *old_image = oc->image;
    CHECK(mapping_contains(old_image), "initial object image is not mapped");

    arm_alloc_failure(tag, 1);
    stop_after_failed_reinit = true;
    names_after_failed_reinit = 0;
    int loaded = loadOc(oc);
    stop_after_failed_reinit = false;
    clear_alloc_failure();

    CHECK(loaded == 0, "second Mach-O metadata initialization did not fail load");
    CHECK(owner.error_status == LL_E_NOMEM,
          "second Mach-O metadata failure did not retain NOMEM");
    CHECK(owner.error_message[0] != '\0',
          "second Mach-O metadata failure omitted diagnostic");
    CHECK(owner_host.injected_failures == 1 &&
              owner_host.matching_callocs == 1,
          "second metadata allocation failure was not injected exactly once");
    CHECK(names_after_failed_reinit == 0,
          "load continued to names after second metadata initialization failed");
    CHECK(map_attempts == attempts_before + 1,
          "image relocation did not make exactly one replacement mapping");
    CHECK(oc->image != old_image && mapping_contains(oc->image),
          "replacement image was not retained as a live owned mapping");
    CHECK(!mapping_contains(old_image),
          "old image mapping remained live after image relocation");
    if (fail_symbol_array) {
        CHECK(oc->info != NULL && oc->info->n_macho_symbols == 1 &&
                  oc->info->macho_symbols == NULL,
              "symbol-array failure did not leave the expected partial metadata");
    } else {
        CHECK(oc->info == NULL,
              "format-info failure did not leave the expected empty metadata state");
    }
    CHECK(oc->sections == NULL,
          "sections were published after reinitialization failed");
    CHECK(owner_host.fatals == 0,
          "second metadata initialization failure reached fatal callback");

    ll_discard_object(oc);
    check_rollback(before, label);
}

static void run_zero_symbol_control(void)
{
    Snapshot before = snapshot();
    ObjectCode *oc = make_object((FixtureSpec){PROFILE_EMPTY, SYMBOLS_ZERO, 0},
                                 "macho-zero-sections-zero-symbols");
    if (oc == NULL) return;
    ll_error_clear(&owner);
    arm_alloc_failure("ocInit_MachO(MachOSymbol)", 1);
    int initialized = ll_init_object(oc);
    CHECK(initialized != 0, "zero-symbol initialization failed");
    CHECK(owner_host.injected_failures == 0,
          "zero-symbol initialization attempted the symbol metadata allocation");
    clear_alloc_failure();
    int loaded = initialized ? loadOc(oc) : 0;
    CHECK(loaded != 0 && owner.error_status == LL_OK,
          "zero-section/zero-symbol load failed");
    CHECK(owner_host.fatals == 0, "zero-symbol control reached fatal callback");
    ll_discard_object(oc);
    check_rollback(before, "zero-symbol control leaked state");
}

static void run_successful_regular_control(void)
{
    Snapshot before = snapshot();
    ObjectCode *oc = make_object((FixtureSpec){PROFILE_REGULAR, SYMBOLS_NONE, 0},
                                 "macho-regular-section-positive");
    if (oc == NULL) return;
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0, "ordinary object initialization failed");
    CHECK(loadOc(oc) != 0 && owner.error_status == LL_OK,
          "ordinary Mach-O load failed");
    CHECK(oc->sections != NULL && oc->sections[0].start != NULL,
          "ordinary Mach-O section was not loaded");
    CHECK(owner_host.fatals == 0, "ordinary load reached fatal callback");
    ll_discard_object(oc);
    check_rollback(before, "ordinary successful load leaked state");
}

static void run_segment_map_failure(void)
{
    Snapshot before = snapshot();
    unsigned attempts_before = map_attempts;
    ObjectCode *oc = make_object((FixtureSpec){PROFILE_REGULAR, SYMBOLS_NONE, 0},
                                 "macho-segment-map-failure");
    if (oc == NULL) return;
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0, "segment map fixture initialization failed");
    map_failure = MAP_FAIL_SEGMENTS;
    CHECK(loadOc(oc) == 0 && owner.error_status == LL_E_NOMEM,
          "segment map failure did not propagate NOMEM");
    CHECK(owner.error_message[0] != '\0', "segment map failure omitted diagnostic");
    map_failure = MAP_FAIL_NONE;
    CHECK(map_attempts == attempts_before + 1,
          "segment mapper injection was not reached exactly once");
    CHECK(owner_host.fatals == 0, "segment map failure reached fatal callback");
    ll_discard_object(oc);
    check_rollback(before, "segment map failure leaked state");
}

static void run_section_map_failure(void)
{
#if defined(aarch64_HOST_ARCH) || defined(arm64_HOST_ARCH)
    Snapshot before = snapshot();
    unsigned attempts_before = map_attempts;
    ObjectCode *oc = make_object((FixtureSpec){PROFILE_REGULAR, SYMBOLS_NONE, 0},
                                 "macho-section-map-failure");
    if (oc == NULL) return;
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0, "section map fixture initialization failed");
    map_failure = MAP_FAIL_SECTION;
    CHECK(loadOc(oc) == 0 && owner.error_status == LL_E_NOMEM,
          "NEED_PLT section map failure did not propagate NOMEM");
    CHECK(owner.error_message[0] != '\0', "section map failure omitted diagnostic");
    map_failure = MAP_FAIL_NONE;
    CHECK(map_attempts == attempts_before + 2,
          "segment and section mapper calls were not observed");
    CHECK(oc->sections != NULL && oc->sections[0].start == NULL,
          "section was published after its mapping failed");
    CHECK(owner_host.fatals == 0, "section map failure reached fatal callback");
    ll_discard_object(oc);
    check_rollback(before, "section map failure leaked state");
#else
    (void)puts("macho_load_errors: SKIP NEED_PLT section-map case on non-AArch64 host");
#endif
}

static void run_segment_metadata_failures(void)
{
    FixtureSpec one = {PROFILE_REGULAR, SYMBOLS_NONE, 0};
    run_alloc_failure("macho-segment-descriptor-nomem", one,
                      "ocBuildSegments_MachO(segments)", 1, false);
    FixtureSpec classes = {PROFILE_CLASSES, SYMBOLS_NONE, 0};
    run_alloc_failure("macho-rx-index-nomem", classes,
                      "initSegment(segment)", 1, false);
    run_alloc_failure("macho-rw-index-nomem", classes,
                      "initSegment(segment)", 2, false);
    run_alloc_failure("macho-gb-index-nomem", classes,
                      "initSegment(segment)", 3, false);
}

static RtsSymbolInfo *lookup_common_symbol(void)
{
    return lookupStrHashTable(owner.symhash, "macho_load_symbol");
}

static void check_common_symbol(RtsSymbolInfo *expected, ObjectCode *first,
                                ll_symbol_addr *value,
                                const unsigned char *bytes,
                                const char *label)
{
    RtsSymbolInfo *actual = lookup_common_symbol();
    CHECK(actual == expected, label);
    if (actual != NULL && expected != NULL) {
        CHECK(actual->owner == first, label);
        CHECK(actual->value == value && value == first->info->common_mem, label);
        CHECK(actual->size == 64, label);
        CHECK(actual->strength == LL_STRENGTH_NORMAL, label);
        CHECK(actual->type == LL_SYM_CODE, label);
        if (actual->value != NULL)
            CHECK(memcmp(actual->value, bytes, 64) == 0, label);
        else
            CHECK(false, label);
    }
}

static void check_common_owner_teardown(Snapshot before, const char *label)
{
    Snapshot after = snapshot();
    size_t retained_hash_chunks = 0;
    for (size_t i = 0; i < owner_host.live; ++i)
        if (strcmp(owner_host.what[i], "allocHashList") == 0)
            ++retained_hash_chunks;
    CHECK(after.allocs - before.allocs == after.frees - before.frees + 1,
          label);
    CHECK(after.live == before.live + 1 && retained_hash_chunks == 1, label);
    CHECK(after.symhash == before.symhash && after.objects == before.objects
              && after.loaded == before.loaded, label);
    CHECK(after.active_maps == before.active_maps, label);
}

static void run_common_size_conflict(void)
{
    Snapshot before_first = snapshot();
    ObjectCode *first = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_COMMON, 64},
        "macho-common-owner");
    if (first == NULL) return;
    ll_error_clear(&owner);
    CHECK(ll_init_object(first) != 0, "first COMMON initialization failed");
    CHECK(loadOc(first) != 0 && owner.error_status == LL_OK,
          "first COMMON definition did not load");
    CHECK(owner_host.fatals == 0, "first COMMON load reached fatal callback");

    RtsSymbolInfo *original = lookup_common_symbol();
    CHECK(original != NULL, "first COMMON symbol was not registered");
    CHECK(original != NULL && original->owner == first,
          "first COMMON symbol has the wrong owner");
    CHECK(original != NULL && original->size == 64,
          "first COMMON symbol has the wrong size");
    if (original == NULL || original->value == NULL) {
        ll_discard_object(first);
        check_rollback(before_first,
                       "tearing down COMMON object without symbol leaked state");
        return;
    }
    ll_symbol_addr *original_value = original->value;
    unsigned char original_bytes[64];
    if (original != NULL && original->value != NULL) {
        memset(original_value, 0xa6, sizeof(original_bytes));
        memset(original_bytes, 0xa6, sizeof(original_bytes));
    } else {
        memset(original_bytes, 0, sizeof(original_bytes));
        CHECK(false, "first COMMON storage is missing");
    }
    Snapshot with_first = snapshot();

    ObjectCode *larger = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_COMMON, 128},
        "macho-common-larger");
    if (larger != NULL) {
        ll_error_clear(&owner);
        CHECK(ll_init_object(larger) != 0,
              "larger COMMON object initialization failed");
        int loaded = loadOc(larger);
        CHECK(loaded == 0 && owner.error_status == LL_E_DUPSYM,
              "larger COMMON definition did not return DUPSYM");
        CHECK(owner.error_message[0] != '\0',
              "larger COMMON definition omitted diagnostic");
        CHECK(owner_host.fatals == 0,
              "larger COMMON definition reached fatal callback");
        check_common_symbol(original, first, original_value, original_bytes,
                            "larger COMMON changed the original symbol");
        ll_discard_object(larger);
        check_rollback(with_first,
                       "discarding larger COMMON failure leaked state");
        check_common_symbol(original, first, original_value, original_bytes,
                            "discarding larger COMMON failure changed original");
    }

    for (uint32_t size = 64; size >= 32; size -= 32) {
        Snapshot before_reuse = snapshot();
        ObjectCode *reuse = make_object(
            (FixtureSpec){PROFILE_EMPTY, SYMBOLS_COMMON, size},
            size == 64 ? "macho-common-equal-reuse" : "macho-common-smaller-reuse");
        if (reuse == NULL) break;
        ll_error_clear(&owner);
        CHECK(ll_init_object(reuse) != 0,
              "reused COMMON object initialization failed");
        CHECK(loadOc(reuse) != 0 && owner.error_status == LL_OK,
              "equal/smaller COMMON definition did not reuse original");
        CHECK(reuse->info->common_mem == NULL,
              "reused COMMON definition allocated replacement storage");
        CHECK(reuse->info->macho_symbols[0].addr == original->value,
              "reused COMMON symbol does not use original address");
        check_common_symbol(original, first, original_value, original_bytes,
                            "equal/smaller COMMON changed the original symbol");
        ll_discard_object(reuse);
        check_rollback(before_reuse,
                       "discarding reused COMMON object leaked state");
        check_common_symbol(original, first, original_value, original_bytes,
                            "discarding reused COMMON object changed original");
    }

    ll_discard_object(first);
    CHECK(lookup_common_symbol() == NULL,
          "discarding first COMMON owner left the hash entry");
    check_common_owner_teardown(before_first,
                                "tearing down first COMMON owner leaked state");
}

int main(void)
{
    long system_page_size = sysconf(_SC_PAGESIZE);
    CHECK(system_page_size > 0, "could not read system page size");
    page_size = system_page_size > 0 ? (size_t)system_page_size : 4096u;

    ll_compat_host_ops default_ops = make_ops(&default_host);
    ll_compat_host_ops owner_ops = make_ops(&owner_host);
    ll_install_host_for(&ll_default_linker, &default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_sentinel,
           sizeof(default_sentinel));
    ll_init_linker(&owner, 0);
    owner.services.cfg.always_pic = true;

    run_alloc_failure("macho-info-nomem",
                      (FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0},
                      "ocInit_MachO(ObjectCodeFormatInfo)", 1, true);
    run_alloc_failure("macho-symbol-metadata-nomem",
                      (FixtureSpec){PROFILE_REGULAR, SYMBOLS_EXPORT, 0},
                      "ocInit_MachO(MachOSymbol)", 1, true);
    run_reinit_allocation_failure(
        "macho-reinit-info-nomem",
        "ocInit_MachO(ObjectCodeFormatInfo)", false);
    run_reinit_allocation_failure(
        "macho-reinit-symbol-metadata-nomem",
        "ocInit_MachO(MachOSymbol)", true);
    run_alloc_failure("macho-section-array-nomem",
                      (FixtureSpec){PROFILE_REGULAR, SYMBOLS_NONE, 0},
                      "ocGetNames_MachO(sections)", 1, false);
    run_segment_map_failure();
    run_segment_metadata_failures();
    run_alloc_failure("macho-section-info-nomem",
                      (FixtureSpec){PROFILE_REGULAR, SYMBOLS_NONE, 0},
                      "addSection(SectionFormatInfo)", 1, false);
    run_alloc_failure("macho-export-array-nomem",
                      (FixtureSpec){PROFILE_REGULAR, SYMBOLS_EXPORT, 0},
                      "ocGetNames_MachO(oc->symbols)", 1, false);
    run_alloc_failure("macho-common-storage-nomem",
                      (FixtureSpec){PROFILE_EMPTY, SYMBOLS_COMMON, 64},
                      "ocGetNames_MachO(common symbols)", 1, false);
    run_common_size_conflict();
    run_section_map_failure();
    run_zero_symbol_control();
    run_successful_regular_control();

    CHECK(map_failure == MAP_FAIL_NONE, "mapper injection remained armed");
    CHECK(owner_host.fatals == 0, "owner fatal callback was called");
    CHECK(default_host.allocs == 0 && default_host.frees == 0
              && default_host.logs == 0 && default_host.fatals == 0,
          "owner operations changed default host callbacks");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && memcmp(ll_default_linker.error_message, default_sentinel,
                        sizeof(default_sentinel)) == 0,
          "owner operations changed default linker error sentinel");
    ll_exit_linker(&owner);
    CHECK(owner_host.live == 0, "linker exit left owner allocations live");
    CHECK(active_mappings() == 0, "linker exit left native mappings live");
    CHECK(default_host.allocs == 0 && default_host.frees == 0
              && default_host.logs == 0 && default_host.fatals == 0,
          "owner exit changed default host callback counters");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && memcmp(ll_default_linker.error_message, default_sentinel,
                        sizeof(default_sentinel)) == 0,
          "owner exit changed default linker error sentinel");
    if (failures != 0) return 1;
    (void)puts("macho_load_errors: PASS");
    return 0;
}
