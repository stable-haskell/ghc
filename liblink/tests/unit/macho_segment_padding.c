#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#if defined(OBJFORMAT_MACHO)
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <mach-o/arm64/reloc.h>
#include <mach-o/x86_64/reloc.h>
#endif
#include "linker/MachO.h"
#include "linker/MMap.h"
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#if !defined(OBJFORMAT_MACHO) || !(defined(aarch64_HOST_ARCH) || defined(arm64_HOST_ARCH) || defined(x86_64_HOST_ARCH))
#error "macho_segment_padding is only built for a supported native Mach-O target"
#endif

enum { MAX_OWNED = 512, MAX_MAPPINGS = 8, MAX_SECTIONS = 9, IMAGE_CAPACITY = 4096 };
typedef struct {
    void *owned[MAX_OWNED];
    size_t live;
    unsigned allocs;
    unsigned frees;
    unsigned fatals;
} Host;

typedef struct {
    void *base;
    size_t size;
    size_t released;
    bool active;
} Mapping;

typedef enum { CLASS_RX, CLASS_RW, CLASS_GB } SectionClass;
typedef struct {
    SectionClass section_class;
    uint32_t align;
    uint64_t size;
    bool zero_fill;
} SectionSpec;

typedef struct {
    unsigned allocs;
    unsigned frees;
    size_t live;
    StrHashTable *symhash;
    ObjectCode *objects;
    ObjectCode *loaded;
} Snapshot;

static Host host;
static ll_linker owner;
static Mapping mappings[MAX_MAPPINGS];
static size_t page_size;
static unsigned map_calls;
static unsigned unmap_calls;
static unsigned failures;

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "macho_segment_padding:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void *segment_test_mmap_anon(ll_linker *L, size_t bytes);
static void segment_test_munmap(ll_linker *L, void *addr, size_t bytes,
                                const char *caller);

#define ll_mmap_anon_for_linker segment_test_mmap_anon
#define ll_munmap_for_linker segment_test_munmap
#include "linker/MachO.c"
#include "Linker.c"
#undef ll_mmap_anon_for_linker
#undef ll_munmap_for_linker

static void remember(void *pointer)
{
    CHECK(pointer != NULL, "owner allocation returned NULL");
    CHECK(host.live < MAX_OWNED, "owner allocation ledger overflow");
    if (pointer != NULL && host.live < MAX_OWNED) host.owned[host.live++] = pointer;
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    (void)ctx;
    (void)what;
    void *pointer = malloc(size == 0 ? 1 : size);
    ++host.allocs;
    remember(pointer);
    return pointer;
}

static void *host_calloc(void *ctx, size_t count, size_t size, const char *what)
{
    (void)ctx;
    (void)what;
    void *pointer = calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
    ++host.allocs;
    remember(pointer);
    return pointer;
}

static void *host_realloc(void *ctx, void *pointer, size_t size, const char *what)
{
    (void)ctx;
    (void)what;
    for (size_t i = 0; i < host.live; ++i) {
        if (host.owned[i] == pointer) {
            void *resized = realloc(pointer, size == 0 ? 1 : size);
            if (resized != NULL) host.owned[i] = resized;
            return resized;
        }
    }
    CHECK(false, "realloc pointer was not owned by test linker");
    return NULL;
}

static void host_free(void *ctx, void *pointer)
{
    (void)ctx;
    if (pointer == NULL) return;
    for (size_t i = 0; i < host.live; ++i) {
        if (host.owned[i] == pointer) {
            host.owned[i] = host.owned[--host.live];
            ++host.frees;
            free(pointer);
            return;
        }
    }
    CHECK(false, "free pointer was not owned by test linker");
}

static void host_log(void *ctx, int kind, const char *format, va_list args)
{
    (void)ctx;
    (void)kind;
    char message[1024];
    int written = vsnprintf(message, sizeof(message), format, args);
    CHECK(written >= 0 && (size_t)written < sizeof(message), "host log was truncated");
}

static _Noreturn void host_fatal(void *ctx, const char *format, va_list args)
{
    (void)ctx;
    char message[1024];
    (void)vsnprintf(message, sizeof(message), format, args);
    ++host.fatals;
    (void)fprintf(stderr, "unexpected linker fatal: %s\n", message);
    _Exit(98);
}

static size_t host_page_size(void *ctx)
{
    (void)ctx;
    return page_size;
}

static void host_mem_failure(void *ctx) { (void)ctx; }

static ll_compat_host_ops make_ops(void)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = &host;
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
    return (Snapshot){host.allocs, host.frees, host.live, owner.symhash,
                      owner.gc.objects, owner.gc.loaded_objects};
}

static void check_snapshot(Snapshot before, const char *label)
{
    Snapshot after = snapshot();
    CHECK(after.allocs - before.allocs == after.frees - before.frees, label);
    CHECK(after.live == before.live, label);
    CHECK(after.symhash == before.symhash && after.objects == before.objects
              && after.loaded == before.loaded, label);
}

/* Return an ordinary mmap region whose base is page aligned but half-way
 * through a 2-page phase. Keep only the requested bytes mapped so the runtime
 * unmap path can release each consecutive segment exactly as it does in use. */
static void *segment_test_mmap_anon(ll_linker *L, size_t bytes)
{
    (void)L;
    ++map_calls;
    if (page_size == 0 || page_size > SIZE_MAX / 2 || bytes % page_size != 0
        || bytes > SIZE_MAX - 2 * page_size) return NULL;
    size_t two_pages = 2 * page_size;
    size_t total = bytes + two_pages;
    void *raw = mmap(NULL, total, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON, -1, 0);
    if (raw == MAP_FAILED) return NULL;

    size_t phase = (size_t)((uintptr_t)raw % two_pages);
    size_t prefix = phase == page_size ? 0 : page_size;
    void *base = (unsigned char *)raw + prefix;
    size_t suffix = total - prefix - bytes;
    if ((prefix != 0 && munmap(raw, prefix) != 0)
        || (suffix != 0 && munmap((unsigned char *)base + bytes, suffix) != 0)) {
        (void)munmap(base, bytes);
        return NULL;
    }
    CHECK((uintptr_t)base % page_size == 0, "mapper base is not page aligned");
    CHECK((uintptr_t)base % two_pages == page_size,
          "mapper did not force a non-2-page-aligned base");
    for (size_t i = 0; i < MAX_MAPPINGS; ++i) {
        if (!mappings[i].active) {
            mappings[i] = (Mapping){base, bytes, 0, true};
            return base;
        }
    }
    (void)munmap(base, bytes);
    CHECK(false, "mapping ledger exhausted");
    return NULL;
}

static void segment_test_munmap(ll_linker *L, void *addr, size_t bytes,
                                const char *caller)
{
    (void)L;
    (void)caller;
    ++unmap_calls;
    for (size_t i = 0; i < MAX_MAPPINGS; ++i) {
        Mapping *mapping = &mappings[i];
        if (mapping->active && (unsigned char *)addr
                == (unsigned char *)mapping->base + mapping->released
            && bytes <= mapping->size - mapping->released) {
            if (munmap(addr, bytes) != 0) {
                CHECK(false, "runtime segment unmap failed");
                return;
            }
            mapping->released += bytes;
            if (mapping->released == mapping->size) mapping->active = false;
            return;
        }
    }
    CHECK(false, "runtime unmap did not match the mapper's owned range");
}

static unsigned log2_exact(size_t value)
{
    unsigned shift = 0;
    while (value > 1) {
        CHECK((value & 1u) == 0, "page size is not a power of two");
        value >>= 1;
        ++shift;
    }
    return shift;
}

static void set_section(MachOSection *section, const SectionSpec *spec,
                        size_t payload_offset)
{
    memset(section, 0, sizeof(*section));
    const char *segment = spec->section_class == CLASS_RX ? "__TEXT" : "__DATA";
    const char *name = spec->section_class == CLASS_RX ? "__text"
        : spec->section_class == CLASS_RW ? "__data" : "__gbss";
    memcpy(section->segname, segment, strlen(segment) + 1);
    memcpy(section->sectname, name, strlen(name) + 1);
    section->align = spec->align;
    section->size = spec->size;
    section->offset = (uint32_t)payload_offset;
    section->flags = spec->section_class == CLASS_GB ? S_GB_ZEROFILL
        : spec->zero_fill ? S_ZEROFILL : S_REGULAR;
}

static ObjectCode *make_object(const char *label, const SectionSpec *specs,
                               size_t count)
{
    CHECK(count > 0 && count <= MAX_SECTIONS, "section spec count out of bounds");
    if (count == 0 || count > MAX_SECTIONS) return NULL;
    size_t cmd_size = sizeof(MachOSegmentCommand) + count * sizeof(MachOSection);
    size_t image_size = sizeof(MachOHeader) + cmd_size + 256;
    CHECK(image_size <= IMAGE_CAPACITY && image_size <= INT_MAX,
          "synthetic image exceeds fixed buffer bounds");
    if (image_size > IMAGE_CAPACITY || image_size > INT_MAX) return NULL;

    unsigned char *image = host_calloc(&host, 1, image_size, "segment padding image");
    if (image == NULL) return NULL;
    MachOHeader *header = (MachOHeader *)image;
    header->magic = MH_MAGIC_64;
    header->filetype = MH_OBJECT;
    header->ncmds = 1;
    header->sizeofcmds = (uint32_t)cmd_size;

    MachOSegmentCommand *segment = (MachOSegmentCommand *)(header + 1);
    segment->cmd = LC_SEGMENT_64;
    segment->cmdsize = (uint32_t)cmd_size;
    segment->nsects = (uint32_t)count;
    MachOSection *sections = (MachOSection *)(segment + 1);
    for (size_t i = 0; i < count; ++i)
        set_section(&sections[i], &specs[i], sizeof(MachOHeader) + cmd_size);

    ObjectCode *oc = mkOc(&owner, STATIC_OBJECT, (pathchar *)label,
                           (char *)image, (int)image_size, false, NULL, 0);
    if (oc == NULL) {
        host_free(&host, image);
        return NULL;
    }
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0 && owner.error_status == LL_OK,
          "ll_init_object rejected the synthetic section image");
    return oc;
}

static int segment_for_section(ObjectCode *oc, int section_index)
{
    for (int i = 0; i < oc->n_segments; ++i)
        for (int j = 0; j < oc->segments[i].n_sections; ++j)
            if (oc->segments[i].sections_idx[j] == section_index) return i;
    return -1;
}

static bool check_spans(ObjectCode *oc, const SectionSpec *specs, size_t count)
{
    bool ok = oc->n_segments > 0;
    for (size_t i = 0; i < count; ++i) {
        int segment_index = segment_for_section(oc, (int)i);
        if (specs[i].size == 0) continue;
        if (segment_index < 0) { ok = false; continue; }
        Segment *segment = &oc->segments[segment_index];
        uintptr_t base = (uintptr_t)segment->start;
        uintptr_t position = base;
        for (int j = 0; j < segment->n_sections; ++j) {
            int section_index = segment->sections_idx[j];
            const MachOSection *section = &oc->info->macho_sections[section_index];
            size_t alignment = (size_t)1 << section->align;
            uintptr_t start = (position + alignment - 1) & ~(uintptr_t)(alignment - 1);
            uintptr_t end = start + (uintptr_t)section->size;
            if (section_index == (int)i) {
                size_t offset = (size_t)(start - base);
                ok = ok && start % alignment == 0 && offset <= segment->size
                    && section->size <= segment->size - offset;
            }
            position = end;
        }
        CHECK(segment->size % page_size == 0,
              "segment allocation size is not page rounded");
    }
    return ok;
}

static void run_positive(const char *label, const SectionSpec *specs,
                         size_t count, size_t expected_segments)
{
    Snapshot before = snapshot();
    unsigned maps_before = map_calls;
    unsigned unmaps_before = unmap_calls;
    ObjectCode *oc = make_object(label, specs, count);
    CHECK(oc != NULL, "could not construct positive section image");
    if (oc == NULL) return;
    int built = ocBuildSegments_MachO(oc);
    CHECK(built != 0 && owner.error_status == LL_OK,
          "ocBuildSegments_MachO rejected valid section layout");
    if (built) {
        CHECK(oc->n_segments == (int)expected_segments,
              "segment grouping count differs from RX/RW/GB classes");
        CHECK(check_spans(oc, specs, count),
              "computed section span escapes its owning segment");
        for (int i = 0; i < oc->n_segments; ++i) {
            SegmentProt expected = i == 0 ? SEGMENT_PROT_RX : SEGMENT_PROT_RWO;
            CHECK(oc->segments[i].prot == expected,
                  "segment protection class/order differs from RX then RW then GB");
            for (int j = 0; j < oc->segments[i].n_sections; ++j) {
                int index = oc->segments[i].sections_idx[j];
                SectionClass expected_class = i == 0 ? CLASS_RX
                    : i == 1 ? CLASS_RW : CLASS_GB;
                CHECK(specs[index].section_class == expected_class,
                      "section was assigned to the wrong RX/RW/GB segment");
            }
        }
    }
    CHECK(map_calls == maps_before + 1,
          "valid nonempty segment layout did not allocate exactly one compound mapping");
    CHECK(unmap_calls == unmaps_before,
          "mapping was released before explicit segment teardown");
    freeSegments(oc);
    CHECK(unmap_calls == unmaps_before + expected_segments,
          "teardown did not release each segment mapping span");
    for (size_t i = 0; i < MAX_MAPPINGS; ++i)
        CHECK(!mappings[i].active, "compound mapping remained live after segment teardown");
    ll_discard_object(oc);
    check_snapshot(before, label);
}

static void run_overflow(SectionClass section_class, const char *label)
{
    unsigned page_shift = log2_exact(page_size);
    SectionSpec spec = {section_class, page_shift + 1, 8, false};
    Snapshot before = snapshot();
    unsigned maps_before = map_calls;
    unsigned unmaps_before = unmap_calls;
    ObjectCode *oc = make_object(label, &spec, 1);
    CHECK(oc != NULL, "could not construct overflow section image");
    if (oc == NULL) return;

    /* Keep the serialized object small and valid; the segment builder consumes
     * this post-validation size exactly as if it came from section metadata. */
    oc->info->macho_sections[0].size = (uint64_t)(SIZE_MAX - page_size + 1);
    ll_error_clear(&owner);
    int built = ocBuildSegments_MachO(oc);
    CHECK(built == 0 && owner.error_status == LL_E_FORMAT,
          "padding addition overflow was not reported as LL_E_FORMAT");
    CHECK(strstr(owner.error_message, "alignment padding overflow") != NULL,
          "padding overflow diagnostic was not specific");
    CHECK(map_calls == maps_before,
          "overflow reached the segment mapper before rejecting the size");
    CHECK(unmap_calls == unmaps_before,
          "overflow unexpectedly released a segment mapping");
    CHECK(oc->segments == NULL && oc->n_segments == 0,
          "overflow left partially constructed segment state");
    ll_discard_object(oc);
    check_snapshot(before, label);
}

int main(void)
{
    long native_page_size = sysconf(_SC_PAGESIZE);
    CHECK(native_page_size > 0, "could not determine native page size");
    if (native_page_size <= 0) return 2;
    page_size = (size_t)native_page_size;

    ll_compat_host_ops ops = make_ops();
    ll_install_host_for(&owner, &ops, NULL);
    ll_init_linker(&owner, 0);
    CHECK(page_size != 0 && (page_size & (page_size - 1)) == 0,
          "native page size is not a power of two");

    unsigned page_shift = log2_exact(page_size);
    const SectionSpec page_aligned[] = {
        {CLASS_RX, 4, 21, false}, {CLASS_RW, 3, 29, false},
        {CLASS_GB, 2, 13, false}
    };
    run_positive("page-aligned-rx-rw-gb", page_aligned, 3, 3);

    const SectionSpec single_overaligned[] = {
        {CLASS_RX, page_shift + 1, 8, false},
        {CLASS_RW, page_shift + 1, 8, false},
        {CLASS_GB, page_shift + 1, 8, false}
    };
    run_positive("over-page-rx-rw-gb", single_overaligned, 3, 3);

    const SectionSpec mixed_overaligned[] = {
        {CLASS_RX, 4, 19, false}, {CLASS_RX, page_shift + 1, 37, false},
        {CLASS_RX, 3, 11, false}, {CLASS_RW, 3, 17, false},
        {CLASS_RW, page_shift + 1, 23, false}, {CLASS_RW, 4, 15, true},
        {CLASS_GB, 2, 29, false}, {CLASS_GB, page_shift + 1, 31, false}
    };
    run_positive("mixed-over-page-rx-rw-gb", mixed_overaligned, 8, 3);

    run_overflow(CLASS_RX, "rx-padding-add-overflow");
    run_overflow(CLASS_RW, "rw-padding-add-overflow");
    run_overflow(CLASS_GB, "gb-padding-add-overflow");

    CHECK(host.fatals == 0, "test invoked the fatal callback");
    for (size_t i = 0; i < MAX_MAPPINGS; ++i)
        CHECK(!mappings[i].active, "test ended with a live mapper allocation");
    ll_exit_linker(&owner);
    CHECK(host.live == 0, "linker exit left owner allocations live");
    if (failures != 0) return 1;
    (void)puts("macho_segment_padding: PASS");
    return 0;
}
