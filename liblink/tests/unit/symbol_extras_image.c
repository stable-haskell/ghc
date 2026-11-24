/* Real native image metadata and shared image-replacement ownership. */
#define main archive_status_original_main
#include "archive_error_status.c"
#undef main
#include "ll_format.h"
#include "Linker.c"

#if RTS_LINKER_USE_MMAP && defined(NEED_SYMBOL_EXTRAS)

static bool fail_extra_map;
static unsigned extra_maps;
static unsigned extra_unmaps;
static uintptr_t tracked_heap_base;
static bool heap_release_pending;
static unsigned heap_releases;

static void image_host_free(void *context, void *pointer)
{
    if (heap_release_pending && (uintptr_t)pointer == tracked_heap_base) {
        ++heap_releases;
        heap_release_pending = false;
    }
    host_free(context, pointer);
}

static void *extra_image_map(ll_linker *owner, size_t size)
{
    ++extra_maps;
    if (fail_extra_map) {
        fail_extra_map = false;
        errno = ENOMEM;
        ll_error_set(owner, LL_E_NOMEM, "injected extra image mapping failure");
        return NULL;
    }
    return ll_mmap_anon_for_linker(owner, size);
}

static void extra_image_unmap(ll_linker *owner, void *image, size_t size,
                              const char *caller)
{
    ++extra_unmaps;
    ll_munmap_for_linker(owner, image, size, caller);
}

#define ll_mmap_anon_for_linker extra_image_map
#define ll_munmap_for_linker extra_image_unmap
#ifndef LL_EXTRAS_SOURCE
#define LL_EXTRAS_SOURCE "linker/SymbolExtras.c"
#endif
#include LL_EXTRAS_SOURCE
#undef ll_munmap_for_linker
#undef ll_mmap_anon_for_linker

static bool owns(const Host *host, uintptr_t address)
{
    for (size_t i = 0; i < host->live; ++i)
        if ((uintptr_t)host->owned[i] == address) return true;
    return false;
}

static void check_image(ll_linker *owner, const unsigned char *bytes, size_t size,
                         bool mapped, unsigned misalignment, bool fail_map)
{
    const size_t before = host_a.live;
    char *base = mapped ? ll_mmap_anon_for_linker(owner, size) :
        LL_MALLOC_FOR(owner, size + misalignment, "symbol extra image input");
    CHECK(base != NULL, "could not allocate valid image storage");
    if (base == NULL) return;
    char *image = base + misalignment;
    const uintptr_t image_address = (uintptr_t)image;
    tracked_heap_base = (uintptr_t)base;
    heap_release_pending = !mapped;
    heap_releases = 0;
    memcpy(image, bytes, size);
    ObjectCode *object = mkOc(owner, STATIC_OBJECT, WSTR("valid-compiler-object.o"),
                              image, (int)size, mapped, NULL, (int)misalignment);
    ll_error_clear(owner);
    CHECK(ll_init_object(object), "valid compiler object initialization failed");
    if (object->format_ops == NULL) {
        freeObjectCode(object);
        return;
    }
    extra_maps = extra_unmaps = 0;
    fail_extra_map = fail_map;
    int result = object->format_ops->alloc_extras(object);
    CHECK(extra_maps == 1, "image replacement did not request one mapping");
    if (fail_map) {
        CHECK(!result && owner->error_status == LL_E_NOMEM &&
              strcmp(owner->error_message, "injected extra image mapping failure") == 0,
              "mapping failure lost its result or primary diagnostic");
        CHECK((uintptr_t)object->image == image_address && object->imageMapped == mapped &&
              memcmp(object->image, bytes, size) == 0,
              "failed replacement changed the original image");
        CHECK(extra_unmaps == 0 &&
              (mapped || (heap_release_pending && owns(&host_a, tracked_heap_base))),
              "failed replacement released the original storage");
    } else {
        CHECK(result && (uintptr_t)object->image != image_address && object->imageMapped &&
              memcmp(object->image, bytes, size) == 0,
              "replacement lost the valid image bytes or mapped ownership");
        CHECK(extra_unmaps == (mapped ? 1u : 0u),
              "replacement released the wrong storage class");
        CHECK(mapped || (heap_releases == 1 && !heap_release_pending),
              "successful replacement retained its old heap allocation");
    }
    freeObjectCode(object);
    CHECK(mapped || (heap_releases == 1 && !heap_release_pending),
          "original heap allocation was not released exactly once");
    CHECK(host_a.live == before, "image replacement/disposal leaked owner storage");
    if (fail_map) {
        CHECK(owner->error_status == LL_E_NOMEM &&
              strcmp(owner->error_message, "injected extra image mapping failure") == 0,
              "failure cleanup replaced the original diagnostic");
    }
    (void)fprintf(stderr,
        "symbol_extras_image: mapped=%d offset=%u fail=%d maps=%u unmaps=%u retained_heap=%d\n",
        mapped, misalignment, fail_map, extra_maps, extra_unmaps,
        heap_release_pending);
    /* Keep a failing old-code run bounded; assertions above still fail. */
    if (heap_release_pending && owns(&host_a, tracked_heap_base))
        LL_FREE_FOR(owner, (void *)tracked_heap_base);
}

int main(int argc, char **argv)
{
    if (argc != 2) return 2;
    unsigned char bytes[32768];
    FILE *input = fopen(argv[1], "rb");
    CHECK(input != NULL, "could not open valid compiler object");
    if (input == NULL) return 1;
    size_t size = fread(bytes, 1, sizeof(bytes), input);
    CHECK(!ferror(input) && feof(input) && size > 8,
          "compiler object is empty, too large, or unreadable");
    CHECK(fclose(input) == 0, "could not close compiler object");
    if (failures != 0) return 1;

    ll_linker owner = {0};
    ll_compat_host_ops ops = make_ops(&host_a);
    ops.free_ = image_host_free;
    ll_compat_config config = {.always_pic = true};
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    for (unsigned fail = 0; fail < 2; ++fail) {
        check_image(&owner, bytes, size, false, 0, fail != 0);
        check_image(&owner, bytes, size, false, 8, fail != 0);
        check_image(&owner, bytes, size, true, 0, fail != 0);
    }
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    CHECK(host_a.live == 0 && host_a.alloc_calls == host_a.free_calls,
          "image controls leaked owner allocations");
    CHECK(host_a.fatal_calls == 0, "image controls called fatal");
    if (failures != 0) return 1;
    (void)puts("symbol_extras_image: PASS (heap, offset heap, mapped image, mapping failures)");
    return 0;
}

#else
int main(void)
{
    (void)puts("symbol_extras_image: SKIP (native mmap symbol extras unavailable)");
    return 0;
}
#endif
