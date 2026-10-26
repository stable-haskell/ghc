/* Exercise the shared storage primitive without selecting an object format. */
#define main archive_status_original_main
#include "archive_error_status.c"
#undef main
#include "Linker.c"

#if RTS_LINKER_USE_MMAP && defined(NEED_SYMBOL_EXTRAS) && \
    (!defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS))

static unsigned storage_maps;
static unsigned storage_refreshes;
static unsigned storage_protections;
static size_t storage_map_bytes;
static size_t storage_protect_bytes;
static void *storage_protect_start;
static bool storage_fail_map;
static bool storage_fail_refresh;
static bool storage_fail_protect;
static unsigned storage_m32_calls;
static size_t storage_m32_bytes;
static ll_linker *storage_m32_owner;
static m32_allocator *storage_m32_allocator;
static _Alignas(16) unsigned char storage_m32_payload[64];

static void *storage_map(ll_linker *owner, size_t size)
{
    ++storage_maps;
    storage_map_bytes = size;
    if (storage_fail_map) {
        ll_error_set(owner, LL_E_NOMEM, "injected storage mapping failure");
        return NULL;
    }
    void *result = ll_mmap_anon_for_linker(owner, size);
    if (result != NULL) memset(result, 0xa5, size);
    return result;
}

static bool storage_protect(ll_linker *owner, void *start, size_t size,
                            MemoryAccess mode)
{
    ++storage_protections;
    storage_protect_start = start;
    storage_protect_bytes = size;
    CHECK(mode == MEM_READ_EXECUTE, "extras protection changed access mode");
    if (storage_fail_protect) {
        ll_error_set(owner, LL_E_IO, "injected storage protection failure");
        return false;
    }
    return true;
}

/* This primitive test checks the allocator request, not OS placement. Real
 * M32 mapping/protection/ownership remain covered by their existing units. */
static void *storage_m32_alloc(ll_linker *owner, m32_allocator *allocator,
                               size_t size, size_t alignment)
{
    ++storage_m32_calls;
    storage_m32_owner = owner;
    storage_m32_allocator = allocator;
    storage_m32_bytes = size;
    CHECK(size <= sizeof(storage_m32_payload) && alignment == 8,
          "extras changed its M32 size or alignment contract");
    if (size > sizeof(storage_m32_payload) || alignment != 8) return NULL;
    memset(storage_m32_payload, 0xa5, sizeof(storage_m32_payload));
    return storage_m32_payload;
}

#define m32_alloc_for storage_m32_alloc
#define ll_mmap_anon_for_linker storage_map
#define ll_mprotect_for_linker storage_protect
#include "linker/SymbolExtras.c"
#undef ll_mprotect_for_linker
#undef ll_mmap_anon_for_linker
#undef m32_alloc_for

static int storage_refresh(ObjectCode *object)
{
    ++storage_refreshes;
    CHECK(object->first_symbol_extra == 19 && object->n_symbol_extras == 17 &&
          object->symbol_extra_stride == 23,
          "allocator published counts or stride before metadata refresh");
    if (storage_fail_refresh) {
        ll_error_set(object->L, LL_E_FORMAT, "injected storage refresh failure");
        return 0;
    }
    return 1;
}

typedef enum {
    STORAGE_SUCCESS,
    STORAGE_MAP_FAILURE,
    STORAGE_REFRESH_FAILURE,
    STORAGE_PROTECTION_FAILURE,
    STORAGE_NO_REFRESH
} StorageCase;

static void check_storage(ll_linker *owner, size_t stride, int count, int bss,
                           bool contiguous, StorageCase mode)
{
    unsigned char bytes[64];
    for (size_t i = 0; i < sizeof(bytes); ++i) bytes[i] = (unsigned char)i;
    char *image = LL_MALLOC_FOR(owner, sizeof(bytes), "storage input");
    CHECK(image != NULL, "could not allocate storage input");
    if (image == NULL) return;
    memcpy(image, bytes, sizeof(bytes));
    const uintptr_t original_address = (uintptr_t)image;
    ObjectCode *object = mkOc(owner, STATIC_OBJECT, WSTR("storage-only"), image,
                              sizeof(bytes), false, NULL, 0);
    CHECK(object->symbol_extra_stride == 0,
          "new object did not initialize its stride");
    /* These sentinels detect publication before the original refresh point. */
    object->first_symbol_extra = 19;
    object->n_symbol_extras = 17;
    object->symbol_extra_stride = 23;
    storage_maps = storage_refreshes = storage_protections = 0;
    storage_m32_calls = 0;
    storage_m32_bytes = 0;
    storage_m32_owner = NULL;
    storage_m32_allocator = NULL;
    storage_map_bytes = storage_protect_bytes = 0;
    storage_protect_start = NULL;
    storage_fail_map = mode == STORAGE_MAP_FAILURE;
    storage_fail_refresh = mode == STORAGE_REFRESH_FAILURE;
    storage_fail_protect = mode == STORAGE_PROTECTION_FAILURE;
    owner->services.cfg.always_pic = contiguous;
    ll_error_clear(owner);

    int result = ocAllocateExtras(object, count, 5, bss, stride,
        mode == STORAGE_NO_REFRESH ? NULL : storage_refresh);
    const bool allocate = count > 0 || bss > 0;
    const bool moved = allocate && contiguous && mode != STORAGE_MAP_FAILURE;
    CHECK(storage_maps == (unsigned)(allocate && contiguous),
          "storage mapping count differs from allocation policy");
    CHECK(storage_refreshes == (unsigned)(moved && mode != STORAGE_NO_REFRESH),
          "refresh did not follow actual image replacement");
    CHECK(storage_m32_calls == (unsigned)(allocate && !contiguous),
          "M32 allocation did not follow selected storage policy");
    if (storage_m32_calls != 0) {
        CHECK(storage_m32_owner == owner && storage_m32_allocator == object->rx_m32 &&
              storage_m32_bytes == stride * count &&
              (void *)object->symbol_extras == storage_m32_payload,
              "extras changed M32 owner, allocator, or target byte length");
        for (size_t i = 0; i < stride * count; ++i)
            CHECK(storage_m32_payload[i] == 0, "M32 extras were not zeroed");
        CHECK(storage_m32_payload[stride * count] == 0xa5,
              "M32 extras zeroing changed following sentinel");
    }
    CHECK(memcmp(object->image, bytes, sizeof(bytes)) == 0,
          "storage allocation changed input image bytes");
    CHECK(((uintptr_t)object->image != original_address) == moved,
          "storage allocation changed image address unexpectedly");
    if (allocate && contiguous) {
        const size_t image_bytes = ll_round_up_to_page_for(owner, sizeof(bytes));
        const size_t bss_bytes = ll_round_up_to_page_for(owner, bss);
        CHECK(storage_map_bytes == image_bytes + bss_bytes + stride * count,
              "mapping size used host payload width or changed BSS padding");
        if (moved) {
            const unsigned char *extras = (const void *)object->symbol_extras;
            CHECK(extras == (const unsigned char *)object->image +
                             image_bytes + bss_bytes,
                  "extras placement changed");
            CHECK(extras[-1] == 0xa5, "extras zeroing changed preceding padding");
            for (size_t i = 0; i < stride * count; ++i)
                CHECK(extras[i] == 0, "extras payload was not zeroed");
        }
    }
    if (mode == STORAGE_MAP_FAILURE || mode == STORAGE_REFRESH_FAILURE) {
        const ll_status expected = mode == STORAGE_MAP_FAILURE ? LL_E_NOMEM :
                                                                     LL_E_FORMAT;
        CHECK(result == 0 && owner->error_status == expected,
              "storage failure lost its result or diagnostic");
        CHECK(object->first_symbol_extra == 19 && object->n_symbol_extras == 17 &&
              object->symbol_extra_stride == 23,
              "storage failure published unfinished metadata");
    } else {
        CHECK(result && object->first_symbol_extra == 5 &&
              object->n_symbol_extras == (unsigned long)count &&
              object->symbol_extra_stride == stride,
              "storage allocator did not publish selected target metadata");
        bool protected = ocProtectExtras(object);
        CHECK(protected == !storage_fail_protect,
              "protection result was not propagated");
        CHECK(storage_protections == (unsigned)(count != 0 && contiguous),
              "protection did not retain count and storage policy");
        if (storage_protections != 0) {
            CHECK(storage_protect_start == object->symbol_extras &&
                  storage_protect_bytes == stride * count,
                  "protection used the host payload width");
        }
        CHECK(owner->error_status == (storage_fail_protect ? LL_E_IO : LL_OK),
              "storage/protection status changed");
    }
    ll_status status = owner->error_status;
    char diagnostic[LL_ERROR_CAPACITY];
    memcpy(diagnostic, owner->error_message, sizeof(diagnostic));
    freeObjectCode(object);
    CHECK(owner->error_status == status &&
          memcmp(diagnostic, owner->error_message, sizeof(diagnostic)) == 0,
          "storage disposal replaced the primary diagnostic");
    (void)fprintf(stderr,
        "symbol_extras_storage: stride=%zu count=%d bss=%d contiguous=%d case=%d\n",
        stride, count, bss, contiguous, mode);
}

int main(void)
{
    ll_linker owner = {0};
    ll_compat_host_ops ops = make_ops(&host_a);
    ll_compat_config config = {.always_pic = true};
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    const size_t strides[] = {0, 8, 16};
    for (size_t i = 0; i < sizeof(strides) / sizeof(strides[0]); ++i) {
        for (StorageCase mode = STORAGE_SUCCESS;
             mode <= STORAGE_PROTECTION_FAILURE; ++mode)
            check_storage(&owner, strides[i], 2, 0, true, mode);
    }
    check_storage(&owner, 16, 0, 0, true, STORAGE_SUCCESS);
    check_storage(&owner, 0, 0, 16, true, STORAGE_SUCCESS);
    check_storage(&owner, 16, 2, 0, true, STORAGE_NO_REFRESH);
#if !USE_CONTIGUOUS_MMAP
    check_storage(&owner, 8, 2, 0, false, STORAGE_SUCCESS);
    check_storage(&owner, 16, 2, 0, false, STORAGE_SUCCESS);
#endif
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    ll_exit_linker(&owner);
    CHECK(host_a.live == 0 && host_a.alloc_calls == host_a.free_calls,
          "storage controls leaked owner allocations");
    CHECK(host_a.fatal_calls == 0, "storage controls called fatal");
    if (failures != 0) return 1;
    (void)puts("symbol_extras_storage: PASS (strides, refresh, failures, protection)");
    return 0;
}
#else
int main(void)
{
    (void)puts("symbol_extras_storage: SKIP (shared native mmap extras unavailable)");
    return 0;
}
#endif
