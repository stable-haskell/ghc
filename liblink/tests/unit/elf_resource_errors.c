/* Reuse owner/mapping ledgers and syscall wrappers, without running the
 * section unit's main. The backend remains supplied by the native archive. */
#define main elf_section_original_main
#define __wrap_ll_mmap_anon_for_linker section_anon_wrapper
#include "elf_section_errors.c"
#undef __wrap_ll_mmap_anon_for_linker
#undef main
#include "ll_failure.h"
#include "Linker.c"

enum ResourceFailure { RESOURCE_NONE, RESOURCE_COMMON, RESOURCE_GOT };
static enum ResourceFailure resource_failure;
static ObjectCode *staged;
static size_t expected_common;
static void *common_map;
static void *got_map;
static void *clobber_pointer;
static unsigned resource_injections;
static unsigned common_requests;
static unsigned cleanup_clobbers;
static unsigned registrations;

void *__wrap_ll_mmap_anon_for_linker(ll_linker *L, size_t bytes)
{
    bool got = staged != NULL && staged->info != NULL
               && staged->info->got_size != 0
               && staged->info->got_size == bytes
               && staged->info->got_start == NULL;
    /* In these BSS-free contiguous fixtures, COMMON is the only anonymous
     * acquisition after Section[] exists and before got_size is published. */
    bool common = staged != NULL && staged->sections != NULL && !got;
    if (common) {
        ++common_requests;
        CHECK(expected_common != 0 && bytes == expected_common,
              "unexpected COMMON mapping request");
    }
    if ((common && resource_failure == RESOURCE_COMMON)
        || (got && resource_failure == RESOURCE_GOT)) {
        ++resource_injections;
        errno = ENOMEM;
        return NULL;
    }
    void *p = section_anon_wrapper(L, bytes);
    if (common && p != NULL) common_map = p;
    if (got && p != NULL) got_map = p;
    return p;
}

static bool mapping_live(void *p)
{
    if (p == NULL) return false;
    for (size_t i = 0; i < host.map_live; ++i)
        if (host.maps[i] == p) return true;
    return false;
}

static void resource_free(void *ctx, void *p)
{
    bool clobber = p != NULL && p == clobber_pointer;
    if (clobber) clobber_pointer = NULL;
    host_free(ctx, p);
    if (clobber) {
        ++cleanup_clobbers;
        ll_error_set(&owner, LL_E_STATE, "cleanup callback diagnostic");
    }
}

static void registered(void *ctx, ObjectCode *oc)
{
    CHECK(ctx == &host && oc->L == &owner, "registration used wrong owner");
    oc->next = owner.gc.objects;
    if (oc->next != NULL) oc->next->prev = oc;
    owner.gc.objects = oc;
    ++registrations;
}

static void begin_owner(void)
{
    memset(&owner, 0, sizeof(owner));
    memset(&host, 0, sizeof(host));
    ll_compat_host_ops ops = host_ops(&host);
    ops.free_ = resource_free;
    ops.object_registered = registered;
    /* Contiguous images avoid introducing M32 page-cache lifetime into the
     * independent COMMON/GOT ownership ledger. Initial file images are mapped. */
    ll_compat_config config = {.always_pic = true};
    ll_install_host_for(&owner, &ops, &config);
    ll_init_linker(&owner, 0);
    ll_init_linker_mmap(&owner);
    staged = NULL;
    clobber_pointer = NULL;
    registrations = 0;
    cleanup_clobbers = 0;
}

static void finish_owner(void)
{
    CHECK(owner.gc.loaded_objects == NULL && owner.gc.objects == NULL,
          "test left retained objects without explicit disposal");
    CHECK(owner.m32_free_page_pool_size == 0,
          "resource-only fixture unexpectedly allocated pooled M32 pages");
    ll_exit_linker(&owner);
    CHECK(host.live == 0 && host.map_live == 0,
          "owner exit left heap allocations or mappings live");
    CHECK(ll_default_linker.error_status == LL_E_STATE
              && strcmp(ll_default_linker.error_message, "resource default sentinel") == 0
              && default_host.live == 0 && default_host.logs == 0,
          "explicit resource operation modified default owner");
}

static ObjectCode *load_staged(const char *path, size_t common_bytes,
                               enum ResourceFailure failure, int *loaded)
{
    ll_error_clear(&owner);
    ObjectCode *oc = preloadObjectFile(&owner, (pathchar *)path);
    CHECK(oc != NULL, "valid data-only fixture failed preload");
    if (oc == NULL) { *loaded = 0; return NULL; }
    staged = oc;
    expected_common = common_bytes;
    resource_failure = failure;
    common_map = got_map = NULL;
    resource_injections = 0;
    common_requests = 0;
    host.opens = host.closes = 0;
    host.fd = -1;
    host.active = true;
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    *loaded = loadOc(oc);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    host.active = false;
    staged = NULL;
    resource_failure = RESOURCE_NONE;
    CHECK(host.opens == 1 && host.closes == 1,
          "resource pass did not close its reopened descriptor exactly once");
    errno = 0;
    CHECK(host.fd >= 0 && fcntl(host.fd, F_GETFD) == -1 && errno == EBADF,
          "resource pass retained its reopened backing descriptor");
    return oc;
}

static void publish(ObjectCode *oc)
{
    registered(&host, oc);
    oc->next_loaded_object = owner.gc.loaded_objects;
    owner.gc.loaded_objects = oc;
}

static void dispose_registered(void)
{
    unsigned count = 0;
    while (owner.gc.loaded_objects != NULL) {
        if (++count > 16) {
            CHECK(false, "logical unload did not advance object list");
            return;
        }
        pathchar *path = owner.gc.loaded_objects->fileName;
        if (unloadObj_(&owner, path, false) != 1) {
            CHECK(false, "could not logically unload registered fixture");
            return;
        }
    }
    /* A disposal model after all test aliases have been unloaded. This is
     * intentionally not a claim about the RTS GC's marking/scheduling. */
    while (owner.gc.objects != NULL) {
        ObjectCode *oc = owner.gc.objects;
        owner.gc.objects = oc->next;
        freeObjectCode(oc);
    }
}

static void discard_primary(ObjectCode *oc, bool clobber)
{
    ll_status status = owner.error_status;
    char message[LL_ERROR_CAPACITY];
    memcpy(message, owner.error_message, sizeof(message));
    owner.error_from_pre_backend = true;
    if (clobber) clobber_pointer = oc->info;
    ll_discard_object(oc);
    CHECK(owner.error_status == status
              && memcmp(owner.error_message, message, sizeof(message)) == 0
              && owner.error_from_pre_backend,
          "format cleanup changed primary error/diagnostic/provenance");
    if (clobber) CHECK(cleanup_clobbers == 1, "cleanup diagnostic injection not reached");
}

static void check_mapping_case(const char *path, size_t common_bytes,
                                enum ResourceFailure failure)
{
    begin_owner();
    int loaded = 0;
    ObjectCode *oc = load_staged(path, common_bytes, failure, &loaded);
    if (oc != NULL) {
        CHECK(loaded == (failure == RESOURCE_NONE ? 1 : 0),
              "resource allocation returned wrong load result");
        CHECK(owner.error_status == (failure == RESOURCE_NONE ? LL_OK : LL_E_NOMEM),
              "resource allocation returned wrong status");
        CHECK(resource_injections == (failure == RESOURCE_NONE ? 0u : 1u),
              "exact resource failure injection not reached");
        if (failure == RESOURCE_NONE) {
            CHECK(got_map != NULL, "positive GOT allocation missing");
            if (common_bytes != 0) {
                CHECK(common_map != NULL, "positive COMMON allocation missing");
                if (common_map != NULL)
                    for (size_t i = 0; i < common_bytes; ++i)
                        CHECK(((unsigned char *)common_map)[i] == 0,
                              "COMMON mapping was not zeroed");
            }
        } else {
            CHECK(registrations == 0 && owner.gc.loaded_objects == NULL,
                  "allocation failure registered the staged object");
            CHECK(!oc->resolution_failed, "pre-backend allocation failure quarantined object");
        }
        void *saved_common = common_map;
        void *saved_got = got_map;
        if (failure == RESOURCE_NONE) ll_discard_object(oc);
        else discard_primary(oc, true);
        CHECK(!mapping_live(saved_common) && !mapping_live(saved_got),
              "actual disposal retained COMMON/GOT mappings");
        CHECK(lookupStrHashTable(owner.symhash, "elf_resource_shared") == NULL,
              "discard retained the staged object's symbol");
    }
    finish_owner();
}

static void check_overflow(const char *path)
{
    begin_owner();
    int loaded = 0;
    ObjectCode *oc = load_staged(path, 0, RESOURCE_NONE, &loaded);
    if (oc != NULL) {
        CHECK(loaded == 0 && owner.error_status == LL_E_UNSUPPORTED,
              "COMMON representational overflow did not reject before mapping");
        CHECK(common_requests == 0 && common_map == NULL && got_map == NULL,
              "overflow reached COMMON/GOT mapping");
        discard_primary(oc, false);
    }
    finish_owner();
}

static void check_aliases(const char *normal, const char *same,
                           const char *smaller, const char *conflict)
{
    begin_owner();
    int loaded = 0;
    ObjectCode *provider = load_staged(normal, 64, RESOURCE_NONE, &loaded);
    if (provider == NULL || !loaded) {
        if (provider != NULL) ll_discard_object(provider);
        finish_owner();
        return;
    }
    publish(provider);
    RtsSymbolInfo *symbol = lookupStrHashTable(owner.symhash, "elf_resource_shared");
    CHECK(symbol != NULL && symbol->owner == provider, "COMMON provider missing");
    if (symbol == NULL) {
        dispose_registered();
        finish_owner();
        return;
    }
    void *address = symbol->value;
    void *provider_mapping = common_map;
    uint64_t value = UINT64_C(0x123456789abcdef0);
    memcpy(address, &value, sizeof(value));
    const char *paths[] = {same, smaller};
    for (size_t i = 0; i < 2; ++i) {
        ObjectCode *alias = load_staged(paths[i], 0, RESOURCE_NONE, &loaded);
        if (alias == NULL) continue;
        CHECK(loaded == 1 && common_map == NULL, "alias allocated replacement COMMON storage");
        if (!loaded) { ll_discard_object(alias); continue; }
        publish(alias);
        ElfSymbolTable *table = alias->info->symbolTables;
        CHECK(table != NULL && table->n_symbols == 2,
              "alias fixture symbol table differs");
        if (table != NULL && table->n_symbols == 2)
            CHECK(table->symbols[1].addr == address,
                  "alias symbol did not retain provider address");
        ll_word key = 0;
        CHECK(keyCountHashTable((HashTable *)alias->dependencies) == 1
                  && keysHashTable((HashTable *)alias->dependencies, &key, 1) == 1
                  && key == (ll_word)provider,
              "COMMON alias omitted existing provider lifetime dependency");
        RtsSymbolInfo *retained = lookupStrHashTable(owner.symhash, "elf_resource_shared");
        uint64_t actual = 0;
        memcpy(&actual, address, sizeof(actual));
        CHECK(retained != NULL && retained->owner == provider
                  && retained->value == address && actual == value,
              "same/smaller alias changed provider address/value");
    }
    unsigned before = registrations;
    ObjectCode *bad = load_staged(conflict, 32, RESOURCE_NONE, &loaded);
    if (bad != NULL) {
        CHECK(loaded == 0 && owner.error_status == LL_E_DUPSYM,
              "larger COMMON conflict did not return DUPSYM");
        void *conflict_mapping = common_map;
        discard_primary(bad, true);
        CHECK(registrations == before && !mapping_live(conflict_mapping)
                  && lookupStrHashTable(owner.symhash, "elf_resource_unique") == NULL,
              "conflict retained new storage/publication/registration");
    }
    CHECK(unloadObj_(&owner, provider->fileName, false) == 1,
          "logical provider unload failed");
    CHECK(mapping_live(provider_mapping), "logical unload freed live alias storage");
    uint64_t actual = 0;
    memcpy(&actual, address, sizeof(actual));
    CHECK(actual == value, "logical provider unload damaged alias data");
    dispose_registered();
    CHECK(!mapping_live(provider_mapping), "actual provider disposal retained mapping");
    finish_owner();
}

static void check_zero(const char *path)
{
    begin_owner();
    int loaded = 0;
    ObjectCode *oc = load_staged(path, 0, RESOURCE_NONE, &loaded);
    if (oc != NULL) {
        RtsSymbolInfo *s = lookupStrHashTable(owner.symhash, "elf_resource_zero");
        CHECK(loaded == 1 && s != NULL && s->value == oc->image
                  && common_map == NULL && got_map != NULL,
              "zero COMMON did not use its owning image without separate storage");
        ll_discard_object(oc);
    }
    finish_owner();
}

static void check_quarantined_alias(const char *normal, const char *alias_path)
{
    begin_owner();
    int loaded = 0;
    ObjectCode *provider = load_staged(normal, 64, RESOURCE_NONE, &loaded);
    if (provider == NULL || !loaded) {
        if (provider != NULL) ll_discard_object(provider);
        finish_owner();
        return;
    }
    publish(provider);
    ll_error_set(&owner, LL_E_IO, "terminal provider model");
    ll_failure_resolution(provider);
    CHECK(provider->resolution_failed, "provider failure model did not quarantine");
    ObjectCode *alias = load_staged(alias_path, 0, RESOURCE_NONE, &loaded);
    if (alias != NULL) {
        CHECK(loaded == 0 && owner.error_status == LL_E_STATE
                  && alias->resolution_failed,
              "quarantined COMMON provider did not reject alias with STATE");
        ll_discard_object(alias);
        CHECK(registrations == 2 && owner.gc.objects == alias,
              "quarantined alias did not retain its approved object lifetime");
        ll_mutex_lock_for(&owner, &owner.linker_mutex);
        int resolved = resolveObjs_(&owner);
        ll_mutex_unlock_for(&owner, &owner.linker_mutex);
        CHECK(resolved == 0 && owner.error_status == LL_E_STATE,
              "global resolution admitted retained failed COMMON objects");
    }
    dispose_registered();
    finish_owner();
}

static void fixture_path(char *out, size_t capacity, const char *dir, const char *name)
{
    int n = snprintf(out, capacity, "%s/%s", dir, name);
    if (n < 0 || (size_t)n >= capacity) _Exit(98);
}

int main(int argc, char **argv)
{
    if (argc != 2) return 2;
    ll_compat_host_ops ops = host_ops(&default_host);
    ll_install_host(&ops, NULL);
    ll_error_set(&ll_default_linker, LL_E_STATE, "resource default sentinel");
    char normal[1024], same[1024], smaller[1024], conflict[1024];
    char zero[1024], got[1024], sum[1024], rounded[1024];
    fixture_path(normal, sizeof(normal), argv[1], "shared64.o");
    fixture_path(same, sizeof(same), argv[1], "same.o");
    fixture_path(smaller, sizeof(smaller), argv[1], "shared32.o");
    fixture_path(conflict, sizeof(conflict), argv[1], "conflict.o");
    fixture_path(zero, sizeof(zero), argv[1], "zero.o");
    fixture_path(got, sizeof(got), argv[1], "got-only.o");
    fixture_path(sum, sizeof(sum), argv[1], "sum-overflow.o");
    fixture_path(rounded, sizeof(rounded), argv[1], "round-overflow.o");
    check_mapping_case(normal, 64, RESOURCE_NONE);
    check_mapping_case(normal, 64, RESOURCE_COMMON);
    check_mapping_case(got, 0, RESOURCE_NONE);
    check_mapping_case(got, 0, RESOURCE_GOT);
    check_aliases(normal, same, smaller, conflict);
    check_quarantined_alias(normal, smaller);
    check_zero(zero);
    check_overflow(sum);
    check_overflow(rounded);
    if (failures != 0) return 1;
    (void)puts("ELF COMMON/GOT resource policies: PASS");
    return 0;
}
