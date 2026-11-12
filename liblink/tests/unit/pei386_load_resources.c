#define LL_INTERNAL_SOURCE 1
#include <windows.h>
#include <limits.h>
#include "LinkerInternals.h"
#include "linker/M32Alloc.h"

static unsigned resource_alloc_calls;
static unsigned resource_fail_call = UINT_MAX;
static bool resource_set_specific_error;
static void *resource_m32_alloc_for(ll_linker *, m32_allocator *, size_t,
                                    size_t);

#define PEI386_TEST_M32_ALLOC resource_m32_alloc_for
#define main pei386_common_ownership_main
#include "pei386_common_ownership.c"
#undef main
#undef PEI386_TEST_M32_ALLOC

static void *resource_m32_alloc_for(ll_linker *linker,
                                    m32_allocator *allocator, size_t size,
                                    size_t alignment)
{
    unsigned call = resource_alloc_calls++;
    if (call == resource_fail_call) {
        if (resource_set_specific_error)
            ll_error_set(linker, LL_E_IO, "specific M32 resource error");
        return NULL;
    }
    return m32_alloc_for(linker, allocator, size, alignment);
}

static ll_status default_status;
static char default_message[sizeof(ll_default_linker.error_message)];

static void set_default_sentinel(void)
{
    int written;
    default_status = ll_default_linker.error_status;
    memcpy(default_message, ll_default_linker.error_message,
           sizeof(default_message));
    ll_default_linker.error_status = LL_E_IO;
    written = snprintf(ll_default_linker.error_message,
                       sizeof(ll_default_linker.error_message),
                       "default linker sentinel");
    CHECK(written >= 0 && (size_t)written <
              sizeof(ll_default_linker.error_message) &&
              (size_t)written == strlen("default linker sentinel"),
          "could not set bounded default linker sentinel");
}

static void check_default_sentinel(void)
{
    CHECK(ll_default_linker.error_status == LL_E_IO,
          "private load failure changed default linker status");
    CHECK(strcmp(ll_default_linker.error_message,
                 "default linker sentinel") == 0,
          "private load failure changed default linker message");
}

static void restore_default_error(void)
{
    ll_default_linker.error_status = default_status;
    memcpy(ll_default_linker.error_message, default_message,
           sizeof(default_message));
}

static bool make_named_object(pathchar *path, const char *name)
{
    path[0] = L'\0';
    if (!make_temp_path(path, MAX_PATH)) return false;
    if (write_object_file(path, 32, true, name)) return true;
    (void)DeleteFileW(path);
    return false;
}

static bool load_named_object(pathchar *path, const char *name)
{
    if (!make_named_object(path, name)) {
        CHECK(false, "could not create bounded PE resource fixture");
        return false;
    }
    ll_error_clear(&owner);
    return loadObj_(&owner, path) != 0;
}

static bool retry_existing_object(pathchar *path)
{
    ll_error_clear(&owner);
    return loadObj_(&owner, path) != 0;
}

static void check_provider_marker(void *provider_bss)
{
    CHECK(provider_bss != NULL, "resolved provider COMMON address is missing");
    if (provider_bss == NULL) return;
    for (size_t i = 0; i < 32; ++i)
        CHECK(((const uint8_t *)provider_bss)[i] == 0xa6,
              "resource failure/retry changed provider COMMON bytes");
}

static void check_failed_load_clean(pathchar *path, const char *name,
                                    size_t host_allocations,
                                    ObjectCode *loaded_head,
                                    ObjectCode *object_head,
                                    bool retain_fixture_file)
{
    CHECK(lookupObjectByPath(&owner, path) == NULL,
          "failed load left an ObjectCode lookup entry");
    CHECK(owner.gc.loaded_objects == loaded_head,
          "failed load changed the loaded-object list");
    CHECK(owner.gc.objects == object_head,
          "failed load changed the registered-object list");
    CHECK(lookupStrHashTable(owner.symhash, (ll_symbol_name *)name) == NULL,
          "failed load left its COMMON symbol registered");
    CHECK(fixture.allocation_count == host_allocations,
          "failed load left host-ledger allocations after loadObj_ cleanup");
    CHECK(fixture.fatal_calls == 0,
          "resource failure called the linker fatal callback");
    check_default_sentinel();
    if (!retain_fixture_file) (void)DeleteFileW(path);
}

static void test_section_failure_case(ObjectCode *provider, void *provider_bss,
                                      pathchar *path, bool specific_error,
                                      bool retain_fixture_file)
{
    const size_t allocations = fixture.allocation_count;
    ObjectCode *loaded_head = owner.gc.loaded_objects;
    ObjectCode *object_head = owner.gc.objects;

    resource_alloc_calls = 0;
    resource_fail_call = 0;
    resource_set_specific_error = specific_error;
    CHECK(!load_named_object(path, "failsec"),
          "first section allocation failure unexpectedly loaded the object");
    CHECK(resource_alloc_calls == 1,
          "first-section failure did not stop at its first M32 request");
    if (specific_error) {
        CHECK(owner.error_status == LL_E_IO &&
                  strcmp(owner.error_message, "specific M32 resource error") == 0,
              "section allocation failure replaced a specific owner error");
    } else {
        CHECK(owner.error_status == LL_E_NOMEM,
              "section allocation failure did not report LL_E_NOMEM");
        CHECK(owner.error_message[0] != '\0',
              "section allocation failure omitted its diagnostic");
    }
    check_failed_load_clean(path, "failsec", allocations,
                            loaded_head, object_head, retain_fixture_file);
    check_provider_marker(provider_bss);
    RtsSymbolInfo *kept = lookupStrHashTable(
        owner.symhash, (ll_symbol_name *)"keepdat");
    CHECK(kept != NULL && kept->owner == provider,
          "section failure disturbed the resolved provider symbol");
}

static void test_section_retry(ObjectCode *provider, void *provider_bss,
                               pathchar *path)
{
    resource_fail_call = UINT_MAX;
    resource_alloc_calls = 0;
    resource_set_specific_error = false;
    CHECK(GetFileAttributesW(path) != INVALID_FILE_ATTRIBUTES,
          "failed section fixture file did not remain available for retry");
    CHECK(retry_existing_object(path),
          "same-path retry after section allocation failure did not load");
    CHECK(resource_alloc_calls >= 2,
          "successful retry did not allocate its section and COMMON BSS");
    ll_error_clear(&owner);
    CHECK(resolveObjs_(&owner) != 0,
          "successful section retry did not resolve");
    RtsSymbolInfo *symbol = lookupStrHashTable(
        owner.symhash, (ll_symbol_name *)"failsec");
    CHECK(symbol != NULL && symbol->owner != provider,
          "successful retry did not register its independent COMMON symbol");
    CHECK(take_object_for_free(path),
          "successful section retry did not dispose cleanly");
    (void)DeleteFileW(path);
    CHECK(lookupStrHashTable(owner.symhash,
                             (ll_symbol_name *)"failsec") == NULL,
          "disposing retry retained its COMMON symbol");
    check_provider_marker(provider_bss);
    RtsSymbolInfo *kept = lookupStrHashTable(
        owner.symhash, (ll_symbol_name *)"keepdat");
    CHECK(kept != NULL && kept->owner == provider && kept->value == provider_bss,
          "successful retry/disposal disturbed the resolved provider");
}

static void test_bss_failure_case(ObjectCode *provider, void *provider_bss,
                                  bool specific_error)
{
    pathchar path[MAX_PATH];
    const size_t allocations = fixture.allocation_count;
    ObjectCode *loaded_head = owner.gc.loaded_objects;
    ObjectCode *object_head = owner.gc.objects;

    resource_alloc_calls = 0;
    resource_fail_call = 1;
    resource_set_specific_error = specific_error;
    CHECK(!load_named_object(path, "failbss"),
          "global BSS allocation failure unexpectedly loaded the object");
    CHECK(resource_alloc_calls == 2,
          "global BSS failure did not follow a successful section allocation");
    if (specific_error) {
        CHECK(owner.error_status == LL_E_IO &&
                  strcmp(owner.error_message, "specific M32 resource error") == 0,
              "global BSS failure replaced a specific owner error");
    } else {
        CHECK(owner.error_status == LL_E_NOMEM,
              "global BSS failure did not report LL_E_NOMEM");
        CHECK(owner.error_message[0] != '\0',
              "global BSS failure omitted its diagnostic");
    }
    check_failed_load_clean(path, "failbss", allocations,
                            loaded_head, object_head, false);
    CHECK(lookupObjectByPath(&owner, provider->fileName) == provider,
          "failed dependent load disturbed the resolved provider");
    RtsSymbolInfo *kept = lookupStrHashTable(
        owner.symhash, (ll_symbol_name *)"keepdat");
    CHECK(kept != NULL && kept->owner == provider && kept->value == provider_bss,
          "failed dependent load changed the provider COMMON symbol");
    check_provider_marker(provider_bss);
    resource_fail_call = UINT_MAX;
    resource_set_specific_error = false;
}

/* Failed loadObj_ cleanup is checked against the live host-ledger baseline in
 * each failure case above. The only residue counted here comes from the two
 * successful objects' existing normal PE disposal path; the harness releases
 * those known metadata allocations after recording their exact counts. */
static void check_known_metadata_residue(void)
{
    size_t string_tables = 0, section_names = 0, relocations = 0;
    for (size_t i = 0; i < fixture.allocation_count; ++i) {
        const char *tag = fixture.allocation_tags[i];
        if (strcmp(tag, "ocVerifyImage_PEi386(oc->info->str_tab)") == 0)
            ++string_tables;
        else if (strcmp(tag, "cstring_from_section_symbol_name") == 0)
            ++section_names;
        else if (strcmp(tag, "ocVerifyImage_PEi386(section->info->relocs)") == 0)
            ++relocations;
        else
            CHECK(false, "unexpected host allocation remained after PE resource tests");
    }
    CHECK(string_tables == 0 && section_names == 0 && relocations == 0,
          "resource tests left unexpected known PE metadata residue counts");
    while (fixture.allocation_count != 0) {
        size_t i = --fixture.allocation_count;
        free(fixture.allocations[i]);
        fixture.allocations[i] = NULL;
        fixture.allocation_tags[i] = NULL;
    }
}

int main(void)
{
    pathchar provider_path[MAX_PATH];
    pathchar section_retry_path[MAX_PATH];
    install_owner();
    set_default_sentinel();
    resource_alloc_calls = 0;
    resource_fail_call = UINT_MAX;
    resource_set_specific_error = false;
    CHECK(load_named_object(provider_path, "keepdat"),
          "normal provider object did not load");
    ll_error_clear(&owner);
    CHECK(resolveObjs_(&owner) != 0,
          "normal provider object did not resolve");
    ObjectCode *provider = lookupObjectByPath(&owner, provider_path);
    RtsSymbolInfo *provider_symbol = lookupStrHashTable(
        owner.symhash, (ll_symbol_name *)"keepdat");
    CHECK(provider != NULL && provider->status == OBJECT_READY,
          "normal provider did not reach OBJECT_READY");
    CHECK(provider_symbol != NULL && provider_symbol->owner == provider,
          "normal provider COMMON symbol was not registered");
    void *provider_bss = provider_symbol == NULL ? NULL : provider_symbol->value;
    CHECK(provider_symbol != NULL && provider_symbol->size == 32 &&
              provider_bss != NULL,
          "normal provider COMMON allocation has the wrong size or address");
    if (provider_bss != NULL) {
        for (size_t i = 0; i < 32; ++i)
            CHECK(((uint8_t *)provider_bss)[i] == 0,
                  "normal provider COMMON storage was not zero-filled");
        if (provider != NULL && provider->n_sections > 0 &&
            provider->sections[0].start != NULL) {
            uint64_t relocation = 0;
            memcpy(&relocation, provider->sections[0].start,
                   sizeof(relocation));
            CHECK(relocation == (uint64_t)(uintptr_t)provider_bss + 4,
                  "normal provider relocation did not target COMMON storage");
        }
    }
    fixture.common_bss = provider_bss;
    if (provider != NULL && provider_bss != NULL) {
        memset(provider_bss, 0xa6, 32);
        test_section_failure_case(provider, provider_bss,
                                  section_retry_path, false, false);
        test_section_failure_case(provider, provider_bss,
                                  section_retry_path, true, true);
        test_section_retry(provider, provider_bss, section_retry_path);
        test_bss_failure_case(provider, provider_bss, false);
        test_bss_failure_case(provider, provider_bss, true);
        CHECK(lookupObjectByPath(&owner, provider_path) == provider,
              "resource failure/retry changed provider object identity");
        CHECK(provider_symbol->value == provider_bss,
              "resource failure/retry changed provider COMMON address");
        check_provider_marker(provider_bss);
        CHECK(take_object_for_free(provider_path),
              "normal provider disposal failed");
        (void)DeleteFileW(provider_path);
    }
    CHECK(fixture.common_bss_host_frees == 0,
          "PE COMMON storage was passed to host_free during disposal");
    CHECK(fixture.fatal_calls == 0,
          "PE load resource test invoked the fatal callback");
    ll_exit_linker(&owner);
    release_test_linker_auxiliary_state();
    check_known_metadata_residue();
    CHECK(fixture.allocation_count == 0,
          "PE resource test host ledger was not empty after cleanup");
    restore_default_error();
    if (failures != 0) return 1;
    (void)puts("pei386_load_resources: PASS");
    return 0;
}
