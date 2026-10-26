#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"

static bool quarantine_resolve(ObjectCode *object);
#define PEI386_TEST_RESOLVE quarantine_resolve
#define PEI386_CLASSIFICATION_ENTRY pei386_resolver_classification_main
#include "pei386_resolver_classification.c"
#undef PEI386_CLASSIFICATION_ENTRY
#undef PEI386_TEST_RESOLVE

static unsigned backend_calls;

static bool quarantine_resolve(ObjectCode *object)
{
    ++backend_calls;
    /* Count an erroneous dispatch without entering released metadata. */
    if (backend_calls != 1) {
        CHECK(false, "core dispatched the backend more than once");
        return false;
    }
    return ocResolve_PEi386(object);
}

static ObjectCode *make_core_fixture(ResolverCase which, uint32_t *slot)
{
    ObjectCode *object = mkOc(&owner, STATIC_OBJECT,
                              (pathchar *)L"resolver-quarantine",
                              NULL, 0, false, NULL, 0);
    object->format_ops = ll_format_native();
    object->n_sections = 2; /* final slot is synthetic global data */
    object->sections = LL_CALLOC_FOR(&owner, 2, sizeof(*object->sections),
                                     "quarantine fixture sections");
    object->info = LL_CALLOC_FOR(&owner, 1, sizeof(*object->info),
                                 "quarantine fixture info");
    object->info->ch_info = LL_CALLOC_FOR(
        &owner, 1, sizeof(*object->info->ch_info), "quarantine fixture header");
    object->info->symbols = LL_CALLOC_FOR(
        &owner, 1, sizeof(*object->info->symbols), "quarantine fixture symbol");
    COFF_HEADER_INFO *header = object->info->ch_info;
    header->type = COFF_IMAGE;
    header->sizeOfHeader = sizeof(IMAGE_FILE_HEADER);
    header->numberOfSections = 1;
    header->numberOfSymbols = 1;

    Section *section = &object->sections[0];
    section->info = LL_CALLOC_FOR(&owner, 1, sizeof(*section->info),
                                  "quarantine fixture section info");
    section->alloc = SECTION_NOMEM; /* borrowed fixture data */
    if (which == CASE_ADD_FUNCTION_TABLE) {
        section->kind = SECTIONKIND_EXCEPTION_TABLE;
        section->start = (void *)(uintptr_t)UINT64_C(0x12345000);
        section->size = sizeof(RUNTIME_FUNCTION);
        section->info->name = LL_CALLOC_FOR(&owner, 6, 1,
                                            "quarantine fixture section name");
        memcpy(section->info->name, "pdata", 6);
    } else {
        section->kind = SECTIONKIND_RWDATA;
        section->start = slot;
        section->size = sizeof(*slot);
        addProddableBlock(&owner, &object->proddables, slot, sizeof(*slot));
        section->info->relocs = LL_CALLOC_FOR(
            &owner, 1, sizeof(*section->info->relocs),
            "quarantine fixture relocation");
        section->info->noRelocs = 1;
        section->info->relocs[0].Type =
            which == CASE_VALID_REL32 ? 4 : 0xffff;
        COFF_symbol *symbol = &object->info->symbols[0];
        memcpy(symbol->og.N.ShortName, "target", sizeof("target"));
        symbol->og.StorageClass = IMAGE_SYM_CLASS_STATIC;
        symbol->og.Value = which == CASE_VALID_REL32 ? 16 : 0;
        symbol->og.SectionNumber = 1;
    }
    return object;
}

static void run_core_case(ResolverCase which)
{
    const size_t baseline = fixture.allocation_count;
    uint32_t slot = which == CASE_VALID_REL32 ? 0 : UINT32_C(0x2468ace0);
    const uint32_t original = slot;
    ObjectCode *object = make_core_fixture(which, &slot);
    const bool expected_ok = which == CASE_VALID_REL32;
    backend_calls = 0;
    add_calls = delete_calls = 0;
    added_table = deleted_table = NULL;
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    const int first = ocTryLoad(object);
    CHECK((first != 0) == expected_ok, "core returned the wrong first result");
    CHECK(owner.error_status == expected_status(which),
          "core replaced the first specific backend status");
    CHECK(strstr(owner.error_message, expected_diagnostic(which)) != NULL,
          "core lost the first specific backend diagnostic");
    CHECK(backend_calls == 1, "core omitted or repeated the first dispatch");
    if (expected_ok) {
        CHECK(object->status == OBJECT_RESOLVED && !object->resolution_failed,
              "valid REL32 object did not become independently resolved");
        CHECK(slot == 12, "core valid REL32 control wrote the wrong value");
        CHECK(ocTryLoad(object) != 0 && owner.error_status == LL_OK,
              "reusing a resolved healthy object failed");
    } else {
        CHECK(object->resolution_failed, "terminal PE failure was not quarantined");
        CHECK(slot == original, "failed core resolution changed its slot");
        /* An uncorrected baseline fails above without redispatching metadata
         * that releaseOcInfo has already removed. */
        if (object->resolution_failed) {
            CHECK(ocTryLoad(object) == 0 && owner.error_status == LL_E_STATE,
                  "quarantined PE object remained retryable");
            CHECK(slot == original, "rejected retry changed its slot");
        }
    }
    CHECK(backend_calls == 1, "core re-entered the PE backend on a later call");
    CHECK(delete_calls == 0, "unwind record was released before disposal");
    if (which == CASE_ADD_FUNCTION_TABLE)
        CHECK(add_calls == 1, "core registration control omitted the API call");
    else
        CHECK(add_calls == 0, "ordinary core case called exception registration");
    if (expected_ok) {
        /* The existing successful PE disposal path does not release copied
         * relocation arrays. Release this fixture-owned array explicitly;
         * failed cases must reach their baseline through product disposal. */
        LL_FREE_FOR(&owner, object->sections[0].info->relocs);
        object->sections[0].info->relocs = NULL;
    }
    freeObjectCode(object);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    CHECK(delete_calls == (which == CASE_ADD_FUNCTION_TABLE ? 1u : 0u),
          "normal disposal used the wrong unwind-unregister count");
    if (which == CASE_ADD_FUNCTION_TABLE)
        CHECK(deleted_table == added_table,
              "normal disposal used another exception table");
    CHECK(fixture.allocation_count == baseline,
          "normal core object disposal leaked host allocations");
    CHECK(ll_default_linker.error_status == LL_E_IO &&
              strcmp(ll_default_linker.error_message,
                     "resolver quarantine default sentinel") == 0,
          "core case changed the default linker error");
}

int main(void)
{
    install_owner();
    ll_error_set(&ll_default_linker, LL_E_IO,
                 "resolver quarantine default sentinel");
    run_core_case(CASE_UNSUPPORTED_RELOCATION);
    run_core_case(CASE_ADD_FUNCTION_TABLE);
    run_core_case(CASE_VALID_REL32);
    CHECK(fixture.fatal_calls == 0, "core control called the fatal callback");
    ll_exit_linker(&owner);
    release_test_linker_auxiliary_state();
    CHECK(fixture.allocation_count == 0, "core control leaked fixture allocations");
    if (failures != 0) return 1;
    (void)puts("pei386_resolver_quarantine: PASS");
    return 0;
}
