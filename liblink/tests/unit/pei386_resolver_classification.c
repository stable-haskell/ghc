#define LL_INTERNAL_SOURCE 1
#include <windows.h>
#include <stdint.h>
#include <string.h>

static BOOLEAN NTAPI test_add_function_table(PRUNTIME_FUNCTION table,
                                              DWORD count, DWORD64 base);
static BOOLEAN NTAPI test_delete_function_table(PRUNTIME_FUNCTION table);
#define PEI386_TEST_RTLADD test_add_function_table
#define PEI386_TEST_RTLDELETE test_delete_function_table
#define main pei386_common_ownership_main
#include "pei386_common_ownership.c"
#undef main
#undef PEI386_TEST_RTLDELETE
#undef PEI386_TEST_RTLADD

static unsigned add_calls;
static PRUNTIME_FUNCTION added_table;
static unsigned delete_calls;
static PRUNTIME_FUNCTION deleted_table;

static BOOLEAN NTAPI test_add_function_table(PRUNTIME_FUNCTION table,
                                              DWORD count, DWORD64 base)
{
    (void)count;
    (void)base;
    ++add_calls;
    added_table = table;
    SetLastError(ERROR_ACCESS_DENIED);
    return FALSE;
}

static BOOLEAN NTAPI test_delete_function_table(PRUNTIME_FUNCTION table)
{
    ++delete_calls;
    deleted_table = table;
    return FALSE;
}

typedef enum {
    CASE_INVALID_STATIC_SECTION,
    CASE_SECREL_RANGE,
    CASE_UNSUPPORTED_RELOCATION,
    CASE_ADD_FUNCTION_TABLE,
    CASE_VALID_SECREL,
    CASE_VALID_REL32
} ResolverCase;

static ll_status expected_status(ResolverCase which)
{
    switch (which) {
    case CASE_INVALID_STATIC_SECTION: return LL_E_FORMAT;
    case CASE_SECREL_RANGE: return LL_E_RELOC_RANGE;
    case CASE_UNSUPPORTED_RELOCATION: return LL_E_UNSUPPORTED;
    case CASE_ADD_FUNCTION_TABLE: return LL_E_IO;
    case CASE_VALID_SECREL:
    case CASE_VALID_REL32: return LL_OK;
    }
    return LL_E_FORMAT;
}

static const char *expected_diagnostic(ResolverCase which)
{
    switch (which) {
    case CASE_INVALID_STATIC_SECTION: return "has invalid section";
    case CASE_SECREL_RANGE: return "IMAGE_REL_AMD64_SECREL";
    case CASE_UNSUPPORTED_RELOCATION: return "unhandled PE relocation";
    case CASE_ADD_FUNCTION_TABLE: return "Win32 error 5";
    case CASE_VALID_SECREL:
    case CASE_VALID_REL32: return "";
    }
    return "";
}

static void run_case(ResolverCase which, int16_t static_section)
{
    ObjectCode oc = {0};
    Section sections[3] = {0};
    const bool valid = which == CASE_VALID_REL32 || which == CASE_VALID_SECREL;
    uint32_t slot = valid ? 0 : UINT32_C(0x2468ace0);
    const uint32_t original = slot;
    const bool secrel = which == CASE_SECREL_RANGE || which == CASE_VALID_SECREL;
    const unsigned section_count = secrel ? 2 : 1;
    const bool has_relocation = which != CASE_ADD_FUNCTION_TABLE;
    const size_t baseline = fixture.allocation_count;

    oc.L = &owner;
    oc.fileName = (pathchar *)L"resolver-classification";
    oc.n_sections = section_count + 1; /* final slot is synthetic global data */
    oc.sections = sections;
    oc.info = LL_CALLOC_FOR(&owner, sizeof(*oc.info), 1, "resolver fixture info");
    oc.info->ch_info = LL_CALLOC_FOR(&owner, sizeof(*oc.info->ch_info), 1,
                                     "resolver fixture COFF header");
    oc.info->symbols = LL_CALLOC_FOR(&owner, sizeof(*oc.info->symbols), 1,
                                     "resolver fixture symbol");
    COFF_HEADER_INFO *header = oc.info->ch_info;
    header->type = COFF_IMAGE;
    header->sizeOfHeader = sizeof(IMAGE_FILE_HEADER);
    header->numberOfSections = section_count;
    header->numberOfSymbols = 1;

    for (unsigned i = 0; i < section_count; ++i) {
        sections[i].info = LL_CALLOC_FOR(&owner, sizeof(*sections[i].info), 1,
                                         "resolver fixture section info");
        sections[i].kind = SECTIONKIND_RWDATA;
    }
    sections[0].start = &slot;
    initProddableBlockSet(&oc.proddables);
    if (has_relocation) {
        sections[0].size = sizeof(slot);
        addProddableBlock(&owner, &oc.proddables, &slot, sizeof(slot));
        sections[0].info->relocs = LL_CALLOC_FOR(
            &owner, sizeof(*sections[0].info->relocs), 1,
            "resolver fixture relocation");
        sections[0].info->noRelocs = 1;
        COFF_reloc *reloc = sections[0].info->relocs;
        reloc->VirtualAddress = 0;
        reloc->SymbolTableIndex = 0;
        reloc->Type = secrel ? 11
                    : which == CASE_UNSUPPORTED_RELOCATION ? 0xffff : 4;

        COFF_symbol *symbol = &oc.info->symbols[0];
        memcpy(symbol->og.N.ShortName, "target", sizeof("target"));
        symbol->og.StorageClass = IMAGE_SYM_CLASS_STATIC;
        symbol->og.Type = 0;
        symbol->og.Value = which == CASE_VALID_REL32 ? 16 : 0;
        symbol->og.SectionNumber = which == CASE_INVALID_STATIC_SECTION
                                 ? static_section : secrel ? 2 : 1;
        if (secrel)
            sections[1].start = (void *)((uintptr_t)&slot +
                (uintptr_t)UINT32_MAX + (which == CASE_SECREL_RANGE ? 1u : 0u));
    } else {
        sections[0].kind = SECTIONKIND_EXCEPTION_TABLE;
        sections[0].start = (void *)(uintptr_t)UINT64_C(0x12345000);
        sections[0].size = sizeof(RUNTIME_FUNCTION);
        sections[0].info->name = LL_CALLOC_FOR(&owner, 6, 1,
                                                "resolver fixture section name");
        memcpy(sections[0].info->name, "pdata", 6);
    }

    owner.error_status = LL_OK;
    owner.error_message[0] = '\0';
    add_calls = 0;
    added_table = NULL;
    delete_calls = 0;
    deleted_table = NULL;
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    bool resolved = ocResolve_PEi386(&oc);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);

    CHECK(resolved == valid,
          "resolver returned the wrong success state");
    CHECK(owner.error_status == expected_status(which),
          "resolver reported the wrong owner status");
    const char *diagnostic = expected_diagnostic(which);
    if (*diagnostic != '\0')
        CHECK(strstr(owner.error_message, diagnostic) != NULL,
              "resolver diagnostic did not identify the failing class");
    if (which == CASE_VALID_REL32)
        CHECK(slot == 12, "valid REL32 relocation did not write the expected value");
    else if (which == CASE_VALID_SECREL)
        CHECK(slot == UINT32_MAX, "valid SECREL boundary wrote the wrong value");
    else if (has_relocation)
        CHECK(slot == original, "failed relocation modified its slot");
    if (which == CASE_ADD_FUNCTION_TABLE) {
        CHECK(add_calls == 1 &&
                  added_table == (PRUNTIME_FUNCTION)sections[0].start,
              "exception registration did not call the intercepted API once");
        bool retained = oc.info != NULL;
        freePreloadObjectFile_PEi386(&oc);
        CHECK(oc.info == NULL,
              "physical disposal did not release the retained format record");
        CHECK(delete_calls == (retained ? 1u : 0u),
              "physical disposal used the wrong unwind unregister count");
        if (retained)
            CHECK(deleted_table == (PRUNTIME_FUNCTION)sections[0].start,
                  "physical disposal unregistered another exception table");
    } else {
        CHECK(add_calls == 0 && delete_calls == 0,
              "non-exception case called an unwind registration API");
    }
    CHECK(ll_default_linker.error_status == LL_E_IO &&
              strcmp(ll_default_linker.error_message,
                     "resolver classification default sentinel") == 0,
          "resolver case changed the default linker's error state");

    freeProddableBlocks(&owner, &oc.proddables);
    releaseOcInfo(&oc);
    CHECK(fixture.allocation_count == baseline,
          "resolver case did not release its per-object host allocations");
}

#ifndef PEI386_CLASSIFICATION_ENTRY
#define PEI386_CLASSIFICATION_ENTRY main
#endif
int PEI386_CLASSIFICATION_ENTRY(void)
{
    install_owner();
    ll_default_linker.error_status = LL_E_IO;
    int written = snprintf(ll_default_linker.error_message,
                           sizeof(ll_default_linker.error_message),
                           "resolver classification default sentinel");
    CHECK(written == (int)strlen("resolver classification default sentinel") &&
              (size_t)written < sizeof(ll_default_linker.error_message),
          "could not set default linker sentinel");

    const int16_t rejected_sections[] = {0, -1, -2, 2};
    for (size_t i = 0; i < sizeof(rejected_sections) / sizeof(rejected_sections[0]); ++i)
        run_case(CASE_INVALID_STATIC_SECTION, rejected_sections[i]);
    run_case(CASE_VALID_SECREL, 0);
    run_case(CASE_SECREL_RANGE, 0);
    run_case(CASE_UNSUPPORTED_RELOCATION, 0);
    run_case(CASE_ADD_FUNCTION_TABLE, 0);
    run_case(CASE_VALID_REL32, 0);

    CHECK(fixture.fatal_calls == 0, "resolver called the fatal callback");
    ll_exit_linker(&owner);
    release_test_linker_auxiliary_state();
    CHECK(fixture.allocation_count == 0,
          "resolver classification unit left host allocations live after cleanup");
    while (fixture.allocation_count != 0) {
        void *allocation = fixture.allocations[--fixture.allocation_count];
        free(allocation);
    }
    if (failures != 0) return 1;
    (void)puts("pei386_resolver_classification: PASS");
    return 0;
}
