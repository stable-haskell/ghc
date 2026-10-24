#define LL_INTERNAL_SOURCE 1
#include <windows.h>
#include <inttypes.h>
#include "LinkerInternals.h"
#include "linker/M32Alloc.h"

static bool fail_m32_mapping;
static bool range_fake_mapping;
static bool range_tracking;
static unsigned range_allocations;
static bool mock_proc_addresses;
static HMODULE first_module;
static HMODULE second_module;
static unsigned first_module_lookups;
static unsigned second_module_lookups;
static ll_status saved_default_status;
static char saved_default_message[sizeof(ll_default_linker.error_message)];

static void *resolution_m32_alloc_for(ll_linker *, m32_allocator *, size_t,
                                      size_t);
static FARPROC WINAPI resolution_get_proc_address(HMODULE, LPCSTR);

#define PEI386_TEST_M32_ALLOC resolution_m32_alloc_for
#define PEI386_TEST_GETPROC resolution_get_proc_address
/* Reuse the native Windows fixture's host ledger and actual source inclusions. */
#define main pei386_common_ownership_main
#include "pei386_common_ownership.c"
#undef main
#undef PEI386_TEST_GETPROC
#undef PEI386_TEST_M32_ALLOC
#undef CHECK
#define CHECK(condition, ...)                                                \
    do {                                                                     \
        if (!(condition)) {                                                  \
            fprintf(stderr, "FAIL: ");                                      \
            fprintf(stderr, __VA_ARGS__);                                    \
            fputc('\n', stderr);                                             \
            failures++;                                                      \
        }                                                                    \
    } while (0)

static struct {
    SymbolExtra extra;
    uint32_t slot;
} range_storage;

static void set_owner_message(const char *message)
{
    int written = snprintf(owner.error_message, sizeof(owner.error_message),
                           "%s", message);
    CHECK(written >= 0 && (size_t)written < sizeof(owner.error_message) &&
              (size_t)written == strlen(message),
          "could not set bounded owner error message");
}

static void set_default_sentinel(void)
{
    saved_default_status = ll_default_linker.error_status;
    memcpy(saved_default_message, ll_default_linker.error_message,
           sizeof(saved_default_message));
    ll_default_linker.error_status = LL_E_IO;
    int written = snprintf(ll_default_linker.error_message,
                           sizeof(ll_default_linker.error_message),
                           "default linker sentinel");
    CHECK(written == (int)strlen("default linker sentinel") &&
              (size_t)written < sizeof(ll_default_linker.error_message),
          "could not set bounded default linker sentinel");
}

static void check_default_sentinel(void)
{
    CHECK(ll_default_linker.error_status == LL_E_IO,
          "owner failure changed default linker status");
    CHECK(strcmp(ll_default_linker.error_message,
                 "default linker sentinel") == 0,
          "owner failure changed default linker message");
}

static void restore_default_error(void)
{
    ll_default_linker.error_status = saved_default_status;
    memcpy(ll_default_linker.error_message, saved_default_message,
           sizeof(saved_default_message));
}

static void *resolution_m32_alloc_for(ll_linker *linker,
                                      m32_allocator *allocator, size_t size,
                                      size_t alignment)
{
    if (range_tracking) ++range_allocations;
    if (range_fake_mapping) {
        CHECK(size == sizeof(SymbolExtra) && alignment == 8,
              "range trampoline requested unexpected M32 allocation");
        return &range_storage.extra;
    }
    if (fail_m32_mapping) return NULL;
    return m32_alloc_for(linker, allocator, size, alignment);
}

static FARPROC WINAPI resolution_get_proc_address(HMODULE module,
                                                   LPCSTR name)
{
    if (!mock_proc_addresses)
        return GetProcAddress(module, name);
    if (module == first_module) ++first_module_lookups;
    if (module == second_module) ++second_module_lookups;
    if (strcmp(name, "__imp_probe") == 0) return NULL;
    if (strcmp(name, "probe") == 0)
        return GetProcAddress(GetModuleHandleW(L"kernel32.dll"),
                              "GetCurrentProcessId");
    return NULL;
}

static bool run_symbol_extra_cases(void)
{
    ObjectCode oc = {0};
    oc.L = &owner;
    oc.rw_m32 = m32_allocator_new_for(&owner, false);
    oc.rx_m32 = m32_allocator_new_for(&owner, true);
    CHECK(oc.rw_m32 != NULL && oc.rx_m32 != NULL,
          "could not create real PE symbol-extra allocators");
    if (oc.rw_m32 == NULL || oc.rx_m32 == NULL) {
        if (oc.rx_m32 != NULL) m32_allocator_free_for(&owner, oc.rx_m32);
        if (oc.rw_m32 != NULL) m32_allocator_free_for(&owner, oc.rw_m32);
        return false;
    }

    size_t result = 0;
    const size_t target = (size_t)UINT64_C(0x123456789abcdef0);
    bool made = makeSymbolExtra_PEi386(&oc, 0, target, "code", LL_SYM_CODE,
                                       &result);
    CHECK(made,
          "code symbol extra was not allocated");
    static const uint8_t expected_jump[6] =
        {0xff, 0x25, 0xf2, 0xff, 0xff, 0xff};
    if (made) {
        CHECK(result != 0 && memcmp((void *)result, expected_jump,
                                    sizeof(expected_jump)) == 0,
              "code symbol extra has incorrect jump bytes");
        if (result != 0)
            CHECK(((SymbolExtra *)((uint8_t *)result
                      - offsetof(SymbolExtra, jumpIsland)))->addr == target,
                  "code symbol extra did not retain the exact target address");
    }

    void *indirect_target = (void *)(uintptr_t)UINT64_C(0x12345678);
    made = makeSymbolExtra_PEi386(&oc, 1,
                                  (size_t)(uintptr_t)&indirect_target,
                                  "indirect", LL_SYM_INDIRECT_DATA, &result);
    CHECK(made,
          "indirect-data symbol extra was not allocated");
    if (made) {
        CHECK(result != 0, "indirect-data symbol extra returned a null slot");
        if (result != 0)
            CHECK(*(void **)result == indirect_target,
                  "indirect-data symbol extra did not copy the pointed-to address");
    }

    made = makeSymbolExtra_PEi386(&oc, 2, target, "data", LL_SYM_DATA,
                                  &result);
    CHECK(made,
          "data symbol extra was not allocated");
    if (made) {
        CHECK(result != 0, "data symbol extra returned a null slot");
        if (result != 0)
            CHECK(*(uint64_t *)result == (uint64_t)target,
                  "data symbol extra did not retain the exact address");
    }

    owner.error_status = LL_OK;
    owner.error_message[0] = '\0';
    result = 0x5a5a;
    fail_m32_mapping = true;
    CHECK(!makeSymbolExtra_PEi386(&oc, 3, target, "failed",
                                  LL_SYM_DATA, &result),
          "M32 mapping failure unexpectedly returned a symbol extra");
    fail_m32_mapping = false;
    CHECK(owner.error_status == LL_E_NOMEM,
          "M32 symbol-extra failure did not report NOMEM on its owner");
    CHECK(result == 0x5a5a,
          "failed symbol-extra allocation changed caller output");
    CHECK(owner.error_status == LL_E_NOMEM,
          "symbol-extra failure status was lost from its owner");
    CHECK(fixture.fatal_calls == 0,
          "symbol-extra failure called the fatal callback");
    check_default_sentinel();

    owner.error_status = LL_E_UNSUPPORTED;
    set_owner_message("preexisting symbol-extra error");
    result = 0x6b6b;
    fail_m32_mapping = true;
    CHECK(!makeSymbolExtra_PEi386(&oc, 4, target, "preserve",
                                  LL_SYM_DATA, &result),
          "M32 failure unexpectedly returned a symbol extra");
    fail_m32_mapping = false;
    CHECK(owner.error_status == LL_E_UNSUPPORTED &&
              strcmp(owner.error_message, "preexisting symbol-extra error") == 0,
          "symbol-extra mapping failure replaced a specific owner error");
    CHECK(result == 0x6b6b,
          "specific symbol-extra failure changed caller output");
    check_default_sentinel();
    owner.error_status = LL_OK;
    owner.error_message[0] = '\0';

    m32_allocator_free_for(&owner, oc.rx_m32);
    m32_allocator_free_for(&owner, oc.rw_m32);
    return true;
}

static void run_dll_fallback_stops_on_resource_error(void)
{
    ObjectCode dependent = {0};
    OpenedDLL second = {0};
    OpenedDLL first = {0};
    ll_symbol_name *symbol = (ll_symbol_name *)"__imp_probe";
    dependent.L = &owner;
    dependent.rw_m32 = m32_allocator_new_for(&owner, false);
    CHECK(dependent.rw_m32 != NULL,
          "could not create DLL-indirection M32 allocator");
    if (dependent.rw_m32 == NULL) return;

    first_module = (HMODULE)(uintptr_t)1;
    second_module = (HMODULE)(uintptr_t)2;
    first.instance = first_module;
    first.next = &second;
    second.instance = second_module;
    OpenedDLL *saved_dlls = owner.opened_dlls;
    owner.opened_dlls = &first;
    mock_proc_addresses = true;
    fail_m32_mapping = true;

    owner.error_status = LL_OK;
    owner.error_message[0] = '\0';
    first_module_lookups = 0;
    second_module_lookups = 0;
    CHECK(lookupSymbolInDLLs(&owner, symbol, &dependent) == NULL,
          "DLL indirection mapping failure unexpectedly resolved");
    CHECK(owner.error_status == LL_E_NOMEM,
          "DLL indirection failure did not report owner NOMEM");
    CHECK(first_module_lookups == 2 && second_module_lookups == 0,
          "DLL fallback did not stop after its resource failure");
    check_default_sentinel();

    /* A specific prior error must survive the same failed mapping. */
    first_module_lookups = 0;
    second_module_lookups = 0;
    owner.error_status = LL_E_UNSUPPORTED;
    set_owner_message("preexisting DLL error");
    CHECK(lookupSymbolInDLLs(&owner, symbol, &dependent) == NULL,
          "DLL indirection mapping failure unexpectedly resolved");
    fail_m32_mapping = false;
    mock_proc_addresses = false;
    owner.opened_dlls = saved_dlls;
    CHECK(owner.error_status == LL_E_UNSUPPORTED &&
              strcmp(owner.error_message, "preexisting DLL error") == 0,
          "DLL indirection failure replaced its owner's specific status");
    CHECK(first_module_lookups == 2,
          "first DLL did not perform import and fallback lookup");
    CHECK(second_module_lookups == 0,
          "DLL fallback continued after a resource failure");
    check_default_sentinel();
    owner.error_status = LL_OK;
    owner.error_message[0] = '\0';
    m32_allocator_free_for(&owner, dependent.rw_m32);
}

static void run_relocation_case(uint16_t type, uint32_t addend,
                                uintptr_t target_address, bool expect_success,
                                uint32_t expected_slot, const char *label)
{
    ObjectCode oc = {0};
    COFF_HEADER_INFO *header;
    COFF_symbol *symbols;
    Section sections[2] = {0};
    struct SectionFormatInfo *reloc_info;
    COFF_reloc *relocs;
    const uint32_t original = addend;

    range_storage.slot = original;
    memset(&range_storage.extra, 0, sizeof(range_storage.extra));
    oc.L = &owner;
    oc.info = LL_CALLOC_FOR(&owner, sizeof(*oc.info), 1, "range fixture info");
    header = LL_CALLOC_FOR(&owner, sizeof(*header), 1, "range fixture header");
    symbols = LL_CALLOC_FOR(&owner, sizeof(*symbols), 1, "range fixture symbol");
    reloc_info = LL_CALLOC_FOR(&owner, sizeof(*reloc_info), 1, "range fixture section");
    relocs = LL_CALLOC_FOR(&owner, sizeof(*relocs), 1, "range fixture relocation");
    if (oc.info == NULL || header == NULL || symbols == NULL ||
        reloc_info == NULL || relocs == NULL) {
        CHECK(false, "host fixture allocator returned NULL");
        return;
    }
    oc.info->ch_info = header;
    oc.info->symbols = symbols;
    header->type = COFF_IMAGE;
    header->sizeOfHeader = sizeof(IMAGE_FILE_HEADER);
    header->numberOfSections = 1;
    header->numberOfSymbols = 1;
    symbols[0].og.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
    symbols[0].og.SectionNumber = IMAGE_SYM_UNDEFINED;
    memcpy(symbols[0].og.N.ShortName, "target", sizeof("target"));
    sections[0].start = &range_storage.slot;
    sections[0].kind = SECTIONKIND_RWDATA;
    sections[0].info = reloc_info;
    reloc_info->relocs = relocs;
    reloc_info->noRelocs = 1;
    relocs[0].VirtualAddress = 0;
    relocs[0].SymbolTableIndex = 0;
    relocs[0].Type = type;
    oc.sections = sections;
    oc.n_sections = 2; /* section 1 is the synthetic global-data slot */
    oc.fileName = (pathchar *)L"post-trampoline-range";
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(&owner, &oc.proddables, &range_storage.slot,
                      sizeof(range_storage.slot));
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    bool registered = ghciInsertSymbolTable(
        &owner, (pathchar *)L"range fixture", (ll_symbol_name *)"target",
        (ll_symbol_addr *)target_address,
        false, LL_SYM_CODE, 0, NULL);
    CHECK(registered, "could not register range fixture target");
    if (registered) {
        owner.error_status = LL_OK;
        owner.error_message[0] = '\0';
        range_allocations = 0;
        range_tracking = true;
        range_fake_mapping = true;
        bool resolved = ocResolve_PEi386(&oc);
        range_fake_mapping = false;
        range_tracking = false;
        CHECK(resolved == expect_success, "%s returned unexpected resolve result", label);
        if (expect_success) {
            CHECK(owner.error_status == LL_OK, "%s set error status %d", label,
                  (int)owner.error_status);
            CHECK(range_storage.slot == expected_slot,
                  "%s wrote 0x%08" PRIx32 " instead of 0x%08" PRIx32,
                  label, range_storage.slot, expected_slot);
            CHECK(range_allocations == 0,
                  "%s unexpectedly allocated a trampoline", label);
        } else {
            CHECK(owner.error_status == LL_E_RELOC_RANGE,
                  "%s did not report LL_E_RELOC_RANGE", label);
            const char *diagnostic = type == 4
                ? "IMAGE_REL_AMD64_REL32: High bits are set"
                : "IMAGE_REL_AMD64_ADDR32[NB]: High bits are set";
            CHECK(strstr(owner.error_message, diagnostic) != NULL,
                  "%s did not retain its range diagnostic", label);
            CHECK(range_storage.slot == original,
                  "%s changed relocation slot on range error", label);
            CHECK(range_allocations == 1,
                  "%s did not request exactly one trampoline", label);
        }
        ghciRemoveSymbolTable(&owner, (ll_symbol_name *)"target", NULL);
    }
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    freeProddableBlocks(&owner, &oc.proddables);
    releaseOcInfo(&oc);
}

static void run_post_trampoline_range_cases(void)
{
    struct { uint16_t type; uint32_t addend; const char *label; } cases[] = {
        {2, UINT32_C(0x80000000), "ADDR32"},
        {3, UINT32_C(0x80000000), "ADDR32NB"},
        {17, UINT32_C(0x80000000), "32S"},
        {4, UINT32_C(0x80000000), "REL32"},
    };
    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        uint32_t addend = cases[i].type == 4
            ? (uint32_t)INT32_MIN : cases[i].addend;
        run_relocation_case(cases[i].type, addend,
                            (uintptr_t)&range_storage.slot +
                                UINT64_C(0x200000000),
                            false, addend, cases[i].label);
    }
    run_relocation_case(4, 0, (uintptr_t)&range_storage.slot + 16,
                        true, 12, "in-range REL32 control");
}

/* The new unit is intentionally data-only; executable code bytes are inspected,
 * never called. The separate pdata test is wired into the same focused mode. */
int main(void)
{
    install_owner();
    set_default_sentinel();
    (void)run_symbol_extra_cases();
    run_dll_fallback_stops_on_resource_error();
    run_post_trampoline_range_cases();
    CHECK(fixture.fatal_calls == 0, "linker fatal callback was called");
    check_default_sentinel();
    ll_exit_linker(&owner);
    release_test_linker_auxiliary_state();
    CHECK(fixture.allocation_count == 0,
          "native resolution unit left host allocations live after cleanup");
    restore_default_error();
    while (fixture.allocation_count != 0) {
        void *allocation = fixture.allocations[--fixture.allocation_count];
        free(allocation);
    }
    if (failures != 0) return 1;
    (void)puts("pei386_resolution_status: PASS");
    return 0;
}
