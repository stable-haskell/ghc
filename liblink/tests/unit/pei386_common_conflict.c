/* Ordinary duplicate COMMON declarations exercise load failure and reuse. */
#define main pei386_common_ownership_main
#include "pei386_common_ownership.c"
#undef main

enum { PROVIDER_SIZE = 64, PREFIX_SIZE = 16 };

static ll_status saved_default_status;
static char saved_default_message[sizeof(ll_default_linker.error_message)];

/* Keep the string table DWORD aligned with either two or four symbols.
 * The optional independent COMMON precedes the conflicting declaration, so
 * failed-load cleanup must undo a symbol that was already published. */
static bool write_common_fixture(const pathchar *path, const char *name,
                                 uint32_t size, bool with_prefix)
{
    enum {
        RAW_OFFSET = sizeof(IMAGE_FILE_HEADER) + sizeof(IMAGE_SECTION_HEADER),
        RELOCATION_OFFSET = RAW_OFFSET + sizeof(uint64_t),
        SYMBOL_OFFSET = (RELOCATION_OFFSET + sizeof(IMAGE_RELOCATION) + 3) & ~3
    };
    const size_t name_length = strlen(name);
    const bool long_name = name_length > sizeof(((IMAGE_SYMBOL *)0)->N.ShortName);
    const DWORD symbol_count = with_prefix ? 4 : 2;
    const DWORD target_index = with_prefix ? 1 : 0;
    const size_t string_offset = SYMBOL_OFFSET
        + symbol_count * sizeof(IMAGE_SYMBOL);
    const size_t string_size = sizeof(DWORD) + (long_name ? name_length + 1 : 0);
    const size_t image_size = string_offset + string_size;
    uint8_t *image = calloc(1, image_size);
    if (image == NULL) return false;

    IMAGE_FILE_HEADER header = {0};
    header.Machine = IMAGE_FILE_MACHINE_AMD64;
    header.NumberOfSections = 1;
    header.PointerToSymbolTable = SYMBOL_OFFSET;
    header.NumberOfSymbols = symbol_count;
    memcpy(image, &header, sizeof(header));

    IMAGE_SECTION_HEADER section = {0};
    memcpy(section.Name, ".data", sizeof(".data") - 1);
    section.SizeOfRawData = sizeof(uint64_t);
    section.PointerToRawData = RAW_OFFSET;
    section.PointerToRelocations = RELOCATION_OFFSET;
    section.NumberOfRelocations = 1;
    section.Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA
        | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_8BYTES;
    memcpy(image + sizeof(header), &section, sizeof(section));

    const uint64_t addend = 4;
    memcpy(image + RAW_OFFSET, &addend, sizeof(addend));
    IMAGE_RELOCATION relocation = {0};
    relocation.SymbolTableIndex = target_index;
    relocation.Type = IMAGE_REL_AMD64_ADDR64;
    memcpy(image + RELOCATION_OFFSET, &relocation, sizeof(relocation));

    for (DWORD i = 0; i < symbol_count; ++i) {
        IMAGE_SYMBOL symbol = {0};
        if (with_prefix && i == 0) {
            memcpy(symbol.N.ShortName, "prefix1", sizeof("prefix1") - 1);
            symbol.Value = PREFIX_SIZE;
            symbol.SectionNumber = IMAGE_SYM_UNDEFINED;
            symbol.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
        } else if (i == target_index) {
            if (long_name) {
                symbol.N.Name.Short = 0;
                symbol.N.Name.Long = sizeof(DWORD);
            } else {
                memcpy(symbol.N.ShortName, name, name_length);
            }
            symbol.Value = size;
            symbol.SectionNumber = IMAGE_SYM_UNDEFINED;
            symbol.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
        } else {
            memcpy(symbol.N.ShortName, "absmeta", sizeof("absmeta") - 1);
            symbol.SectionNumber = IMAGE_SYM_ABSOLUTE;
            symbol.StorageClass = IMAGE_SYM_CLASS_STATIC;
        }
        memcpy(image + SYMBOL_OFFSET + i * sizeof(symbol), &symbol, sizeof(symbol));
    }
    const DWORD stored_string_size = (DWORD)string_size;
    memcpy(image + string_offset, &stored_string_size, sizeof(stored_string_size));
    if (long_name)
        memcpy(image + string_offset + sizeof(DWORD), name, name_length + 1);

    FILE *file = _wfopen(path, L"wb");
    bool written = file != NULL && fwrite(image, 1, image_size, file) == image_size;
    if (file != NULL && fclose(file) != 0) written = false;
    free(image);
    return written;
}

static RtsSymbolInfo *find_symbol(const char *name)
{
    return lookupStrHashTable(owner.symhash, (ll_symbol_name *)name);
}

static ObjectCode *load_existing_fixture(pathchar *path)
{
    ll_error_clear(&owner);
    if (loadObj_(&owner, path) == 0) {
        CHECK(false, "compatible COMMON fixture did not load");
        return NULL;
    }
    CHECK(owner.error_status == LL_OK, "successful COMMON load retained an error");
    ll_error_clear(&owner);
    CHECK(resolveObjs_(&owner) != 0, "compatible COMMON fixture did not resolve");
    CHECK(owner.error_status == LL_OK, "successful COMMON resolve retained an error");
    ObjectCode *oc = lookupObjectByPath(&owner, path);
    CHECK(oc != NULL && oc->status == OBJECT_READY,
          "compatible COMMON fixture did not reach OBJECT_READY");
    return oc;
}

static bool allocation_has_tag(void *pointer, const char *tag)
{
    for (size_t i = 0; i < fixture.allocation_count; ++i)
        if (fixture.allocations[i] == pointer)
            return strcmp(fixture.allocation_tags[i], tag) == 0;
    return false;
}

/* Normal successful PE disposal has three known copied-metadata residues for
 * these fixtures. Assert their exact ownership, dispose normally, and release
 * only those known residues as harness cleanup. Failed loads get no such help. */
static void dispose_success(pathchar *path)
{
    ObjectCode *oc = lookupObjectByPath(&owner, path);
    CHECK(oc != NULL && oc->info != NULL && oc->n_sections == 2 &&
              oc->sections[0].info != NULL,
          "successful COMMON fixture metadata was missing before disposal");
    if (oc == NULL || oc->info == NULL || oc->n_sections != 2 ||
        oc->sections[0].info == NULL) return;
    void *residue[] = { oc->info->str_tab, oc->sections[0].info->name,
                        oc->sections[0].info->relocs };
    const char *tags[] = { "ocVerifyImage_PEi386(oc->info->str_tab)",
                          "cstring_from_section_symbol_name",
                          "ocVerifyImage_PEi386(section->info->relocs)" };
    CHECK(take_object_for_free(path), "successful COMMON fixture disposal failed");
    for (size_t i = 0; i < sizeof(residue) / sizeof(residue[0]); ++i) {
        bool owned = allocation_has_tag(residue[i], tags[i]);
        CHECK(owned, "successful disposal left a different PE metadata residue");
        if (owned) host_free(&owner, residue[i]);
    }
    (void)DeleteFileW(path);
}

static bool dependency_is_provider(void *data, ll_word key, const void *value)
{
    ObjectCode **expected = data;
    (void)value;
    if ((ObjectCode *)(uintptr_t)key == *expected) *expected = NULL;
    return true;
}

static void check_provider(const char *name, ObjectCode *provider, void *address)
{
    RtsSymbolInfo *symbol = find_symbol(name);
    CHECK(symbol != NULL && symbol->owner == provider &&
              symbol->value == address && symbol->size == PROVIDER_SIZE,
          "COMMON conflict/reuse changed provider ownership, size, or address");
    CHECK(provider->status == OBJECT_READY && !provider->resolution_failed,
          "COMMON conflict/reuse quarantined the healthy provider");
    CHECK(lookupObjectByPath(&owner, provider->fileName) == provider,
          "COMMON conflict/reuse changed provider registration");
    for (size_t i = 0; i < PROVIDER_SIZE; ++i)
        CHECK(((const uint8_t *)address)[i] == 0xa6,
              "COMMON conflict/reuse changed provider bytes");
}

static void check_consumer(ObjectCode *consumer, ObjectCode *provider,
                           void *address, bool with_prefix)
{
    CHECK(consumer != NULL, "compatible consumer was missing");
    if (consumer == NULL) return;
    ObjectCode *expected = provider;
    iterHashTable(consumer->dependencies, &expected, dependency_is_provider);
    CHECK(expected == NULL, "COMMON reuse omitted its provider dependency");
    uint64_t relocation = 0;
    memcpy(&relocation, consumer->sections[0].start, sizeof(relocation));
    CHECK(relocation == (uint64_t)(uintptr_t)address + 4,
          "COMMON consumer relocation did not reuse provider storage");
    Section *bss = &consumer->sections[consumer->n_sections - 1];
    CHECK(bss->size == (with_prefix ? PREFIX_SIZE : 0),
          "COMMON reuse allocated replacement storage for the provider symbol");
    CHECK(with_prefix ? bss->alloc == SECTION_M32
                      : bss->alloc == SECTION_NOMEM && bss->start == NULL,
          "COMMON consumer BSS ownership changed");
    if (with_prefix) {
        RtsSymbolInfo *prefix = find_symbol("prefix1");
        CHECK(prefix != NULL && prefix->owner == consumer &&
                  prefix->size == PREFIX_SIZE && prefix->value == bss->start,
              "successful retry did not publish its independent prefix COMMON");
    }
}

static void check_default_sentinel(void)
{
    CHECK(ll_default_linker.error_status == LL_E_IO &&
              strcmp(ll_default_linker.error_message, "COMMON default sentinel") == 0,
          "private COMMON load changed the default linker error");
}

static void test_reuse(const char *name, ObjectCode *provider, void *address,
                       uint32_t size)
{
    pathchar path[MAX_PATH];
    if (!make_temp_path(path, MAX_PATH)) {
        CHECK(false, "could not create compatible COMMON path");
        return;
    }
    if (!write_common_fixture(path, name, size, false)) {
        CHECK(false, "could not write compatible COMMON fixture");
        (void)DeleteFileW(path);
        return;
    }
    const size_t baseline = fixture.allocation_count;
    ObjectCode *consumer = load_existing_fixture(path);
    check_consumer(consumer, provider, address, false);
    check_provider(name, provider, address);
    if (consumer != NULL) dispose_success(path);
    else (void)DeleteFileW(path);
    CHECK(fixture.allocation_count == baseline,
          "compatible COMMON reuse left unexpected host allocations");
    check_default_sentinel();
}

static void test_conflict_and_retry(const char *name, ObjectCode *provider,
                                    void *address, bool with_prefix)
{
    pathchar path[MAX_PATH];
    if (!make_temp_path(path, MAX_PATH)) {
        CHECK(false, "could not create conflicting COMMON path");
        return;
    }
    if (!write_common_fixture(path, name, PROVIDER_SIZE + 1, with_prefix)) {
        CHECK(false, "could not write conflicting COMMON fixture");
        (void)DeleteFileW(path);
        return;
    }
    const size_t baseline = fixture.allocation_count;
    ObjectCode *loaded_head = owner.gc.loaded_objects;
    ObjectCode *object_head = owner.gc.objects;
    char expected[256];
    int written = snprintf(expected, sizeof(expected),
        "linker: trying to link COMMON symbols %s with incompatible sizes: "
        "previous size %u, new size %u\n", name,
        (unsigned)PROVIDER_SIZE, (unsigned)PROVIDER_SIZE + 1);
    CHECK(written >= 0 && (size_t)written < sizeof(expected),
          "conflict diagnostic expectation truncated");
    ll_error_clear(&owner);
    int loaded = loadObj_(&owner, path);
    CHECK(loaded == 0, "larger COMMON declaration unexpectedly loaded");
    CHECK(owner.error_status == LL_E_DUPSYM,
          "larger COMMON declaration did not report LL_E_DUPSYM");
    CHECK(strcmp(owner.error_message, expected) == 0,
          "COMMON size conflict changed its precise diagnostic");
    CHECK(fixture.fatal_calls == 0, "COMMON size conflict called fatal");
    CHECK(lookupObjectByPath(&owner, path) == NULL,
          "rejected COMMON object remained registered by path");
    CHECK(owner.gc.loaded_objects == loaded_head && owner.gc.objects == object_head,
          "rejected COMMON object changed the loaded/registered object lists");
    CHECK(find_symbol("prefix1") == NULL,
          "failed COMMON load retained an earlier published prefix symbol");
    CHECK(fixture.allocation_count == baseline,
          "failed COMMON load did not restore its host allocation ledger");
    check_provider(name, provider, address);
    check_default_sentinel();
    if (loaded != 0) {
        dispose_success(path);
        return;
    }

    CHECK(write_common_fixture(path, name, PROVIDER_SIZE / 2, with_prefix),
          "could not replace rejected file with a compatible COMMON declaration");
    ObjectCode *consumer = load_existing_fixture(path);
    check_consumer(consumer, provider, address, with_prefix);
    check_provider(name, provider, address);
    if (consumer != NULL) dispose_success(path);
    else (void)DeleteFileW(path);
    CHECK(find_symbol("prefix1") == NULL,
          "disposing compatible retry retained its independent prefix symbol");
    CHECK(fixture.allocation_count == baseline,
          "same-path COMMON retry/disposal left unexpected host allocations");
    check_provider(name, provider, address);
    check_default_sentinel();
}

static void test_name_form(const char *name)
{
    pathchar path[MAX_PATH];
    if (!make_temp_path(path, MAX_PATH)) {
        CHECK(false, "could not create COMMON provider path");
        return;
    }
    if (!write_common_fixture(path, name, PROVIDER_SIZE, false)) {
        CHECK(false, "could not write COMMON provider fixture");
        (void)DeleteFileW(path);
        return;
    }
    ObjectCode *provider = load_existing_fixture(path);
    RtsSymbolInfo *symbol = find_symbol(name);
    CHECK(provider != NULL && symbol != NULL && symbol->owner == provider &&
              symbol->value != NULL && symbol->size == PROVIDER_SIZE,
          "COMMON provider was missing its expected storage");
    if (provider == NULL || symbol == NULL || symbol->value == NULL) {
        if (provider != NULL) dispose_success(path);
        else (void)DeleteFileW(path);
        return;
    }
    void *address = symbol->value;
    fixture.common_bss = address;
    for (size_t i = 0; i < PROVIDER_SIZE; ++i)
        CHECK(((const uint8_t *)address)[i] == 0,
              "COMMON provider storage was not initially zero-filled");
    memset(address, 0xa6, PROVIDER_SIZE);
    test_reuse(name, provider, address, PROVIDER_SIZE);
    test_reuse(name, provider, address, PROVIDER_SIZE / 2);
    test_conflict_and_retry(name, provider, address, false);
    test_conflict_and_retry(name, provider, address, true);
    dispose_success(path);
    fixture.common_bss = NULL;
    CHECK(find_symbol(name) == NULL, "disposed provider retained its COMMON symbol");
}

int main(void)
{
    install_owner();
    saved_default_status = ll_default_linker.error_status;
    memcpy(saved_default_message, ll_default_linker.error_message,
           sizeof(saved_default_message));
    ll_default_linker.error_status = LL_E_IO;
    int written = snprintf(ll_default_linker.error_message,
                           sizeof(ll_default_linker.error_message),
                           "COMMON default sentinel");
    CHECK(written >= 0 && (size_t)written < sizeof(ll_default_linker.error_message),
          "default COMMON sentinel truncated");

    test_name_form("common1");
    test_name_form("common08");
    test_name_form("common_conflict_long");
    CHECK(fixture.fatal_calls == 0, "COMMON controls invoked the fatal callback");
    CHECK(fixture.common_bss_host_frees == 0,
          "COMMON storage reached host_free during conflict/reuse/disposal");
    check_default_sentinel();
    ghciRemoveSymbolTable(&owner, (ll_symbol_name *)"__image_base__", NULL);
    ll_exit_linker(&owner);
    release_test_linker_auxiliary_state();
    CHECK(owner.m32_reservations == NULL && owner.m32_free_page_pool == NULL &&
              owner.m32_free_page_pool_size == 0,
          "COMMON controls retained an M32 reservation or pooled page");
    CHECK(fixture.allocation_count == 0,
          "COMMON controls left host allocations after explicit fixture cleanup");
    ll_default_linker.error_status = saved_default_status;
    memcpy(ll_default_linker.error_message, saved_default_message,
           sizeof(saved_default_message));
    if (failures != 0) return 1;
    (void)puts("pei386_common_conflict: PASS (3 name forms, 6 conflicts, 6 retries)");
    return 0;
}
