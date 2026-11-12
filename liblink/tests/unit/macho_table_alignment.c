#define main macho_input_original_main
#include "macho_input.c"
#undef main

typedef struct {
    unsigned char *bytes;
    size_t size;
} Image;

static Image append_symbols(const unsigned char *source, size_t size)
{
    MachOHeader *header;
    MachOSymtabCommand *symtab;
    MachOSegmentCommand *segment;
    if (!image_commands((unsigned char *)source, size, &header, &symtab, &segment)
        || symtab->nsyms == 0) return (Image){0};

    size_t bytes = (size_t)symtab->nsyms * sizeof(MachONList);
    size_t offset = size;
    while (offset % _Alignof(MachONList) == 0) ++offset;
    if (offset > SIZE_MAX - bytes) return (Image){0};
    size_t total = offset + bytes;
    unsigned char *copy = calloc(total, 1);
    if (copy == NULL) return (Image){0};
    memcpy(copy, source, size);
    memcpy(copy + offset, source + symtab->symoff, bytes);

    MachOHeader *copy_header;
    MachOSymtabCommand *copy_symtab;
    MachOSegmentCommand *copy_segment;
    if (!image_commands(copy, total, &copy_header, &copy_symtab, &copy_segment)) {
        free(copy);
        return (Image){0};
    }
    (void)header;
    (void)segment;
    (void)copy_header;
    (void)copy_segment;
    copy_symtab->symoff = (uint32_t)offset;
    return (Image){copy, total};
}

static Image append_relocations(const unsigned char *source, size_t size)
{
    MachOHeader *header;
    MachOSymtabCommand *symtab;
    MachOSegmentCommand *segment;
    if (!image_commands((unsigned char *)source, size, &header, &symtab, &segment))
        return (Image){0};
    MachOSection *sections = (MachOSection *)(segment + 1);
    size_t total = size;
    for (uint32_t i = 0; i < segment->nsects; ++i) {
        if (sections[i].nreloc == 0) continue;
        size_t bytes = (size_t)sections[i].nreloc * sizeof(MachORelocationInfo);
        if (bytes > SIZE_MAX - total - _Alignof(MachORelocationInfo))
            return (Image){0};
        total += bytes + _Alignof(MachORelocationInfo);
    }
    uint32_t *old_offsets = calloc(segment->nsects == 0 ? 1 : segment->nsects,
                                   sizeof(*old_offsets));
    if (old_offsets == NULL) return (Image){0};
    for (uint32_t i = 0; i < segment->nsects; ++i)
        old_offsets[i] = sections[i].reloff;

    unsigned char *copy = calloc(total, 1);
    if (copy == NULL) {
        free(old_offsets);
        return (Image){0};
    }
    memcpy(copy, source, size);
    MachOHeader *copy_header;
    MachOSymtabCommand *copy_symtab;
    MachOSegmentCommand *copy_segment;
    if (!image_commands(copy, total, &copy_header, &copy_symtab, &copy_segment)) {
        free(old_offsets);
        free(copy);
        return (Image){0};
    }
    (void)header;
    (void)symtab;
    (void)copy_header;
    (void)copy_symtab;
    MachOSection *copy_sections = (MachOSection *)(copy_segment + 1);
    size_t position = size;
    for (uint32_t i = 0; i < copy_segment->nsects; ++i) {
        if (copy_sections[i].nreloc == 0) continue;
        while (position % _Alignof(MachORelocationInfo) == 0) ++position;
        size_t bytes = (size_t)copy_sections[i].nreloc
                     * sizeof(MachORelocationInfo);
        uint32_t old_offset = old_offsets[i];
        if ((size_t)old_offset > size || bytes > size - (size_t)old_offset
            || position > total || bytes > total - position
            || position > UINT32_MAX) {
            free(old_offsets);
            free(copy);
            return (Image){0};
        }
        memcpy(copy + position, source + old_offset, bytes);
        copy_sections[i].reloff = (uint32_t)position;
        position += bytes;
    }
    free(old_offsets);
    return (Image){copy, position};
}

static bool symbol_offset_is_aligned(const unsigned char *bytes, size_t size,
                                     bool aligned)
{
    MachOHeader *header;
    MachOSymtabCommand *symtab;
    MachOSegmentCommand *segment;
    if (!image_commands((unsigned char *)bytes, size, &header, &symtab, &segment))
        return false;
    (void)header;
    (void)segment;
    return ((size_t)symtab->symoff % _Alignof(MachONList) == 0) == aligned;
}

static bool relocation_offsets_are_aligned(const unsigned char *bytes, size_t size,
                                           bool aligned)
{
    MachOHeader *header;
    MachOSymtabCommand *symtab;
    MachOSegmentCommand *segment;
    if (!image_commands((unsigned char *)bytes, size, &header, &symtab, &segment))
        return false;
    (void)header;
    (void)symtab;
    MachOSection *sections = (MachOSection *)(segment + 1);
    bool found = false;
    for (uint32_t i = 0; i < segment->nsects; ++i) {
        if (sections[i].nreloc == 0) continue;
        found = true;
        if (((size_t)sections[i].reloff
             % _Alignof(MachORelocationInfo) == 0) != aligned) return false;
    }
    return found;
}

static bool clear_relocations(unsigned char *bytes, size_t size)
{
    MachOHeader *header;
    MachOSymtabCommand *symtab;
    MachOSegmentCommand *segment;
    if (!image_commands(bytes, size, &header, &symtab, &segment)) return false;
    (void)header;
    (void)symtab;
    MachOSection *sections = (MachOSection *)(segment + 1);
    for (uint32_t i = 0; i < segment->nsects; ++i) sections[i].nreloc = 0;
    return true;
}

static bool check_relocation_copy(ObjectCode *oc, bool expected_owned,
                                  bool expect_any)
{
    bool found = false;
    for (int i = 0; i < oc->n_sections; ++i) {
        Section *section = &oc->sections[i];
        MachOSection *macho = section->info == NULL
            ? NULL : section->info->macho_section;
        if (macho == NULL) continue;
        if (macho->nreloc == 0) {
            CHECK(section->info->owned_relocations == NULL,
                  "zero relocation count acquired an owned table");
            continue;
        }
        found = true;
        CHECK((section->info->owned_relocations != NULL) == expected_owned,
              "relocation copy ownership does not match input alignment");
        CHECK(section->info->relocation_info != NULL,
              "relocation table pointer missing after initialization");
        const void *input_table = oc->image + macho->reloff;
        CHECK(((const void *)section->info->relocation_info != input_table)
                  == expected_owned,
              "relocation table borrowed/copied pointer provenance is wrong");
        CHECK((uintptr_t)section->info->relocation_info
                  % _Alignof(MachORelocationInfo) == 0,
              "active relocation table pointer is not naturally aligned");
    }
    return found == expect_any;
}

static void check_resolved_data(ObjectCode *oc, bool expect_pointer_relocation)
{
    RtsSymbolInfo *value = lookupStrHashTable(owner.symhash, "_ll_table_value");
    RtsSymbolInfo *pointer = lookupStrHashTable(owner.symhash, "_ll_table_pointer");
    RtsSymbolInfo *common = lookupStrHashTable(owner.symhash, "_ll_table_common");
    CHECK(value != NULL && pointer != NULL && common != NULL,
          "fixture data exports or COMMON symbol missing");
    if (value == NULL || pointer == NULL || common == NULL) return;
    CHECK(value->owner == oc && pointer->owner == oc && common->owner == oc,
          "fixture symbols were registered to the wrong object");
    CHECK(value->value != NULL && pointer->value != NULL && common->value != NULL,
          "loaded fixture data has no address");
    if (value->value == NULL || pointer->value == NULL || common->value == NULL)
        return;
    CHECK(*(int *)value->value == 17, "initialized fixture value changed");
    if (expect_pointer_relocation)
        CHECK(*(int **)pointer->value == (int *)value->value,
              "relocated pointer does not target loaded value");
    CHECK(*(int *)common->value == 0, "COMMON storage is not zero initialized");
}

static void loaded_alignment_case(const char *label, const unsigned char *source,
                                  size_t size, bool expect_symbol_copy,
                                  bool expect_relocation_copy, bool expect_relocations)
{
    Snapshot before = snapshot();
    unsigned char *image = mmap(NULL, size, PROT_READ | PROT_WRITE,
                                MAP_PRIVATE | MAP_ANON, -1, 0);
    CHECK(image != MAP_FAILED, "could not allocate mapped input image");
    if (image == MAP_FAILED) return;
    memcpy(image, source, size);
    ObjectCode *oc = mkOc(&owner, STATIC_OBJECT, (pathchar *)label, (char *)image,
                          (int)size, true, (pathchar *)"table_alignment_member.o", 0);
    CHECK(oc != NULL, "mkOc returned NULL");
    if (oc == NULL) {
        (void)munmap(image, size);
        return;
    }
    ll_error_clear(&owner);
    int initialized = ll_init_object(oc);
    if (initialized == 0 || owner.error_status != LL_OK)
        (void)fprintf(stderr, "%s: init status=%d diagnostic=%s\n", label,
                      (int)owner.error_status, owner.error_message);
    CHECK(initialized != 0 && owner.error_status == LL_OK, label);
    if (initialized) {
        CHECK((oc->info->owned_nlist != NULL) == expect_symbol_copy,
              "symbol table ownership does not match input alignment");
        CHECK(oc->info->nlist != NULL, "symbol table pointer missing");
        const void *input_symbols = oc->image + oc->info->symCmd->symoff;
        CHECK(((const void *)oc->info->nlist != input_symbols) == expect_symbol_copy,
              "symbol table borrowed/copied pointer provenance is wrong");
        CHECK((uintptr_t)oc->info->nlist % _Alignof(MachONList) == 0,
              "active symbol table pointer is not naturally aligned");
        CHECK(oc->status == OBJECT_LOADED,
              "archive member did not retain its real loaded status after init");
        int loaded = loadOc(oc);
        if (loaded == 0 || owner.error_status != LL_OK)
            (void)fprintf(stderr, "%s: load status=%d diagnostic=%s\n", label,
                          (int)owner.error_status, owner.error_message);
        CHECK(loaded != 0 && owner.error_status == LL_OK, label);
        if (loaded) {
            CHECK(check_relocation_copy(oc, expect_relocation_copy,
                                        expect_relocations),
                  "relocation table count differs from the fixture");
            /* Archive-member loading leaves this object loaded but unresolved. */
            CHECK(oc->status == OBJECT_LOADED,
                  "load changed archive member status before lookup");
            ll_mutex_lock(&owner.linker_mutex);
            ll_symbol_addr *trigger = lookupSymbol_(&owner, "_ll_table_value");
            ll_mutex_unlock(&owner.linker_mutex);
            if (trigger == NULL || owner.error_status != LL_OK)
                (void)fprintf(stderr, "lookup status=%d diagnostic=%s oc-status=%d\n",
                              (int)owner.error_status, owner.error_message,
                              (int)oc->status);
            CHECK(trigger != NULL && owner.error_status == LL_OK,
                  "lookup-triggered relocation resolution failed");
            if (trigger != NULL && owner.error_status == LL_OK) {
                CHECK(oc->status == OBJECT_RESOLVED,
                      "lookup did not advance object to resolved state");
                check_resolved_data(oc, expect_relocations);
            }
        }
    }
    CHECK(owner_host.fatals == 0, "unexpected fatal during aligned-table case");
    ll_discard_object(oc);
    CHECK(lookupStrHashTable(owner.symhash, "_ll_table_value") == NULL
              && lookupStrHashTable(owner.symhash, "_ll_table_pointer") == NULL
              && lookupStrHashTable(owner.symhash, "_ll_table_common") == NULL,
          "discard left fixture symbols in the owner hash");
    check_loaded_delta(before, label);
}

static void run_invalid_alignment_case(const char *label,
                                       const unsigned char *source, size_t size,
                                       Mutation mutation)
{
    run_case(label, source, size, mutation, false, false);
}

static void partial_verifier_failure_case(const unsigned char *source, size_t size)
{
    Snapshot before = snapshot();
    unsigned char *image = mmap(NULL, size, PROT_READ | PROT_WRITE,
                                MAP_PRIVATE | MAP_ANON, -1, 0);
    CHECK(image != MAP_FAILED, "could not allocate partial-verifier input image");
    if (image == MAP_FAILED) return;
    memcpy(image, source, size);
    ObjectCode *oc = mkOc(&owner, STATIC_OBJECT,
                          (pathchar *)"partial-verifier-failure",
                          (char *)image, (int)size, true,
                          (pathchar *)"table_alignment_member.o", 0);
    CHECK(oc != NULL, "mkOc returned NULL for partial-verifier failure");
    if (oc == NULL) {
        (void)munmap(image, size);
        return;
    }
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0 && owner.error_status == LL_OK,
          "packed valid input failed before partial-verifier case");
    CHECK(oc->info != NULL && oc->info->owned_nlist != NULL,
          "partial-verifier case did not establish owned symbol table first");
    if (oc->info != NULL && oc->info->owned_nlist != NULL) {
        CHECK(bad_symoff((unsigned char *)oc->image, (size_t)oc->fileSize),
              "could not mutate aligned symbol-table header after initialization");
        int loaded = loadOc(oc);
        CHECK(loaded == 0 && owner.error_status == LL_E_FORMAT,
              "load did not reject changed symbol-table span as LL_E_FORMAT");
    }
    ll_discard_object(oc);
    CHECK(lookupStrHashTable(owner.symhash, "_ll_table_value") == NULL
              && lookupStrHashTable(owner.symhash, "_ll_table_pointer") == NULL
              && lookupStrHashTable(owner.symhash, "_ll_table_common") == NULL,
          "partial verifier failure left fixture symbols in owner hash");
    check_delta(before, "partial verifier failure leaked owner allocations");
}

static void zero_metadata_count_case(const unsigned char *source, size_t size)
{
    Snapshot before = snapshot();
    unsigned char *image = mmap(NULL, size, PROT_READ | PROT_WRITE,
                                MAP_PRIVATE | MAP_ANON, -1, 0);
    CHECK(image != MAP_FAILED, "could not allocate zero-count input image");
    if (image == MAP_FAILED) return;
    memcpy(image, source, size);
    MachOHeader *header;
    MachOSymtabCommand *symtab;
    MachOSegmentCommand *segment;
    CHECK(image_commands(image, size, &header, &symtab, &segment),
          "could not inspect zero-count input image");
    if (!image_commands(image, size, &header, &symtab, &segment)) {
        (void)munmap(image, size);
        return;
    }
    (void)header;
    (void)segment;
    symtab->nsyms = 0;
    CHECK(clear_relocations(image, size), "could not clear zero-count relocations");
    ObjectCode *oc = mkOc(&owner, STATIC_OBJECT,
                          (pathchar *)"zero-symbol-and-relocation-count",
                          (char *)image, (int)size, true,
                          (pathchar *)"table_alignment_member.o", 0);
    CHECK(oc != NULL, "mkOc returned NULL for zero-count case");
    if (oc == NULL) {
        (void)munmap(image, size);
        return;
    }
    ll_error_clear(&owner);
    int initialized = ll_init_object(oc);
    CHECK(initialized != 0 && owner.error_status == LL_OK,
          "zero symbol and relocation counts did not initialize");
    if (initialized) {
        CHECK(oc->info->owned_nlist == NULL,
              "zero symbol count allocated an owned symbol table");
        int loaded = loadOc(oc);
        CHECK(loaded != 0 && owner.error_status == LL_OK,
              "zero symbol and relocation counts did not load");
        CHECK(oc->info->owned_nlist == NULL,
              "zero symbol count acquired an owned table during load");
        CHECK(check_relocation_copy(oc, false, false),
              "zero relocation count allocated or exposed relocation tables");
    }
    ll_discard_object(oc);
    CHECK(lookupStrHashTable(owner.symhash, "_ll_table_value") == NULL
              && lookupStrHashTable(owner.symhash, "_ll_table_pointer") == NULL
              && lookupStrHashTable(owner.symhash, "_ll_table_common") == NULL,
          "zero-count discard left fixture symbols in owner hash");
    check_loaded_delta(before, "zero-count cleanup retained unexpected owner allocations");
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        (void)fprintf(stderr, "usage: macho_table_alignment table-data.o\n");
        return 2;
    }
    ll_compat_host_ops default_ops = make_ops(&default_host);
    ll_compat_host_ops owner_ops = make_ops(&owner_host);
    ll_install_host_for(&ll_default_linker, &default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_sentinel, sizeof(default_sentinel));
    ll_init_linker(&owner, 0);
    owner.services.cfg.always_pic = true;

    size_t size = 0;
    unsigned char *raw = read_image(argv[1], &size);
    CHECK(raw != NULL, "could not read data-only table fixture");
    if (raw != NULL) {
        CHECK(symbol_offset_is_aligned(raw, size, true),
              "compiler fixture symbol table is not aligned control");
        CHECK(relocation_offsets_are_aligned(raw, size, true),
              "compiler fixture relocation table is not aligned control");
        loaded_alignment_case("aligned-tables-control", raw, size, false, false, true);

        Image symbols = append_symbols(raw, size);
        CHECK(symbols.bytes != NULL, "could not construct unaligned symbol table");
        if (symbols.bytes != NULL) {
            CHECK(symbol_offset_is_aligned(symbols.bytes, symbols.size, false),
                  "symbol-only repack did not make symbol table unaligned");
            CHECK(relocation_offsets_are_aligned(symbols.bytes, symbols.size, true),
                  "symbol-only repack changed relocation table alignment");
            loaded_alignment_case("symbol-table-only-packed", symbols.bytes,
                                  symbols.size, true, false, true);
            free(symbols.bytes);
        }

        Image relocations = append_relocations(raw, size);
        CHECK(relocations.bytes != NULL,
              "could not construct unaligned relocation table");
        if (relocations.bytes != NULL) {
            CHECK(symbol_offset_is_aligned(relocations.bytes, relocations.size, true),
                  "relocation-only repack changed symbol table alignment");
            CHECK(relocation_offsets_are_aligned(relocations.bytes,
                                                 relocations.size, false),
                  "relocation-only repack did not make tables unaligned");
            loaded_alignment_case("relocation-tables-only-packed", relocations.bytes,
                                  relocations.size, false, true, true);
            free(relocations.bytes);
        }

        unsigned char *bad_symbol = malloc(size);
        CHECK(bad_symbol != NULL, "could not allocate malformed symbol input");
        if (bad_symbol != NULL) {
            memcpy(bad_symbol, raw, size);
            CHECK(bad_symbol_section(bad_symbol, size),
                  "could not mutate aligned symbol table index");
            Image packed_bad_symbol = append_symbols(bad_symbol, size);
            CHECK(packed_bad_symbol.bytes != NULL,
                  "could not repack malformed symbol table");
            if (packed_bad_symbol.bytes != NULL) {
                run_invalid_alignment_case("packed-invalid-symbol-index-still-format",
                                           packed_bad_symbol.bytes,
                                           packed_bad_symbol.size, NULL);
                free(packed_bad_symbol.bytes);
            }
            free(bad_symbol);
        }

        unsigned char *bad_relocation = malloc(size);
        CHECK(bad_relocation != NULL, "could not allocate malformed relocation input");
        if (bad_relocation != NULL) {
            memcpy(bad_relocation, raw, size);
            CHECK(bad_reloc_symbol(bad_relocation, size),
                  "could not mutate aligned relocation symbol index");
            Image packed_bad_relocation = append_relocations(bad_relocation, size);
            CHECK(packed_bad_relocation.bytes != NULL,
                  "could not repack malformed relocation table");
            if (packed_bad_relocation.bytes != NULL) {
                run_invalid_alignment_case("packed-invalid-relocation-index-still-format",
                                           packed_bad_relocation.bytes,
                                           packed_bad_relocation.size, NULL);
                free(packed_bad_relocation.bytes);
            }
            free(bad_relocation);
        }

        Image both = append_symbols(raw, size);
        if (both.bytes != NULL) {
            Image next = append_relocations(both.bytes, both.size);
            free(both.bytes);
            both = next;
        }
        CHECK(both.bytes != NULL, "could not construct both packed tables");
        if (both.bytes != NULL) {
            CHECK(symbol_offset_is_aligned(both.bytes, both.size, false),
                  "combined repack did not make symbol table unaligned");
            CHECK(relocation_offsets_are_aligned(both.bytes, both.size, false),
                  "combined repack did not make relocation tables unaligned");
            loaded_alignment_case("both-table-kinds-packed", both.bytes, both.size,
                                  true, true, true);
            partial_verifier_failure_case(both.bytes, both.size);
            zero_metadata_count_case(both.bytes, both.size);
            free(both.bytes);
        }

        unsigned char *no_relocations = malloc(size);
        CHECK(no_relocations != NULL, "could not allocate zero-relocation control");
        if (no_relocations != NULL) {
            memcpy(no_relocations, raw, size);
            CHECK(clear_relocations(no_relocations, size),
                  "could not create zero-relocation control");
            loaded_alignment_case("zero-relocation-count-no-copy", no_relocations,
                                  size, false, false, false);
            free(no_relocations);
        }
        zero_metadata_count_case(raw, size);
        run_invalid_alignment_case("invalid-symbol-span-still-format", raw, size,
                                   bad_symoff);
        run_invalid_alignment_case("invalid-symbol-index-still-format", raw, size,
                                   bad_symbol_section);
        run_invalid_alignment_case("invalid-relocation-span-still-format", raw, size,
                                   bad_reloc_span);
        run_invalid_alignment_case("invalid-relocation-index-still-format", raw, size,
                                   bad_reloc_symbol);
        free(raw);
    }

    CHECK(owner_host.fatals == 0, "owner fatal callback was called");
    CHECK(default_host.allocs == 0 && default_host.frees == 0
              && default_host.logs == 0 && default_host.fatals == 0,
          "test changed default linker host callbacks");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && strcmp(ll_default_linker.error_message, default_sentinel) == 0,
          "test changed default linker error sentinel");
    ll_exit_linker(&owner);
    CHECK(owner_host.live == 0, "linker exit left test-owner allocations live");
    if (failures != 0) return 1;
    (void)puts("macho_table_alignment: PASS");
    return 0;
}
