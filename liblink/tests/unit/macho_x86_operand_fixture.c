#define main macho_input_original_main
#include "macho_input.c"
#undef main

#if !defined(darwin_HOST_OS) || !defined(x86_64_HOST_ARCH) \
    || !defined(OBJFORMAT_MACHO)
#error "macho_x86_operand_fixture requires generated x86_64 Darwin configuration"
#endif

static int fixture_external = 25;

int main(int argc, char **argv)
{
    if (argc != 2) {
        (void)fprintf(stderr, "usage: macho_x86_operand_fixture ordinary.o\n");
        return 2;
    }
    ll_compat_host_ops default_ops = make_ops(&default_host);
    ll_compat_host_ops owner_ops = make_ops(&owner_host);
    ll_install_host_for(&ll_default_linker, &default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_error_set(&ll_default_linker, LL_E_NOT_FOUND, "%s", default_sentinel);
    ll_init_linker(&owner, 0);
    owner.services.cfg.always_pic = true;
    CHECK(ghciInsertSymbolTable(&owner, (pathchar *)"fixture-host",
              "_wp24_macho_external", (ll_symbol_addr *)&fixture_external,
              false, LL_SYM_DATA, sizeof(fixture_external), NULL),
          "could not install fixture host data symbol");
    Snapshot before = snapshot();
    size_t size = 0;
    unsigned char *original = read_image(argv[1], &size);
    CHECK(original != NULL && size <= INT_MAX, "could not read compiler fixture");
    if (original == NULL || size > INT_MAX) return 1;
    unsigned char *image = mmap(NULL, size, PROT_READ | PROT_WRITE,
                                MAP_PRIVATE | MAP_ANON, -1, 0);
    if (image == MAP_FAILED) {
        free(original);
        return 1;
    }
    memcpy(image, original, size);
    free(original);
    ObjectCode *oc = mkOc(&owner, STATIC_OBJECT,
                          (pathchar *)"compiler-operand-regression.o",
                          (char *)image, (int)size, true,
                          (pathchar *)"ordinary.o", 0);
    ll_error_clear(&owner);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    int initialized = ll_init_object(oc);
    int loaded = initialized ? loadOc(oc) : 0;
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    CHECK(initialized && loaded && owner.error_status == LL_OK,
          "compiler fixture did not load");
    if (loaded) {
        ll_mutex_lock_for(&owner, &owner.linker_mutex);
        ll_symbol_addr *entry = lookupSymbol_(&owner, "_wp24_macho_entry");
        ll_mutex_unlock_for(&owner, &owner.linker_mutex);
        CHECK(entry != NULL && oc->status == OBJECT_RESOLVED,
              "compiler fixture did not resolve on lookup");
        CHECK(!oc->initializersRan, "fixture initialization ran during lookup");
        RtsSymbolInfo *data = lookupStrHashTable(owner.symhash, "_wp24_macho_data");
        RtsSymbolInfo *pointer = lookupStrHashTable(owner.symhash,
                                                    "_wp24_macho_external_pointer");
        CHECK(data != NULL && pointer != NULL && data->value != NULL
                  && pointer->value != NULL, "fixture data exports missing");
        if (data != NULL && data->value != NULL) {
            int value = 0;
            memcpy(&value, data->value, sizeof(value));
            CHECK(value == 17, "fixture initialized data changed");
        }
        if (pointer != NULL && pointer->value != NULL) {
            int *value = NULL;
            memcpy(&value, pointer->value, sizeof(value));
            CHECK(value == &fixture_external, "fixture external pointer relocation differs");
        }
    }
    ll_discard_object(oc);
    check_loaded_delta(before, "compiler operand fixture leaked owner state");
    ll_exit_linker(&owner);
    CHECK(owner_host.live == 0, "compiler fixture teardown leaked allocations");
    CHECK(owner_host.fatals == 0 && default_host.logs == 0
              && default_host.fatals == 0, "fixture used unexpected host callback");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && strcmp(ll_default_linker.error_message, default_sentinel) == 0,
          "fixture changed default owner sentinel");
    if (failures != 0) return 1;
    (void)puts("Mach-O x86 compiler operand fixture: PASS (no code executed)");
    return 0;
}
