/* Terminal backend-resolution failure cases for macho_load_errors.c. */

static void remove_object_symbol_entries(ObjectCode *oc)
{
    for (int i = 0; i < oc->n_symbols; ++i)
        if (oc->symbols[i].name != NULL)
            ghciRemoveSymbolTable(oc->L, oc->symbols[i].name, oc);
}

static void run_failed_resolve_lookup_gap_probe(bool terminal_resolution)
{
    ObjectCode *oc = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "protection-failure-lookup-gap");
    if (oc == NULL) return;
    bool optimistic = owner.services.cfg.optimistic;
    owner.services.cfg.optimistic = true;
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0 && loadOc(oc) != 0,
          "lookup-gap fixture did not load");
    RtsSymbolInfo *entry = lookupStrHashTable(owner.symhash,
                                               "macho_load_symbol");
    CHECK(entry != NULL && entry->owner == oc,
          "lookup-gap fixture symbol is not registered to its object");
    unsigned before = mprotect_attempts;
    unsigned before_backend = resolution_backend_calls;
    if (terminal_resolution) {
        injected_resolution_status = LL_E_IO;
        memcpy(injected_resolution_message, "terminal lookup resolver error",
               sizeof("terminal lookup resolver error"));
        inject_resolution_without_error = false;
        inject_resolution_failure = true;
    } else {
        mprotect_fail_at = before + 1;
        mprotect_injected_failures = 0;
    }
    int resolved = ocTryLoad(oc);
    if (terminal_resolution) inject_resolution_failure = false;
    else reset_mprotect_injection();
    CHECK(resolved == 0 && oc->status == OBJECT_NEEDED,
          "lookup-gap fixture did not stop on injected protection error");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "first failed resolve lost its IO diagnostic");
    CHECK(resolution_backend_calls == before_backend +
              (terminal_resolution ? 1u : 0u),
          "failed resolve reached an unexpected backend-call count");
    CHECK(mprotect_attempts == before + (terminal_resolution ? 0u : 1u),
          "failure path reached an unexpected protection attempt count");
    char primary_message[LL_ERROR_CAPACITY];
    memcpy(primary_message, owner.error_message, sizeof(primary_message));
    unsigned after_failure = mprotect_attempts;
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    ll_symbol_addr *lookup = lookupSymbol_(&owner,
                                            (ll_symbol_name *)"macho_load_symbol");
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(lookup == NULL,
          "failed NEEDED object exposed its cached symbol value to a later lookup");
    CHECK(owner.error_status == LL_E_STATE,
          "later lookup of a quarantined object did not report LL_E_STATE");
    CHECK(mprotect_attempts == after_failure,
          "cached lookup unexpectedly retried protection");
    CHECK(resolution_backend_calls == before_backend +
              (terminal_resolution ? 1u : 0u),
          "cached lookup unexpectedly re-entered backend resolution");
    CHECK(owner.services.cfg.optimistic,
          "optimistic lookup probe did not enable optimistic linking");
    owner.services.cfg.optimistic = optimistic;
    CHECK(primary_message[0] != '\0',
          "first protection failure diagnostic was empty");
    ll_error_clear(&owner);
    ObjectCode *saved_objects = owner.gc.objects;
    oc->next = saved_objects;
    owner.gc.objects = oc;
    ll_mutex_lock(&owner.linker_mutex);
    int global_resolve = resolveObjs_(&owner);
    ll_mutex_unlock(&owner.linker_mutex);
    owner.gc.objects = saved_objects;
    oc->next = NULL;
    CHECK(global_resolve == 0,
          "global resolve accepted an active quarantined object");
    CHECK(owner.error_status == LL_E_STATE,
          "global resolve of a quarantined object did not report LL_E_STATE");
    CHECK(resolution_backend_calls == before_backend +
              (terminal_resolution ? 1u : 0u),
          "global resolve re-entered a quarantined backend resolver");

    ObjectCode *healthy = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "protection-failure-independent-healthy");
    ObjectCode *healthy_next = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "protection-failure-independent-healthy-next");
    if (healthy != NULL) {
        rename_export(healthy, "healthy_symbol");
        if (healthy_next != NULL) rename_export(healthy_next, "healthy_next");
        ll_error_clear(&owner);
        CHECK(ll_init_object(healthy) != 0 && loadOc(healthy) != 0 &&
                  ocTryLoad(healthy) != 0,
              "independent healthy fixture did not resolve");
        if (healthy_next != NULL)
            CHECK(ll_init_object(healthy_next) != 0 && loadOc(healthy_next) != 0 &&
                      ocTryLoad(healthy_next) != 0,
                  "second independent healthy fixture did not resolve");
        healthy->next = oc;
        if (healthy_next != NULL) {
            healthy_next->next = healthy;
            healthy->next = oc;
            healthy_next->next = healthy;
        }
        oc->next = saved_objects;
        owner.gc.objects = healthy_next != NULL ? healthy_next : healthy;
        ll_error_clear(&owner);
        unsigned before_init = owner_host.before_init_calls;
        unsigned before_after = owner_host.after_init_calls;
        unsigned before_batch = owner_host.init_batches;
        ll_mutex_lock(&owner.linker_mutex);
        ll_symbol_addr *healthy_lookup = lookupSymbol_(
            &owner, (ll_symbol_name *)"healthy_symbol");
        ll_mutex_unlock(&owner.linker_mutex);
        CHECK(healthy_lookup != NULL,
              "healthy symbol lookup failed beside a quarantined object");
        CHECK(healthy->status == OBJECT_READY,
              "healthy object initializer pass did not complete");
        CHECK(oc->status == OBJECT_NEEDED,
              "independent lookup changed quarantined object's lifecycle status");
        unsigned expected_hooks = healthy_next != NULL ? 2u : 1u;
        CHECK(owner_host.before_init_calls == before_init + expected_hooks &&
                  owner_host.after_init_calls == before_after + expected_hooks,
              "independent lookup did not preserve healthy init lifecycle hooks");
        CHECK(owner_host.init_batches == before_batch + 1,
              "independent lookup did not preserve the init batch hook");
        CHECK(before_init < HOST_CAPACITY &&
                  strcmp(owner_host.init_order[before_init],
                         healthy_next != NULL
                             ? "protection-failure-independent-healthy-next"
                             : "protection-failure-independent-healthy") == 0,
              "first healthy initializer order changed beside quarantine");
        if (healthy_next != NULL)
            CHECK(before_init + 1 < HOST_CAPACITY &&
                      strcmp(owner_host.init_order[before_init + 1],
                             "protection-failure-independent-healthy") == 0,
                  "second healthy initializer order changed beside quarantine");
        owner.gc.objects = saved_objects;
        if (healthy_next != NULL) {
            healthy_next->next = NULL;
            dispose_test_object(healthy_next);
        }
        healthy->next = NULL;
        oc->next = NULL;
        dispose_test_object(healthy);
    }
    ll_discard_object(oc);
    dispose_test_object(oc);
}


static void run_terminal_resolution_status_probe(void)
{
    static const ll_status terminal_statuses[] = {
        LL_E_IO, LL_E_FORMAT, LL_E_UNSUPPORTED, LL_E_DUPSYM,
        LL_E_RELOC_RANGE, LL_E_NOMEM, LL_E_INIT, LL_E_NOT_FOUND, LL_E_STATE
    };
    bool saved_optimistic = owner.services.cfg.optimistic;
    owner.services.cfg.optimistic = true;

    for (size_t i = 0; i < sizeof(terminal_statuses) / sizeof(terminal_statuses[0]);
         ++i) {
        char label[64];
        int written = snprintf(label, sizeof(label), "terminal-resolution-%u",
                               (unsigned)terminal_statuses[i]);
        CHECK(written > 0 && (size_t)written < sizeof(label),
              "terminal-resolution fixture label overflowed");
        if (written <= 0 || (size_t)written >= sizeof(label)) continue;

        Snapshot before = snapshot();
        ObjectCode *oc = make_object(
            (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0}, label);
        if (oc == NULL) continue;
        ll_error_clear(&owner);
        CHECK(ll_init_object(oc) != 0 && loadOc(oc) != 0,
              "terminal-resolution fixture did not reach NEEDED state");
        if (oc->status != OBJECT_NEEDED) {
            dispose_test_object(oc);
            check_rollback(before, "terminal-resolution fixture setup failed");
            continue;
        }

        for (size_t j = 0; j + 1 < sizeof(injected_resolution_message); ++j)
            injected_resolution_message[j] = (char)('a' + (j % 26));
        injected_resolution_message[sizeof(injected_resolution_message) - 1] = '\0';
        char expected_message[LL_ERROR_CAPACITY];
        memcpy(expected_message, injected_resolution_message,
               sizeof(expected_message));
        injected_resolution_status = terminal_statuses[i];
        inject_resolution_without_error = false;
        inject_resolution_failure = true;
        unsigned calls_before = resolution_backend_calls;
        int first_result = ocTryLoad(oc);
        inject_resolution_failure = false;

        CHECK(first_result == 0 && oc->status == OBJECT_NEEDED,
              "terminal backend failure advanced object lifecycle state");
        CHECK(oc->resolution_failed,
              "explicit terminal backend failure did not mark its object");
        CHECK(owner.error_status == terminal_statuses[i] &&
                  memcmp(owner.error_message, expected_message,
                         sizeof(expected_message)) == 0,
              "first terminal failure did not preserve full primary diagnostic");
        CHECK(resolution_backend_calls == calls_before + 1,
              "terminal failure did not reach the backend exactly once");

        ll_error_clear(&owner);
        int repeated_result = ocTryLoad(oc);
        CHECK(repeated_result == 0 && owner.error_status == LL_E_STATE,
              "later terminal-failed resolve did not return LL_E_STATE");
        CHECK(resolution_backend_calls == calls_before + 1,
              "later terminal-failed resolve re-entered the backend");
        ll_error_clear(&owner);
        ll_mutex_lock(&owner.linker_mutex);
        ll_symbol_addr *cached = lookupSymbol_(
            &owner, (ll_symbol_name *)"macho_load_symbol");
        ll_mutex_unlock(&owner.linker_mutex);
        CHECK(cached == NULL && owner.error_status == LL_E_STATE,
              "terminal-failed object exposed a cached or optimistic address");
        CHECK(resolution_backend_calls == calls_before + 1,
              "cached lookup re-entered the terminal-failed backend");

        ObjectCode *saved_objects = owner.gc.objects;
        oc->next = saved_objects;
        owner.gc.objects = oc;
        unsigned before_init = owner_host.before_init_calls;
        ll_error_clear(&owner);
        ll_mutex_lock(&owner.linker_mutex);
        int global_result = resolveObjs_(&owner);
        ll_mutex_unlock(&owner.linker_mutex);
        owner.gc.objects = saved_objects;
        oc->next = NULL;
        CHECK(global_result == 0 && owner.error_status == LL_E_STATE,
              "global resolve accepted a terminal-failed object");
        CHECK(owner_host.before_init_calls == before_init,
              "terminal-failed object reached initializer hooks");
        CHECK(resolution_backend_calls == calls_before + 1,
              "global resolve re-entered the terminal-failed backend");

        dispose_test_object(oc);
        check_rollback(before, "terminal-resolution status probe leaked state");
    }

    owner.services.cfg.optimistic = saved_optimistic;
}

static void run_unresolved_backend_retry_probe(void)
{
    Snapshot before = snapshot();
    ObjectCode *oc = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "ordinary-backend-unresolved-retry");
    if (oc == NULL) return;
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0 && loadOc(oc) != 0,
          "ordinary backend retry fixture did not load");

    inject_resolution_without_error = true;
    inject_resolution_failure = true;
    unsigned before_calls = resolution_backend_calls;
    int unresolved = ocTryLoad(oc);
    inject_resolution_failure = false;
    inject_resolution_without_error = false;
    CHECK(unresolved == 0 && owner.error_status == LL_E_UNRESOLVED,
          "backend false without status did not retain UNRESOLVED fallback");
    CHECK(!oc->resolution_failed,
          "ordinary UNRESOLVED fallback quarantined the object");
    CHECK(resolution_backend_calls == before_calls + 1,
          "ordinary unresolved attempt did not call backend exactly once");

    ll_error_clear(&owner);
    int retried = ocTryLoad(oc);
    CHECK(retried != 0 && oc->status == OBJECT_RESOLVED,
          "ordinary unresolved object did not resolve after retry");
    CHECK(!oc->resolution_failed && resolution_backend_calls == before_calls + 1,
          "ordinary retry changed quarantine state or called injected backend");
    dispose_test_object(oc);
    check_rollback(before, "ordinary backend retry probe leaked state");
}

static void run_early_duplicate_resolution_negative_probe(void)
{
    Snapshot before = snapshot();
    ObjectCode *first = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "terminal-policy-duplicate-first");
    ObjectCode *second = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "terminal-policy-duplicate-second");
    CHECK(first != NULL && second != NULL,
          "could not create duplicate-symbol negative controls");
    if (first == NULL || second == NULL) goto cleanup_duplicates;

    ll_error_clear(&owner);
    CHECK(ll_init_object(first) != 0 && loadOc(first) != 0,
          "first duplicate-control object did not load");
    remove_object_symbol_entries(first);
    ll_error_clear(&owner);
    CHECK(ll_init_object(second) != 0 && loadOc(second) != 0,
          "second duplicate-control symbol owner did not load");
    unsigned before_backend = resolution_backend_calls;
    int duplicate_result = ocTryLoad(first);
    CHECK(duplicate_result == 0 && owner.error_status == LL_E_DUPSYM,
          "early duplicate-symbol rejection lost its DUPSYM status");
    CHECK(!first->resolution_failed,
          "pre-backend duplicate rejection was marked terminal");
    CHECK(resolution_backend_calls == before_backend,
          "early duplicate rejection reached the backend resolver");
    removeOcSymbols(second);
    ll_error_clear(&owner);
    CHECK(ocTryLoad(first) != 0 && !first->resolution_failed,
          "duplicate-control object did not retry after removing its collision");

cleanup_duplicates:
    if (second != NULL) {
        dispose_test_object(second);
    }
    if (first != NULL) {
        dispose_test_object(first);
    }
    check_rollback(before, "early duplicate negative probe leaked state");
}

static void run_nested_duplicate_resolution_negative_probe(void)
{
    Snapshot before = snapshot();
    ObjectCode *conflict = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "nested-duplicate-conflicting-owner");
    ObjectCode *nested = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "nested-duplicate-child");
    ObjectCode *parent = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "nested-duplicate-parent");
    ObjectCode *fresh_error = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "nested-duplicate-followup-backend-error");
    ObjectCode *stale_error = make_object(
        (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
        "nested-duplicate-top-level-backend-error");
    CHECK(conflict != NULL && nested != NULL && parent != NULL &&
              fresh_error != NULL && stale_error != NULL,
          "could not create nested duplicate fixtures");
    if (conflict == NULL || nested == NULL || parent == NULL ||
        fresh_error == NULL || stale_error == NULL)
        goto cleanup_nested_duplicate;

    rename_export(parent, "parent_unique");
    rename_export(fresh_error, "fresh_backend");
    rename_export(stale_error, "stale_backend");
    ll_error_clear(&owner);
    CHECK(ll_init_object(nested) != 0 && loadOc(nested) != 0,
          "nested duplicate object did not reach NEEDED state");
    remove_object_symbol_entries(nested);
    ll_error_clear(&owner);
    CHECK(ll_init_object(conflict) != 0 && loadOc(conflict) != 0,
          "conflicting symbol owner did not load");
    ll_error_clear(&owner);
    CHECK(ll_init_object(parent) != 0 && loadOc(parent) != 0,
          "parent duplicate fixture did not reach NEEDED state");
    CHECK(ll_init_object(fresh_error) != 0 && loadOc(fresh_error) != 0,
          "follow-up backend error object did not load");
    CHECK(ll_init_object(stale_error) != 0 && loadOc(stale_error) != 0,
          "top-level stale-provenance control object did not load");

    nested_duplicate_object = nested;
    inject_nested_duplicate = true;
    unsigned before_backend = resolution_backend_calls;
    int parent_result = ocTryLoad(parent);
    inject_nested_duplicate = false;
    nested_duplicate_object = NULL;
    CHECK(parent_result == 0 && owner.error_status == LL_E_DUPSYM,
          "nested early duplicate did not propagate its original status");
    CHECK(!nested->resolution_failed && !parent->resolution_failed,
          "nested pre-backend duplicate incorrectly quarantined an object");
    CHECK(parent->status == OBJECT_NEEDED && nested->status == OBJECT_NEEDED,
          "nested duplicate advanced an affected object lifecycle state");
    CHECK(resolution_backend_calls == before_backend + 1,
          "nested duplicate unexpectedly re-entered the backend");
    CHECK(owner.error_from_pre_backend,
          "nested early duplicate lost its pre-backend provenance");

    injected_resolution_status = LL_E_FORMAT;
    memcpy(injected_resolution_message, "same-operation explicit backend error",
           sizeof("same-operation explicit backend error"));
    nested_duplicate_object = nested;
    inject_nested_duplicate_then_terminal = true;
    unsigned before_same_operation_backend = resolution_backend_calls;
    int same_operation_result = ocTryLoad(fresh_error);
    nested_duplicate_object = NULL;
    CHECK(same_operation_result == 0 && fresh_error->resolution_failed &&
              !nested->resolution_failed,
          "backend error following nested DUPSYM did not quarantine only its owner");
    CHECK(owner.error_status == LL_E_FORMAT &&
              memcmp(owner.error_message, nested_terminal_expected_message,
                     sizeof(nested_terminal_expected_message)) == 0 &&
              strcmp(owner.error_message,
                     "same-operation explicit backend error") == 0,
          "same-operation cleanup did not preserve the new full diagnostic");
    CHECK(!owner.error_from_pre_backend,
          "same-operation ll_error_set retained nested pre-backend provenance");
    CHECK(resolution_backend_calls == before_same_operation_backend + 1,
          "same-operation nested error used an unexpected backend-call count");

    injected_resolution_status = LL_E_FORMAT;
    memcpy(injected_resolution_message, "fresh explicit backend error",
           sizeof("fresh explicit backend error"));
    inject_resolution_without_error = false;
    inject_resolution_failure = true;
    owner.error_from_pre_backend = true;
    int fresh_result = ocTryLoad(stale_error);
    inject_resolution_failure = false;
    CHECK(fresh_result == 0 && stale_error->resolution_failed &&
              owner.error_status == LL_E_FORMAT &&
              !resolution_wrapper_entry_from_pre_backend &&
              !owner.error_from_pre_backend,
          "top-level operation did not clear stale duplicate provenance before backend entry");

    removeOcSymbols(conflict);
    ll_error_clear(&owner);
    CHECK(ocTryLoad(nested) != 0 && !nested->resolution_failed,
          "nested object did not recover after removing the duplicate owner");
    ll_error_clear(&owner);
    CHECK(ocTryLoad(parent) != 0 && !parent->resolution_failed,
          "parent object did not recover after nested duplicate removal");

cleanup_nested_duplicate:
    inject_nested_duplicate = false;
    inject_nested_duplicate_then_terminal = false;
    nested_duplicate_object = NULL;
    inject_resolution_failure = false;
    if (fresh_error != NULL) {
        dispose_test_object(fresh_error);
    }
    if (stale_error != NULL) {
        dispose_test_object(stale_error);
    }
    if (parent != NULL) {
        dispose_test_object(parent);
    }
    if (nested != NULL) {
        dispose_test_object(nested);
    }
    if (conflict != NULL) {
        dispose_test_object(conflict);
    }
    check_rollback(before, "nested duplicate negative probe leaked state");
}

static void run_terminal_resolution_probes(void)
{
    /* The hash table retains its chunk allocation until table teardown.  A
       focused run may otherwise mistake this first-use arena allocation for
       a leak from the first fixture. */
    static const char hash_arena_warmup[] = "terminal-policy-hash-warmup";
    insertStrHashTable(owner.symhash, hash_arena_warmup, NULL);
    removeStrHashTable(owner.symhash, hash_arena_warmup, NULL);
    Snapshot before = snapshot();
    run_nested_duplicate_resolution_negative_probe();
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded &&
              active_mappings() == before.active_maps,
          "nested duplicate negative probe left an active object or mapping");
    before = snapshot();
    run_terminal_resolution_status_probe();
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded &&
              active_mappings() == before.active_maps,
          "terminal-status probe left an active object or mapping");
    before = snapshot();
    run_dependency_closure_frames_probe(true);
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded &&
              active_mappings() == before.active_maps,
          "terminal closure probe left an active object or mapping");
    before = snapshot();
    run_failed_resolve_lookup_gap_probe(true);
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded &&
              active_mappings() == before.active_maps,
          "terminal cached-lookup probe left an active object or mapping");
    before = snapshot();
    run_preregister_weak_failure(true);
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded &&
              active_mappings() == before.active_maps,
          "terminal preregistration probe left an active object or mapping");
    before = snapshot();
    run_unload_reload_quarantine_probe(true);
    CHECK(owner.gc.loaded_objects == before.loaded &&
              active_mappings() == before.active_maps,
          "terminal unload/reload probe left an active object or mapping");
    before = snapshot();
    run_unresolved_backend_retry_probe();
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded &&
              active_mappings() == before.active_maps,
          "ordinary unresolved retry probe left an active object or mapping");
    before = snapshot();
    run_early_duplicate_resolution_negative_probe();
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded &&
              active_mappings() == before.active_maps,
          "early duplicate negative probe left an active object or mapping");
}
