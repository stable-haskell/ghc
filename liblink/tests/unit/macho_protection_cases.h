/* Protection and M32 failure cases for macho_load_errors.c. */

static void rename_export(ObjectCode *oc, const char *name)
{
    MachOHeader *header = (MachOHeader *)oc->image;
    MachOSegmentCommand *segment = (MachOSegmentCommand *)(header + 1);
    MachOSymtabCommand *symtab = (MachOSymtabCommand *)
        ((char *)segment + segment->cmdsize);
    size_t capacity = symtab->strsize - 1;
    CHECK(strlen(name) < capacity, "replacement Mach-O symbol name is too long");
    if (strlen(name) < capacity)
        memcpy(oc->image + symtab->stroff + 1, name, strlen(name) + 1);
}

static bool write_fixture_file(char *path, size_t path_capacity,
                               FixtureSpec spec, const char *symbol)
{
    if (path_capacity < sizeof("/tmp/ll-quarantine-XXXXXX")) return false;
    memcpy(path, "/tmp/ll-quarantine-XXXXXX",
           sizeof("/tmp/ll-quarantine-XXXXXX"));
    int fd = mkstemp(path);
    if (fd < 0) return false;
    unsigned char *image = make_image(spec);
    if (image == NULL) {
        (void)close(fd);
        (void)unlink(path);
        return false;
    }
    if (symbol != NULL) {
        ObjectCode fake = {0};
        fake.image = (char *)image;
        rename_export(&fake, symbol);
    }
    size_t written = 0;
    while (written < IMAGE_CAPACITY) {
        ssize_t n = write(fd, image + written, IMAGE_CAPACITY - written);
        if (n <= 0) break;
        written += (size_t)n;
    }
    int close_result = close(fd);
    bool ok = written == IMAGE_CAPACITY && close_result == 0;
    test_munmap_for_linker(&owner, image, IMAGE_CAPACITY,
                           "write_fixture_file");
    if (!ok) (void)unlink(path);
    return ok;
}

static ObjectCode *find_loaded_path(const char *path)
{
    for (ObjectCode *oc = owner.gc.loaded_objects; oc != NULL;
         oc = oc->next_loaded_object)
        if (oc->fileName != NULL && strcmp(oc->fileName, path) == 0)
            return oc;
    return NULL;
}

static bool dependency_contains(ObjectCode *dependent, ObjectCode *provider);

static void dispose_test_object(ObjectCode *oc)
{
    if (oc == NULL) return;
    ObjectCode **link = &owner.gc.loaded_objects;
    while (*link != NULL && *link != oc)
        link = &(*link)->next_loaded_object;
    if (*link == oc) *link = oc->next_loaded_object;
    oc->next_loaded_object = NULL;
    removeOcSymbols(oc);
    oc->status = OBJECT_UNLOADED;
    freeObjectCode(oc);
}

static void reset_mprotect_injection(void)
{
    mprotect_fail_at = 0;
}

static void run_mprotect_helper_status(void)
{
    ll_error_clear(&owner);
    unsigned before = mprotect_attempts;
    CHECK(ll_mprotect_for_linker(&owner, NULL, 0, MEM_READ_EXECUTE),
          "zero-length protection was not a successful no-op");
    CHECK(mprotect_attempts == before,
          "zero-length protection called the operating system");
    CHECK(owner.error_status == LL_OK,
          "zero-length protection changed the owner error status");

    void *memory = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANON, -1, 0);
    CHECK(memory != MAP_FAILED, "could not map protection helper control");
    if (memory == MAP_FAILED) return;
    CHECK(ll_mprotect_for_linker(&owner, memory, page_size, MEM_READ_ONLY),
          "real protection syscall success was rejected");
    CHECK(owner.error_status == LL_OK,
          "successful protection changed the owner error status");
    CHECK(mprotect(memory, page_size, PROT_READ | PROT_WRITE) == 0,
          "could not restore protection helper control mapping");

    ll_default_linker.error_status = LL_E_NOT_FOUND;
    memcpy(ll_default_linker.error_message, default_sentinel,
           sizeof(default_sentinel));
    ll_error_clear(&owner);
    before = mprotect_attempts;
    mprotect_fail_at = before + 1;
    mprotect_injected_failures = 0;
    clobber_log_errno = true;
    errno = 0;
    bool protected = ll_mprotect_for_linker(&owner, memory, page_size,
                                             MEM_READ_EXECUTE);
    clobber_log_errno = false;
    reset_mprotect_injection();
    CHECK(!protected, "injected protection error was accepted");
    CHECK(mprotect_attempts == before + 1 && mprotect_injected_failures == 1,
          "protection error was not injected exactly once");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "protection failure did not set owner IO status and diagnostic");
    CHECK(strstr(owner.error_message, "errno 13") != NULL,
          "protection diagnostic did not retain EACCES");
    CHECK(errno == EACCES,
          "protection failure did not restore errno after logging");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
              && memcmp(ll_default_linker.error_message, default_sentinel,
                        sizeof(default_sentinel)) == 0,
          "owner protection failure changed default linker error state");
    CHECK(munmap(memory, page_size) == 0,
          "could not unmap protection helper control");
}

static void run_oc_try_load_protection_failure(SectionProfile profile,
                                               unsigned fail_offset,
                                               const char *label)
{
    Snapshot before = snapshot();
    ObjectCode *oc = make_object((FixtureSpec){profile, SYMBOLS_NONE, 0}, label);
    if (oc == NULL) return;
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0, "protection fixture initialization failed");
    CHECK(loadOc(oc) != 0, "protection fixture did not reach loaded state");
    CHECK(oc->status == OBJECT_NEEDED,
          "protection fixture did not reach OBJECT_NEEDED before resolve");

    unsigned before_attempts = mprotect_attempts;
    mprotect_fail_at = before_attempts + fail_offset;
    mprotect_injected_failures = 0;
    int resolved = ocTryLoad(oc);
    reset_mprotect_injection();

    CHECK(resolved == 0, label);
    CHECK(oc->status == OBJECT_NEEDED,
          "failed protection advanced the object to OBJECT_RESOLVED");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "late protection failure lost its IO status");
    CHECK(mprotect_injected_failures == 1,
          "late protection failure injection was not reached");
    CHECK(owner_host.fatals == 0,
          "late protection failure reached fatal callback");
    ll_discard_object(oc);
    dispose_test_object(oc);
    check_rollback(before, label);
}

#if defined(NEED_SYMBOL_EXTRAS) && \
    (defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH) || \
     defined(powerpc_HOST_ARCH) || defined(riscv64_HOST_ARCH))
static void run_oc_try_load_extras_protection_failure(void)
{
    Snapshot before = snapshot();
    ObjectCode *oc = make_object((FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0},
                                 "ocTryLoad-symbol-extras-protection-failure");
    if (oc == NULL) return;
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0, "extras fixture initialization failed");
    CHECK(loadOc(oc) != 0, "extras fixture did not reach loaded state");
    void *memory = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANON, -1, 0);
    CHECK(memory != MAP_FAILED, "could not map symbol extras fixture");
    if (memory == MAP_FAILED) {
        ll_discard_object(oc);
        check_rollback(before, "extras fixture setup failed");
        return;
    }
    oc->symbol_extras = memory;
    oc->n_symbol_extras = 1;
    oc->symbol_extra_stride = sizeof(SymbolExtra);

    unsigned before_attempts = mprotect_attempts;
    mprotect_fail_at = before_attempts + 1;
    mprotect_injected_failures = 0;
    int resolved = ocTryLoad(oc);
    reset_mprotect_injection();
    CHECK(resolved == 0,
          "ocTryLoad accepted failed symbol-extras protection");
    CHECK(oc->status == OBJECT_NEEDED,
          "extras protection failure advanced object to OBJECT_RESOLVED");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "extras protection failure lost IO status");
    CHECK(mprotect_injected_failures == 1,
          "extras protection failure injection was not reached");
    oc->symbol_extras = NULL;
    oc->n_symbol_extras = 0;
    CHECK(munmap(memory, page_size) == 0,
          "could not unmap symbol extras fixture");
    ll_discard_object(oc);
    dispose_test_object(oc);
    check_rollback(before, "extras protection failure leaked state");
}
#endif

#if defined(NEED_M32)
static void drain_m32_free_pool(void);

static void run_oc_try_load_m32_failure(bool executable, const char *label)
{
    Snapshot before = snapshot();
    ObjectCode *oc = make_object((FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0},
                                 label);
    if (oc == NULL) return;
    ll_error_clear(&owner);
    CHECK(ll_init_object(oc) != 0, "m32 protection fixture initialization failed");
    CHECK(loadOc(oc) != 0, "m32 protection fixture did not reach loaded state");

    if (executable) {
        CHECK(oc->rx_m32 != NULL &&
              m32_alloc_for(&owner, oc->rx_m32, page_size, 8) != NULL,
              "could not create pending RX m32 mapping");
    } else {
        CHECK(oc->rw_m32 != NULL &&
              m32_alloc_for(&owner, oc->rw_m32, 8, 8) != NULL,
              "could not create pending RW m32 nursery page");
        CHECK(oc->rw_m32 != NULL && oc->rw_m32->pages[0] != NULL,
              "RW m32 fixture has no nursery page");
        if (oc->rw_m32 != NULL && oc->rw_m32->pages[0] != NULL)
            oc->rw_m32->pages[0]->current_size = sizeof(struct m32_page_t);
    }

    unsigned before_attempts = mprotect_attempts;
    mprotect_fail_at = before_attempts + 1;
    mprotect_injected_failures = 0;
    int resolved = ocTryLoad(oc);
    reset_mprotect_injection();
    CHECK(resolved == 0, label);
    CHECK(oc->status == OBJECT_NEEDED,
          "m32 protection failure advanced the object to OBJECT_RESOLVED");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "m32 protection failure lost IO status");
    CHECK(mprotect_injected_failures == 1,
          "m32 protection failure injection was not reached");
    CHECK(owner_host.fatals == 0,
          "m32 protection failure reached fatal callback");

    ll_discard_object(oc);
    dispose_test_object(oc);
    drain_m32_free_pool();
    check_rollback(before, label);
}
#endif

#if defined(NEED_M32)
static size_t m32_list_occurrences(struct m32_page_t *head,
                                   struct m32_page_t *wanted)
{
    size_t count = 0;
    size_t steps = 0;
    while (head != NULL && steps++ < 64) {
        if (head == wanted) ++count;
        head = m32_filled_page_get_next(&owner, head);
    }
    CHECK(steps < 64, "m32 list contains a cycle or excess pages");
    return count;
}

static void drain_m32_free_pool(void)
{
    while (owner.m32_free_page_pool != NULL) {
        struct m32_page_t *page = owner.m32_free_page_pool;
        owner.m32_free_page_pool = page->free_page.next;
        CHECK(owner.m32_free_page_pool_size > 0,
              "m32 free-pool count underflowed during fixture cleanup");
        if (owner.m32_free_page_pool_size > 0)
            --owner.m32_free_page_pool_size;
        test_munmap_for_linker(&owner, page, page_size,
                               "drain_m32_free_pool");
    }
    CHECK(owner.m32_free_page_pool_size == 0,
          "m32 free-pool count remained nonzero after fixture cleanup");
}

static void run_m32_rx_partial_flush_failure(void)
{
    Snapshot before = snapshot();
    m32_allocator *alloc = m32_allocator_new_for(&owner, true);
    CHECK(alloc != NULL, "could not create executable m32 allocator");
    if (alloc == NULL) return;
    CHECK(m32_alloc_for(&owner, alloc, page_size, 8) != NULL,
          "first large RX m32 mapping failed");
    CHECK(m32_alloc_for(&owner, alloc, page_size, 8) != NULL,
          "second large RX m32 mapping failed");
    struct m32_page_t *first = alloc->unprotected_list;
    struct m32_page_t *second = first != NULL
        ? m32_filled_page_get_next(&owner, first) : NULL;
    CHECK(first != NULL && second != NULL &&
              m32_filled_page_get_next(&owner, second) == NULL,
          "fixture did not create exactly two unprotected RX pages");

    unsigned before_attempts = mprotect_attempts;
    mprotect_fail_at = before_attempts + 2;
    mprotect_injected_failures = 0;
    ll_error_clear(&owner);
    bool flushed = m32_allocator_flush_for(&owner, alloc);
    reset_mprotect_injection();
    CHECK(!flushed, "partial RX protection failure returned success");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "partial RX protection failure lost IO status");
    CHECK(mprotect_attempts == before_attempts + 2 &&
              mprotect_injected_failures == 1,
          "partial RX failure did not protect one page then fail the next");
    CHECK(alloc->unprotected_list == NULL,
          "failed RX page remained on the unprotected list");
    CHECK(m32_list_occurrences(alloc->protected_list, first) == 1 &&
              m32_list_occurrences(alloc->protected_list, second) == 1,
          "RX page ownership was lost or duplicated across failure");

    m32_allocator_free_for(&owner, alloc);
    CHECK(owner.m32_free_page_pool_size == 4,
          "normal RX allocator cleanup did not return all four mapping pages");
    drain_m32_free_pool();
    check_rollback(before, "partial RX failure cleanup leaked state");
}

static void run_m32_rw_release_failure(void)
{
    Snapshot before = snapshot();
    m32_allocator *alloc = m32_allocator_new_for(&owner, false);
    CHECK(alloc != NULL, "could not create writable m32 allocator");
    if (alloc == NULL) return;
    CHECK(m32_alloc_for(&owner, alloc, 8, 8) != NULL,
          "writable m32 nursery allocation failed");
    size_t pool_before = owner.m32_free_page_pool_size;
    struct m32_page_t *empty = alloc->pages[0];
    CHECK(empty != NULL, "writable allocator did not own a nursery page");
    if (empty == NULL) {
        m32_allocator_free_for(&owner, alloc);
        drain_m32_free_pool();
        return;
    }

    empty->current_size = sizeof(struct m32_page_t);
    unsigned before_attempts = mprotect_attempts;
    mprotect_fail_at = before_attempts + 1;
    mprotect_injected_failures = 0;
    ll_error_clear(&owner);
    bool flushed = m32_allocator_flush_for(&owner, alloc);
    reset_mprotect_injection();
    CHECK(!flushed, "failed RW release returned success");
    CHECK(owner.error_status == LL_E_IO && owner.error_message[0] != '\0',
          "failed RW release lost IO status");
    CHECK(alloc->pages[0] == NULL,
          "failed RW release retained the now-unmapped nursery reference");
    CHECK(!mapping_contains(empty),
          "failed RW release did not unmap the page after protection failure");
    CHECK(owner.m32_free_page_pool_size == pool_before,
          "failed RW release wrote or pooled the unprotected page");
    m32_allocator_free_for(&owner, alloc);
    drain_m32_free_pool();
    check_rollback(before, "failed RW release cleanup leaked state");
}
#endif

static void run_protection_status_cases(void)
{
    run_mprotect_helper_status();
    run_oc_try_load_protection_failure(PROFILE_CLASSES, 1,
                                       "Mach-O RX segment protection failure");
#if defined(aarch64_HOST_ARCH) || defined(arm64_HOST_ARCH)
    run_oc_try_load_protection_failure(PROFILE_CLASSES, 2,
                                       "Mach-O mapped section protection failure");
#else
    (void)puts("macho_load_errors: SKIP separate mapped-section protection on non-AArch64");
#endif
#if defined(NEED_SYMBOL_EXTRAS) && \
    (defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH) || \
     defined(powerpc_HOST_ARCH) || defined(riscv64_HOST_ARCH))
    run_oc_try_load_extras_protection_failure();
#else
    (void)puts("macho_load_errors: SKIP zero-sized or unavailable symbol extras");
#endif
#if defined(NEED_M32)
    run_oc_try_load_m32_failure(true, "ocTryLoad RX m32 protection failure");
    run_oc_try_load_m32_failure(false, "ocTryLoad RW m32 release failure");
    run_m32_rx_partial_flush_failure();
    run_m32_rw_release_failure();
#else
    (void)puts("macho_load_errors: SKIP m32 protection paths without NEED_M32");
#endif
}

static void run_preregister_weak_failure(bool terminal_resolution)
{
    char provider_path[64];
    char dependent_path[64];
    bool provider_file = write_fixture_file(provider_path, sizeof(provider_path),
              (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
              "macho_load_symbol");
    CHECK(provider_file,
          "could not write provider Mach-O fixture");
    bool dependent_file = write_fixture_file(dependent_path, sizeof(dependent_path),
              (FixtureSpec){PROFILE_CLASSES, SYMBOLS_WEAK_EXPORT, 0},
              "macho_load_symbol");
    CHECK(dependent_file,
          "could not write weak-dependent Mach-O fixture");
    if (!provider_file || !dependent_file) return;

    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    int provider_loaded = loadObj_(&owner, provider_path);
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(provider_loaded != 0,
          "provider fixture did not load through loadObj_");
    if (!provider_loaded) {
        (void)fprintf(stderr, "provider load status=%d message=%s path=%s\n",
                       owner.error_status, owner.error_message, provider_path);
        (void)unlink(provider_path);
        (void)unlink(dependent_path);
        return;
    }
    ObjectCode *provider = find_loaded_path(provider_path);
    CHECK(provider != NULL,
          "provider fixture was not registered by loadObj_");
    if (provider != NULL)
        provider->status = OBJECT_LOADED; /* archive members resolve on demand */

    unsigned before = mprotect_attempts;
    unsigned before_backend = resolution_backend_calls;
    if (terminal_resolution) {
        injected_resolution_status = LL_E_NOMEM;
        memcpy(injected_resolution_message, "pre-registration terminal resolver error",
               sizeof("pre-registration terminal resolver error"));
        inject_resolution_without_error = false;
        inject_resolution_failure = true;
    } else {
        mprotect_fail_at = before + 1;
        mprotect_injected_failures = 0;
    }
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    int dependent_loaded = loadObj_(&owner, dependent_path);
    ll_mutex_unlock(&owner.linker_mutex);
    if (terminal_resolution) inject_resolution_failure = false;
    else reset_mprotect_injection();
    CHECK(dependent_loaded == 0,
          "pre-registration weak lookup accepted provider protection failure");
    CHECK(terminal_resolution
              ? resolution_backend_calls == before_backend + 1
              : mprotect_injected_failures == 1,
          "pre-registration weak lookup did not reach its injected resolver failure");
    CHECK(owner.error_status == (terminal_resolution ? LL_E_NOMEM : LL_E_IO) &&
              owner.error_message[0] != '\0',
          "pre-registration unwind did not preserve its first failure diagnostic");

    ObjectCode *dependent = find_loaded_path(dependent_path);
    CHECK(dependent != NULL,
          "failed pre-registration dependent was freed instead of retained");
    if (dependent != NULL) {
        CHECK(dependent->resolution_failed,
              "failed pre-registration dependent was not quarantined");
        if (provider != NULL)
            CHECK(dependency_contains(dependent, provider),
                  "escaped pre-registration dependency key was not retained");
    }
    provider = find_loaded_path(provider_path);
    CHECK(provider != NULL && provider->resolution_failed,
          "weak symbol provider was not retained in quarantine");
    dispose_test_object(dependent);
    dispose_test_object(provider);
    (void)unlink(provider_path);
    (void)unlink(dependent_path);
}

static void run_ordinary_missing_retry_probe(void)
{
    char path[64];
    bool written = write_fixture_file(path, sizeof(path),
              (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
              "_retry_symbol");
    CHECK(written, "could not write ordinary retry provider fixture");
    if (!written) return;

    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    ll_symbol_addr *missing = lookupSymbol_(&owner,
                                            (ll_symbol_name *)"_retry_symbol");
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(missing == NULL,
          "ordinary missing-symbol lookup did not remain unresolved");

    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    int loaded = loadObj_(&owner, path);
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(loaded != 0,
          "ordinary unresolved-symbol provider did not load on retry");
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    ll_symbol_addr *resolved = lookupSymbol_(&owner,
                                              (ll_symbol_name *)"_retry_symbol");
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(resolved != NULL,
          "ordinary missing-symbol retry did not find the new definition");
    ObjectCode *provider = find_loaded_path(path);
    CHECK(provider != NULL && !provider->resolution_failed,
          "ordinary retry incorrectly marked its provider quarantined");
    dispose_test_object(provider);
    (void)unlink(path);
}

static void run_unload_reload_quarantine_probe(bool terminal_resolution)
{
    char path[64];
    bool written = write_fixture_file(path, sizeof(path),
              (FixtureSpec){PROFILE_CLASSES, SYMBOLS_EXPORT, 0},
              "macho_load_symbol");
    CHECK(written, "could not write unload/reload fixture");
    if (!written) return;

    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    int loaded = loadObj_(&owner, path);
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(loaded != 0, "unload/reload fixture did not load");
    ObjectCode *old = find_loaded_path(path);
    CHECK(old != NULL, "unload/reload fixture was not registered");
    if (old == NULL) {
        (void)unlink(path);
        return;
    }
    old->status = OBJECT_LOADED;
    unsigned before_backend = resolution_backend_calls;
    if (terminal_resolution) {
        injected_resolution_status = LL_E_NOMEM;
        memcpy(injected_resolution_message, "reload terminal resolver error",
               sizeof("reload terminal resolver error"));
        inject_resolution_without_error = false;
        inject_resolution_failure = true;
    } else {
        mprotect_fail_at = mprotect_attempts + 1;
        mprotect_injected_failures = 0;
    }
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    ll_symbol_addr *failed_lookup = lookupSymbol_(
        &owner, (ll_symbol_name *)"macho_load_symbol");
    ll_mutex_unlock(&owner.linker_mutex);
    if (terminal_resolution) inject_resolution_failure = false;
    else reset_mprotect_injection();
    CHECK(failed_lookup == NULL && old->resolution_failed &&
              owner.error_status == (terminal_resolution ? LL_E_NOMEM : LL_E_IO),
          "lazy lookup did not quarantine the old loaded object");
    CHECK(terminal_resolution
              ? resolution_backend_calls == before_backend + 1
              : mprotect_injected_failures == 1,
          "lazy resolution did not reach exactly one injected failure");

    ObjectCode *saved_objects = owner.gc.objects;
    old->next = saved_objects;
    owner.gc.objects = old;
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    int blocked_global_resolve = resolveObjs_(&owner);
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(blocked_global_resolve == 0 && owner.error_status == LL_E_STATE,
          "global resolve did not remain blocked while quarantine was active");
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    int unloaded = unloadObj_(&owner, (pathchar *)path, false);
    ll_mutex_unlock(&owner.linker_mutex);
    owner.gc.objects = saved_objects;
    old->next = NULL;
    CHECK(unloaded != 0 && old->status == OBJECT_UNLOADED,
          "normal unload did not retire the quarantined object");
    CHECK(old->resolution_failed,
          "normal unload cleared the old object's quarantine marker");
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    int reloaded = loadObj_(&owner, path);
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(reloaded != 0,
          "fresh copy could not load after quarantined object unload");
    ObjectCode *fresh = find_loaded_path(path);
    CHECK(fresh != NULL && fresh != old,
          "reload did not create a fresh object identity");
    CHECK(fresh != NULL && !fresh->resolution_failed,
          "freshly loaded copy inherited the old quarantine marker");
    ObjectCode *fresh_saved_objects = owner.gc.objects;
    unsigned before_fresh_resolve = mprotect_attempts;
    unsigned injected_before_fresh_resolve = mprotect_injected_failures;
    if (fresh != NULL) {
        fresh->next = fresh_saved_objects;
        owner.gc.objects = fresh;
    }
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    int fresh_resolved = resolveObjs_(&owner);
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(fresh_resolved != 0,
          "freshly loaded copy did not pass normal global resolution");
    CHECK(fresh != NULL && fresh->status == OBJECT_READY &&
              fresh->initializersRan,
          "freshly loaded copy did not reach its initialized READY state");
    CHECK(mprotect_attempts > before_fresh_resolve &&
              mprotect_injected_failures == injected_before_fresh_resolve,
          "fresh resolution did not perform real protection without injection");
    owner.gc.objects = fresh_saved_objects;
    if (fresh != NULL) fresh->next = NULL;
    ll_error_clear(&owner);
    ll_mutex_lock(&owner.linker_mutex);
    ll_symbol_addr *fresh_lookup = lookupSymbol_(
        &owner, (ll_symbol_name *)"macho_load_symbol");
    ll_mutex_unlock(&owner.linker_mutex);
    CHECK(fresh_lookup != NULL && fresh != NULL && !fresh->resolution_failed,
          "freshly loaded initialized copy did not expose its symbol");

    if (fresh != NULL) {
        dispose_test_object(fresh);
    }
    freeObjectCode(old);
    (void)unlink(path);
}

typedef struct {
    ll_word expected;
    bool found;
} DependencyKeySearch;

static bool has_dependency_key(void *data, ll_word key, const void *value)
{
    DependencyKeySearch *search = data;
    (void)value;
    if (key == search->expected) {
        search->found = true;
        return false;
    }
    return true;
}

static bool dependency_contains(ObjectCode *dependent, ObjectCode *provider)
{
    DependencyKeySearch search = {(ll_word)(uintptr_t)provider, false};
    /* HashSet keys are present with NULL values, so iteration is the oracle. */
    iterHashTable(dependent->dependencies, &search, has_dependency_key);
    return search.found;
}

static void run_dependency_closure_frames_probe(bool terminal_resolution)
{
    ObjectCode *provider = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0}, "closure-provider");
    ObjectCode *chain = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0}, "closure-chain");
    ObjectCode *cycle = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0}, "closure-cycle");
    ObjectCode *completed = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0}, "closure-completed");
    ObjectCode *independent = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0}, "closure-independent");
    ObjectCode *pre_registration = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0}, "closure-preregister");
    ObjectCode *new_dependent = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_NONE, 0}, "closure-new-dependent");
    ObjectCode *all[] = {provider, chain, cycle, completed, independent,
                         pre_registration, new_dependent};
    ObjectCode *saved_objects = owner.gc.objects;
    for (size_t i = 0; i < sizeof(all) / sizeof(all[0]); ++i) {
        CHECK(all[i] != NULL, "could not allocate dependency closure fixture");
        if (all[i] != NULL)
            CHECK(ll_init_object(all[i]) != 0,
                  "could not initialize dependency closure fixture");
    }
    if (provider == NULL || chain == NULL || cycle == NULL ||
        completed == NULL || independent == NULL ||
        pre_registration == NULL || new_dependent == NULL)
        goto cleanup;

    insertHashSet(chain->dependencies, (ll_word)(uintptr_t)provider);
    insertHashSet(cycle->dependencies, (ll_word)(uintptr_t)chain);
    insertHashSet(chain->dependencies, (ll_word)(uintptr_t)cycle);
    insertHashSet(completed->dependencies, (ll_word)(uintptr_t)provider);
    insertHashSet(pre_registration->dependencies,
                  (ll_word)(uintptr_t)provider);
    CHECK(dependency_contains(chain, provider),
          "NULL-valued dependency set lost its provider key");
    CHECK(lookupHashTable(chain->dependencies,
                          (ll_word)(uintptr_t)provider) == NULL,
          "dependency fixture unexpectedly stored a non-NULL set value");

    provider->status = OBJECT_NEEDED;
    chain->status = OBJECT_NEEDED;
    cycle->status = OBJECT_NEEDED;
    completed->status = OBJECT_READY;
    independent->status = OBJECT_READY;
    pre_registration->status = OBJECT_NEEDED;
    new_dependent->status = OBJECT_NEEDED;
    provider->next = chain;
    chain->next = cycle;
    cycle->next = completed;
    completed->next = independent;
    independent->next = NULL;
    owner.gc.objects = provider;

    struct ll_failure_scope outer;
    struct ll_failure_scope preregistered_frame;
    ll_error_clear(&owner);
    ll_failure_begin(&owner, &outer, provider);
    ll_failure_begin(&owner, &preregistered_frame, pre_registration);
    ll_error_set(&owner, terminal_resolution ? LL_E_FORMAT : LL_E_IO,
                 terminal_resolution
                     ? "primary terminal resolution diagnostic survives"
                     : "primary protection diagnostic survives");
    char primary[LL_ERROR_CAPACITY];
    memcpy(primary, owner.error_message, sizeof(primary));
    if (terminal_resolution) ll_failure_resolution(provider);
    else ll_failure_protection(provider);
    CHECK(provider->resolution_failed && chain->resolution_failed &&
              cycle->resolution_failed && completed->resolution_failed,
          "failure did not mark chain, cycle, and completed dependent");
    CHECK(pre_registration->resolution_failed,
          "failure did not mark the unregistered active frame");
    CHECK(!independent->resolution_failed &&
              independent->status == OBJECT_READY,
          "independent completed sibling was changed by quarantine");
    ll_error_set(&owner, LL_E_STATE, "secondary unwind diagnostic");
    ll_failure_end(&owner, &preregistered_frame);
    CHECK(memcmp(owner.error_message, primary, sizeof(primary)) == 0 &&
              owner.error_status == (terminal_resolution ? LL_E_FORMAT : LL_E_IO),
          "inner frame unwind did not restore all primary diagnostic bytes");
    ll_failure_end(&owner, &outer);
    CHECK(owner.failure_scopes == NULL,
          "failure frames were not cleared after nested unwind");
    CHECK(memcmp(owner.error_message, primary, sizeof(primary)) == 0 &&
              owner.error_status == (terminal_resolution ? LL_E_FORMAT : LL_E_IO),
          "outer frame unwind did not restore all primary diagnostic bytes");

    insertHashSet(new_dependent->dependencies,
                  (ll_word)(uintptr_t)provider);
    CHECK(dependency_contains(new_dependent, provider),
          "new dependent edge was not stored in the NULL-valued set");
    ll_failure_inherit(new_dependent, provider);
    CHECK(new_dependent->resolution_failed,
          "new dependent of an already quarantined provider stayed usable");
    CHECK(!ll_failure_usable(new_dependent) &&
              owner.error_status == LL_E_STATE,
          "later use of newly dependent object did not report LL_E_STATE");

    /* Exercise the pre-registration discard contract while its edge is live. */
    ll_discard_object(pre_registration);
    bool retained = false;
    for (ObjectCode *oc = owner.gc.loaded_objects; oc != NULL;
         oc = oc->next_loaded_object)
        if (oc == pre_registration) retained = true;
    CHECK(retained,
          "quarantined pre-registration dependency was freed before normal unload");
    owner.gc.objects = saved_objects;

cleanup:
    owner.gc.objects = saved_objects;
    for (size_t i = 0; i < sizeof(all) / sizeof(all[0]); ++i) {
        ObjectCode *oc = all[i];
        if (oc == NULL) continue;
        oc->resolution_failed = false;
        if (oc == pre_registration && owner.gc.loaded_objects != NULL) {
            bool retained = false;
            for (ObjectCode *loaded = owner.gc.loaded_objects; loaded != NULL;
                 loaded = loaded->next_loaded_object)
                if (loaded == oc) retained = true;
            if (retained) {
                dispose_test_object(oc);
                continue;
            }
        }
        ll_discard_object(oc);
    }
}

static void run_common_alias_quarantine_probe(void)
{
    ObjectCode *provider = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_COMMON, 64},
        "common-alias-provider");
    ObjectCode *alias = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_COMMON, 32},
        "common-alias-dependent");
    ObjectCode *late_alias = make_object(
        (FixtureSpec){PROFILE_EMPTY, SYMBOLS_COMMON, 32},
        "common-alias-late-dependent");
    ObjectCode *saved_objects = owner.gc.objects;
    CHECK(provider != NULL && alias != NULL && late_alias != NULL,
          "could not create COMMON alias fixtures");
    if (provider == NULL || alias == NULL || late_alias == NULL) goto cleanup;

    ll_error_clear(&owner);
    CHECK(ll_init_object(provider) != 0 && loadOc(provider) != 0,
          "COMMON alias provider did not load");
    RtsSymbolInfo *provider_symbol = lookupStrHashTable(
        owner.symhash, "macho_load_symbol");
    CHECK(provider_symbol != NULL && provider_symbol->owner == provider,
          "COMMON alias provider did not own its hash entry");
    CHECK(provider->info->common_mem != NULL,
          "COMMON alias provider did not allocate common storage");
    ll_error_clear(&owner);
    CHECK(ll_init_object(alias) != 0 && loadOc(alias) != 0,
          "healthy COMMON alias could not initialize");
    CHECK(dependency_contains(alias, provider),
          "healthy COMMON alias did not record its owner dependency");
    RtsSymbolInfo *common = lookupStrHashTable(owner.symhash,
                                                "macho_load_symbol");
    CHECK(common != NULL && alias->info->macho_symbols[0].addr ==
              (common != NULL ? common->value : NULL),
          "healthy COMMON alias did not reuse provider storage");

    provider->next = alias;
    alias->next = saved_objects;
    owner.gc.objects = provider;
    struct ll_failure_scope scope;
    ll_error_clear(&owner);
    ll_failure_begin(&owner, &scope, provider);
    ll_error_set(&owner, LL_E_IO, "COMMON provider protection failure");
    ll_failure_protection(provider);
    CHECK(provider->resolution_failed && alias->resolution_failed,
          "provider protection failure did not quarantine its COMMON alias");
    ll_failure_end(&owner, &scope);
    CHECK(owner.failure_scopes == NULL,
          "COMMON alias failure frame remained active after unwind");

    ll_error_clear(&owner);
    int late_initialized = ll_init_object(late_alias);
    int late_loaded = late_initialized ? loadOc(late_alias) : 0;
    CHECK(late_initialized != 0 && late_loaded == 0 &&
              late_alias->resolution_failed,
          "new COMMON dependent reused storage from a failed owner");
    CHECK(owner.error_status == LL_E_STATE,
          "new COMMON dependent of a failed owner did not report LL_E_STATE");
    CHECK(dependency_contains(late_alias, provider),
          "rejected late COMMON dependent did not record its owner edge");
    owner.gc.objects = saved_objects;
    provider->next = NULL;
    alias->next = NULL;

cleanup:
    owner.gc.objects = saved_objects;
    ObjectCode *objects[] = {provider, alias, late_alias};
    for (size_t i = 0; i < sizeof(objects) / sizeof(objects[0]); ++i) {
        if (objects[i] == NULL) continue;
        objects[i]->resolution_failed = false;
        ll_discard_object(objects[i]);
    }
}

static void run_m32_retry_gap_probe(void)
{
#if defined(NEED_M32)
    m32_allocator *alloc = m32_allocator_new_for(&owner, true);
    CHECK(alloc != NULL, "could not create retry-gap m32 allocator");
    if (alloc == NULL) return;
    CHECK(m32_alloc_for(&owner, alloc, page_size, 8) != NULL &&
              m32_alloc_for(&owner, alloc, page_size, 8) != NULL,
          "could not create retry-gap RX pages");
    mprotect_fail_at = mprotect_attempts + 2;
    mprotect_injected_failures = 0;
    ll_error_clear(&owner);
    bool first = m32_allocator_flush_for(&owner, alloc);
    CHECK(owner.error_status == LL_E_IO,
          "first failed RX flush did not preserve LL_E_IO");
    reset_mprotect_injection();
    unsigned after_failure = mprotect_attempts;
    ll_error_clear(&owner);
    bool second = m32_allocator_flush_for(&owner, alloc);
    CHECK(!first, "retry-gap fixture did not fail the first flush");
    CHECK(!second, "failed RX allocator accepted a second flush");
    CHECK(owner.error_status == LL_E_STATE,
          "failed RX allocator retry did not report LL_E_STATE");
    CHECK(mprotect_attempts == after_failure,
          "failed RX allocator retried protection after failure");
    ll_error_clear(&owner);
    void *rejected_allocation = m32_alloc_for(&owner, alloc, 8, 8);
    CHECK(rejected_allocation == NULL,
          "failed RX allocator accepted a new allocation");
    CHECK(owner.error_status == LL_E_STATE,
          "allocation from failed RX allocator did not report LL_E_STATE");
    CHECK(mprotect_attempts == after_failure,
          "allocation rejection attempted operating-system protection");
    m32_allocator_free_for(&owner, alloc);
    drain_m32_free_pool();
#else
    (void)puts("GAP_PROBE m32 retry: unavailable without NEED_M32");
#endif
}

static void run_cleanup_error_overwrite_gap_probe(void)
{
#if defined(NEED_M32)
    char primary_message[LL_ERROR_CAPACITY];
    ObjectCode *first = make_object(
        (FixtureSpec){PROFILE_REGULAR, SYMBOLS_EXPORT, 0},
        "protection-cleanup-first-owner");
    ObjectCode *second = make_object(
        (FixtureSpec){PROFILE_REGULAR, SYMBOLS_EXPORT, 0},
        "protection-cleanup-duplicate-owner");
    if (first == NULL || second == NULL) {
        if (first != NULL) ll_discard_object(first);
        if (second != NULL) ll_discard_object(second);
        return;
    }
    ll_error_clear(&owner);
    CHECK(ll_init_object(first) != 0 && loadOc(first) != 0 &&
              ocTryLoad(first) != 0,
          "could not load primary cleanup-error owner");
    ll_error_clear(&owner);
    int second_initialized = ll_init_object(second);
    int second_loaded = second_initialized ? loadOc(second) : 0;
    CHECK(second_initialized != 0 && second_loaded == 0 &&
              owner.error_status == LL_E_DUPSYM,
          "duplicate load did not establish primary DUPSYM status");
    memcpy(primary_message, owner.error_message, sizeof(primary_message));
    CHECK(m32_alloc_for(&owner, second->rw_m32, page_size, 8) != NULL,
          "could not create RW allocation for cleanup-error probe");
    mprotect_fail_at = mprotect_attempts + 1;
    mprotect_injected_failures = 0;
    ll_discard_object(second);
    reset_mprotect_injection();
    CHECK(mprotect_injected_failures == 1,
          "cleanup-error probe did not inject an RW release failure");
    CHECK(owner.error_status == LL_E_DUPSYM,
          "RW cleanup replaced the primary DUPSYM status");
    CHECK(memcmp(owner.error_message, primary_message,
                 sizeof(primary_message)) == 0,
          "RW cleanup changed the primary DUPSYM diagnostic bytes");
    drain_m32_free_pool();
    ll_discard_object(first);
#else
    (void)puts("GAP_PROBE cleanup status: unavailable without NEED_M32");
#endif
}

#include "macho_terminal_resolution_cases.h"

static void run_protection_gap_probes(void)
{
    Snapshot before = snapshot();
    run_dependency_closure_frames_probe(false);
    CHECK(active_mappings() == before.active_maps,
          "dependency closure frame probe left native mappings active");
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded,
          "dependency closure frame probe left an active object root");
    before = snapshot();
    run_failed_resolve_lookup_gap_probe(false);
    CHECK(active_mappings() == before.active_maps,
          "failed resolve quarantine probe left native mappings active");
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded,
          "failed resolve quarantine probe left an active object root");
    CHECK(lookupStrHashTable(owner.symhash, "macho_load_symbol") == NULL,
          "failed resolve quarantine probe left its symbol registered");
    before = snapshot();
    run_common_alias_quarantine_probe();
    CHECK(active_mappings() == before.active_maps,
          "COMMON alias quarantine probe left native mappings active");
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded,
          "COMMON alias quarantine probe left an active object root");
    before = snapshot();
    run_ordinary_missing_retry_probe();
    CHECK(active_mappings() == before.active_maps,
          "ordinary missing retry probe left native mappings active");
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded,
          "ordinary missing retry probe left an active object root");
    before = snapshot();
    run_unload_reload_quarantine_probe(false);
    CHECK(active_mappings() == before.active_maps,
          "unload/reload quarantine probe left native mappings active");
    CHECK(owner.gc.loaded_objects == before.loaded,
          "unload/reload quarantine probe left an active loaded object");
    before = snapshot();
    run_preregister_weak_failure(false);
    CHECK(active_mappings() == before.active_maps,
          "pre-registration quarantine probe left native mappings active");
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded,
          "pre-registration quarantine probe left an active object root");
    CHECK(owner.symhash == NULL ||
              lookupStrHashTable(owner.symhash, "macho_load_symbol") == NULL,
          "pre-registration quarantine probe left its symbol registered");
    before = snapshot();
    run_m32_retry_gap_probe();
    CHECK(active_mappings() == before.active_maps,
          "M32 retry quarantine probe left native mappings active");
    before = snapshot();
    run_cleanup_error_overwrite_gap_probe();
    CHECK(active_mappings() == before.active_maps,
          "cleanup error probe left native mappings active");
    CHECK(owner.gc.objects == before.objects &&
              owner.gc.loaded_objects == before.loaded,
          "cleanup error probe left an active object root");
}
