#ifndef LIBLINK_TEST_ELF_RELOCATION_ERRORS_HELPERS_H
#define LIBLINK_TEST_ELF_RELOCATION_ERRORS_HELPERS_H

enum { SECTION_COUNT = 5, OWNER_COUNT = 2, SLOT_OFFSET = 128,
       EXTRA_OFFSET = 512, GUARDED_BYTES = 16 };

typedef struct {
    ll_elf_target_shdr sections[SECTION_COUNT];
    ll_elf_target_sym symbols[2];
    char strings[8];
    ll_elf_target_rela relocation;
} RelaImage;

typedef struct {
    unsigned log_calls;
    unsigned fatal_calls;
} HostState;

typedef struct {
    ll_linker linker;
    HostState host;
} Owner;

static unsigned failures;
static Owner owners[OWNER_COUNT];
static HostState default_host_state;
static ll_status default_status;
static char default_message[LL_ERROR_CAPACITY];

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "elf_relocation_errors:%d: %s\n", \
                      __LINE__, (message)); \
        ++failures; \
    } \
} while (0)

static void *test_malloc(void *ctx, size_t size, const char *what)
{
    (void)ctx;
    (void)what;
    return malloc(size == 0 ? 1 : size);
}

static void *test_calloc(void *ctx, size_t count, size_t size,
                         const char *what)
{
    (void)ctx;
    (void)what;
    return calloc(count == 0 ? 1 : count, size == 0 ? 1 : size);
}

static void *test_realloc(void *ctx, void *pointer, size_t size,
                          const char *what)
{
    (void)ctx;
    (void)what;
    return realloc(pointer, size == 0 ? 1 : size);
}

static void test_free(void *ctx, void *pointer)
{
    (void)ctx;
    free(pointer);
}

static void test_log(void *ctx, int kind, const char *format, va_list args)
{
    HostState *host = ctx;
    (void)kind;
    (void)format;
    (void)args;
    ++host->log_calls;
}

static _Noreturn void test_fatal(void *ctx, const char *format, va_list args)
{
    HostState *host = ctx;
    (void)format;
    (void)args;
    ++host->fatal_calls;
    _Exit(99);
}

static size_t test_page_size(void *ctx)
{
    (void)ctx;
    long page_size = sysconf(_SC_PAGESIZE);
    return page_size > 0 ? (size_t)page_size : 4096u;
}

static ll_compat_host_ops host_ops(HostState *state)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = state;
    ops.malloc_ = test_malloc;
    ops.calloc_ = test_calloc;
    ops.realloc_ = test_realloc;
    ops.free_ = test_free;
    ops.log = test_log;
    ops.fatal = test_fatal;
    ops.page_size = test_page_size;
    return ops;
}

static void init_owners(void)
{
    ll_compat_host_ops default_ops = host_ops(&default_host_state);
    ll_compat_config config = {.always_pic = true};
    ll_install_host(&default_ops, &config);
    ll_default_linker.error_status = LL_E_STATE;
    int message_length = snprintf(ll_default_linker.error_message,
                                  sizeof(ll_default_linker.error_message),
                                  "%s", "default owner sentinel");
    CHECK(message_length > 0
              && (size_t)message_length < sizeof(ll_default_linker.error_message),
          "could not initialize default owner sentinel");
    default_status = ll_default_linker.error_status;
    memcpy(default_message, ll_default_linker.error_message, sizeof(default_message));

    for (size_t i = 0; i < OWNER_COUNT; ++i) {
        ll_compat_host_ops ops = host_ops(&owners[i].host);
        ll_install_host_for(&owners[i].linker, &ops, &config);
        ll_mutex_init_for(&owners[i].linker.services,
                          &owners[i].linker.linker_mutex);
    }
}

static void check_default_unchanged(void)
{
    CHECK(ll_default_linker.error_status == default_status,
          "default owner's status changed");
    CHECK(memcmp(ll_default_linker.error_message, default_message,
                 sizeof(default_message)) == 0,
          "default owner's diagnostic changed");
    CHECK(default_host_state.log_calls == 0,
          "default owner's log callback was called");
    CHECK(default_host_state.fatal_calls == 0,
          "default owner's fatal callback was called");
}

static void *map_target(size_t bytes)
{
    void *memory = mmap(NULL, bytes, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANONYMOUS | MAP_32BIT, -1, 0);
    if (memory == MAP_FAILED) return NULL;
    if ((uintptr_t)memory > (uintptr_t)INT32_MAX - bytes) {
        (void)munmap(memory, bytes);
        return NULL;
    }
    return memory;
}

static void *map_far_extras(size_t bytes, void *target)
{
    void *memory = mmap(NULL, bytes, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (memory == MAP_FAILED) return NULL;
    uintptr_t a = (uintptr_t)memory;
    uintptr_t b = (uintptr_t)target;
    uintptr_t distance = a > b ? a - b : b - a;
    if (distance <= (uintptr_t)INT32_MAX) {
        (void)munmap(memory, bytes);
        return NULL;
    }
    return memory;
}

static void init_image(RelaImage *image, unsigned type, uint64_t symbol_value,
                       bool external_symbol, int64_t addend)
{
    memset(image, 0, sizeof(*image));
    image->sections[1].sh_type = LL_ELF_SHT_PROGBITS;
    image->sections[2].sh_type = LL_ELF_SHT_SYMTAB;
    image->sections[2].sh_link = 3;
    image->sections[2].sh_offset = offsetof(RelaImage, symbols);
    image->sections[3].sh_type = LL_ELF_SHT_STRTAB;
    image->sections[3].sh_offset = offsetof(RelaImage, strings);
    image->sections[4].sh_type = LL_ELF_SHT_RELA;
    image->sections[4].sh_link = 2;
    image->sections[4].sh_info = 1;
    image->sections[4].sh_offset = offsetof(RelaImage, relocation);
    image->sections[4].sh_size = sizeof(image->relocation);
    image->strings[1] = 's';
    image->strings[2] = '\0';
    image->symbols[1].st_name = 1;
    image->symbols[1].st_info = ELF64_ST_INFO(
        external_symbol ? LL_ELF_STB_GLOBAL : LL_ELF_STB_LOCAL, LL_ELF_STT_OBJECT);
    image->symbols[1].st_shndx = 1;
    image->symbols[1].st_value = symbol_value;
    image->relocation.r_info = ELF64_R_INFO(1, type);
    image->relocation.r_addend = addend;
}

static int run_relocation(Owner *owner, RelaImage *image, unsigned char *target,
                          SymbolExtra *extras)
{
    Owner *peer = owner == &owners[0] ? &owners[1] : &owners[0];
    ll_status peer_status = peer->linker.error_status;
    char peer_message[LL_ERROR_CAPACITY];
    unsigned peer_logs = peer->host.log_calls;
    unsigned peer_fatals = peer->host.fatal_calls;
    unsigned default_logs = default_host_state.log_calls;
    unsigned default_fatals = default_host_state.fatal_calls;
    memcpy(peer_message, peer->linker.error_message, sizeof(peer_message));
    Section sections[2] = {0};
    ObjectCode object = {0};
    object.L = &owner->linker;
    object.fileName = (ll_compat_pathchar *)"relocation-fixture.o";
    object.image = (char *)image;
    object.n_sections = 2;
    object.sections = sections;
#if LL_ELF_HAS_SHN_XINDEX
    object.shndx_table = NULL;
#endif
    object.symbol_extras = extras;
    object.symbol_extras_exec = (uintptr_t)extras;
    object.first_symbol_extra = 1;
    object.n_symbol_extras = extras == NULL ? 0 : 1;
    object.symbol_extra_stride = sizeof(SymbolExtra);
    initProddableBlockSet(&object.proddables);
    sections[1].start = target;
    sections[1].exec_start = (uintptr_t)target;
    sections[1].size = GUARDED_BYTES;
    sections[1].kind = SECTIONKIND_CODE_OR_RODATA;
    addProddableBlock(&owner->linker, &object.proddables, target,
                      GUARDED_BYTES);

    ll_mutex_lock_for(&owner->linker, &owner->linker.linker_mutex);
    ll_error_clear(&owner->linker);
    int result = do_Elf_Rela_relocations(&object, (char *)image,
                                         image->sections, 4);
    ll_mutex_unlock_for(&owner->linker, &owner->linker.linker_mutex);
    freeProddableBlocks(&owner->linker, &object.proddables);
    CHECK(peer->linker.error_status == peer_status,
          "relocation changed the other owner's status");
    CHECK(memcmp(peer->linker.error_message, peer_message,
                 sizeof(peer_message)) == 0,
          "relocation changed the other owner's diagnostic");
    CHECK(peer->host.log_calls == peer_logs,
          "relocation used the other owner's log callback");
    CHECK(peer->host.fatal_calls == peer_fatals,
          "relocation used the other owner's fatal callback");
    CHECK(default_host_state.log_calls == default_logs,
          "relocation used the default owner's log callback");
    CHECK(default_host_state.fatal_calls == default_fatals,
          "relocation used the default owner's fatal callback");
    check_default_unchanged();
    return result;
}

static void check_error(Owner *owner, ll_status expected, const char *label)
{
    CHECK(owner->linker.error_status == expected, label);
    CHECK(owner->linker.error_message[0] != '\0', label);
    CHECK(owner->host.fatal_calls == 0, "relocation called fatal callback");
    CHECK(owner->host.log_calls > 0, "error did not use the owning log callback");
    check_default_unchanged();
}

static void run_range_case(unsigned type, uint64_t symbol_value,
                           bool needs_far_extra, const char *label,
                           void *target, void *far_extras, void *near_extras,
                           size_t page_size)
{
    RelaImage image;
    unsigned char *slot = (unsigned char *)target + SLOT_OFFSET;
    unsigned char before[GUARDED_BYTES];
    SymbolExtra *extras = needs_far_extra ? (SymbolExtra *)far_extras
                                          : (SymbolExtra *)near_extras;
    init_image(&image, type, symbol_value, false, 0);
    memset(slot, 0x5a, sizeof(before));
    memcpy(before, slot, sizeof(before));
    int result = run_relocation(&owners[0], &image, slot, extras);
    CHECK(result == 0, label);
    check_error(&owners[0], LL_E_RELOC_RANGE, label);
    CHECK(memcmp(slot, before, sizeof(before)) == 0,
          "failed relocation changed payload or adjacent canary");
    CHECK(page_size > SLOT_OFFSET + sizeof(before), "target fixture too small");
}

static void run_valid_64(unsigned type, int64_t addend, void *target,
                         uint64_t expected_value, const char *label)
{
    RelaImage image;
    unsigned char *slot = (unsigned char *)target + SLOT_OFFSET;
    unsigned char adjacent[8];
    uint64_t observed = 0;
    init_image(&image, type, 0x123, false, addend);
    memset(slot, 0x3c, 16);
    memcpy(adjacent, slot + sizeof(observed), sizeof(adjacent));
    int result = run_relocation(&owners[1], &image, slot, NULL);
    memcpy(&observed, slot, sizeof(observed));
    CHECK(result == 1, label);
    CHECK(owners[1].linker.error_status == LL_OK, label);
    CHECK(observed == expected_value, label);
    CHECK(memcmp(slot + sizeof(observed), adjacent, sizeof(adjacent)) == 0,
          "64-bit relocation changed adjacent canary");
    check_default_unchanged();
}

static void run_valid_32(unsigned type, void *target, const char *label)
{
    RelaImage image;
    unsigned char *slot = (unsigned char *)target + SLOT_OFFSET;
    unsigned char upper_canary[4];
    uint32_t observed = 0;
    uint32_t expected = type == COMPAT_R_X86_64_PC32
                            ? UINT32_C(0x144)
                            : (uint32_t)((uintptr_t)slot + 0x144u);
    init_image(&image, type, 0x123, false, 0x21);
    memset(slot, 0xc3, 8);
    memcpy(upper_canary, slot + sizeof(observed), sizeof(upper_canary));
    int result = run_relocation(&owners[1], &image, slot, NULL);
    memcpy(&observed, slot, sizeof(observed));
    CHECK(result == 1, label);
    CHECK(owners[1].linker.error_status == LL_OK, label);
    CHECK(observed == expected, label);
    CHECK(memcmp(slot + sizeof(observed), upper_canary, sizeof(upper_canary)) == 0,
          "32-bit relocation changed adjacent upper bytes");
}

static void run_unsupported(unsigned type, bool external_symbol,
                            const char *label, void *target)
{
    RelaImage image;
    unsigned char *slot = (unsigned char *)target + SLOT_OFFSET;
    unsigned char before[GUARDED_BYTES];
    init_image(&image, type, 0, external_symbol, 0);
    memset(slot, 0xa7, sizeof(before));
    memcpy(before, slot, sizeof(before));
    int result = run_relocation(&owners[1], &image, slot, NULL);
    CHECK(result == 0, label);
    check_error(&owners[1], LL_E_UNSUPPORTED, label);
    CHECK(memcmp(slot, before, sizeof(before)) == 0,
          "unsupported relocation changed payload or canary");
}

static void run_valid_extra(unsigned type, bool use_jump_island,
                            void *target, void *near_extras)
{
    RelaImage image;
    unsigned char *slot = (unsigned char *)target + SLOT_OFFSET;
    SymbolExtra *extras = (SymbolExtra *)near_extras;
    uint64_t symbol_value = use_jump_island ? UINT64_C(0x100000000) : 0x123;
    init_image(&image, type, symbol_value, false, 0);
    memset(slot, 0x61, 16);
    int result = run_relocation(&owners[0], &image, slot, extras);
    CHECK(result == 1, "valid symbol-extra relocation failed");
    CHECK(owners[0].linker.error_status == LL_OK,
          "valid symbol-extra relocation set an error");
    if (type == COMPAT_R_X86_64_GOTPCREL) {
        ll_elf64_sword observed = 0;
        ll_elf64_sword expected = (ll_elf64_sword)((uintptr_t)&extras[0].addr
                                               - (uintptr_t)slot);
        memcpy(&observed, slot, sizeof(observed));
        CHECK(observed == expected, "GOTPCREL payload was incorrect");
    } else if (use_jump_island) {
        ll_elf64_sword observed = 0;
        ll_elf64_sword expected = (ll_elf64_sword)((uintptr_t)&extras[0].jumpIsland
                                               - (uintptr_t)slot);
        memcpy(&observed, slot, sizeof(observed));
        CHECK(observed == expected, "PLT32 fallback payload was incorrect");
    }
    check_default_unchanged();
}


#endif
