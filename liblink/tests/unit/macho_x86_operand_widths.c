#define main macho_x86_status_original_main
#include "macho_x86_error_status.c"
#undef main

#if !defined(darwin_HOST_OS) || !defined(x86_64_HOST_ARCH) \
    || !defined(OBJFORMAT_MACHO)
#error "macho_x86_operand_widths requires generated x86_64 Darwin configuration"
#endif

static unsigned operand_cases;

static void separate_site_cases(void)
{
    _Alignas(16) unsigned char source[16] = {0};
    _Alignas(16) unsigned char target[16] = {0};
    SymbolExtra extra = {0};
    MachORelocationInfo relocation = {0};
    MachOSection macho[2] = {{0}, {0}};
    struct SectionFormatInfo info[2] = {{0}, {0}};
    struct ObjectCodeFormatInfo format = {0};
    MachONList nlist = {0};
    MachOSymbol symbol = {0};
    Section sections[2] = {{0}, {0}};
    ObjectCode oc = {0};
    uint32_t word;
    uint64_t wide;

    sections[0].start = source; sections[0].exec_start = 0x10000000;
    sections[0].size = sizeof(source); sections[0].info = &info[0];
    sections[1].start = target; sections[1].exec_start = 0x10001000;
    sections[1].size = sizeof(target); sections[1].info = &info[1];
    info[0].macho_section = &macho[0]; info[0].relocation_info = &relocation;
    info[1].macho_section = &macho[1]; macho[0].nreloc = 1;
    nlist.n_type = N_SECT; symbol.nlist = &nlist;
    symbol.name = (ll_symbol_name *)"_separate_site";
    symbol.addr = (ll_symbol_addr *)(uintptr_t)0x10003000;
    format.macho_symbols = &symbol; format.n_macho_symbols = 1;
    oc.L = &owner; oc.fileName = (pathchar *)"separate-site.o";
    oc.info = &format; oc.sections = sections; oc.n_sections = 2;
    oc.symbol_extras = &extra; oc.symbol_extras_exec = 0x10002000;
    oc.n_symbol_extras = 1; oc.symbol_extra_stride = sizeof(extra);
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(&owner, &oc.proddables, source, sizeof(source));
    addProddableBlock(&owner, &oc.proddables, target, sizeof(target));

    relocation.r_length = 2; relocation.r_extern = 1; relocation.r_pcrel = 1;
    relocation.r_type = X86_64_RELOC_SIGNED;
    CHECK(relocateSection(&oc, 0), "separate site SIGNED failed");
    memcpy(&word, source, sizeof(word));
    CHECK(word == 0x2ffc, "SIGNED used work address as place");
    memset(source, 0, sizeof(source));
    relocation.r_type = X86_64_RELOC_GOT;
    CHECK(relocateSection(&oc, 0), "separate site GOT failed");
    memcpy(&word, source, sizeof(word));
    CHECK(word == 0x1ffc && extra.addr == 0x10003000,
          "GOT used work address or wrote the wrong target");
    memset(source, 0, sizeof(source));
    source[0] = 4; relocation.r_length = 3; relocation.r_extern = 0;
    relocation.r_pcrel = 0; relocation.r_symbolnum = 2;
    relocation.r_type = X86_64_RELOC_UNSIGNED;
    CHECK(relocateSection(&oc, 0), "separate site internal target failed");
    memcpy(&wide, source, sizeof(wide));
    CHECK(wide == 0x10001004, "internal target used work address");
    freeProddableBlocks(&owner, &oc.proddables);
}

static void little_endian_bytes(unsigned char *out, uint64_t value, size_t width)
{
    for (size_t i = 0; i < width; ++i)
        out[i] = (unsigned char)(value >> (8 * i));
}

static void operand_case(unsigned length, unsigned offset, bool internal,
                          bool invalid_span)
{
    static const uint64_t inputs[] = {
        UINT64_C(0xe1), UINT64_C(0xe2b1), UINT64_C(0xf1e2d3c4),
        UINT64_C(0xf1e2d3c4b5a69788)
    };
    static const uint64_t symbols[] = {
        UINT64_C(0x44), UINT64_C(0x3344), UINT64_C(0x33445566),
        UINT64_C(0x123456789abcdef1)
    };
    /* Literal modular sums also check carry and write narrowing. */
    static const uint64_t sums[] = {
        UINT64_C(0x25), UINT64_C(0x15f5), UINT64_C(0x2527292a),
        UINT64_C(0x04172a3d50637679)
    };
    _Alignas(16) unsigned char source[64];
    _Alignas(16) unsigned char target[64];
    unsigned char expected[sizeof(source)];
    unsigned char target_before[sizeof(target)];
    const size_t width = (size_t)1 << length;
    unsigned char *start = source + 16;
    size_t span = invalid_span ? offset + width - 1 : 32;
    MachORelocationInfo relocation = {0};
    MachOSection macho[2] = {{0}, {0}};
    struct SectionFormatInfo info[2] = {{0}, {0}};
    struct ObjectCodeFormatInfo format = {0};
    MachONList nlist = {0};
    MachOSymbol symbol = {0};
    Section sections[2] = {{0}, {0}};
    ObjectCode oc = {0};
    uint64_t input = inputs[length];
    uint64_t result_value = sums[length];
    size_t live_before = owner_host.live;

    memset(source, 0xa5, sizeof(source));
    memset(target, 0x5a, sizeof(target));
    memcpy(target_before, target, sizeof(target));
    if (internal) {
        /* A mistaken sign extension changes the full computed target before
         * the final narrow store, so the target-span check distinguishes it. */
        CHECK(length == 1 || length == 2, "invalid zero-extension control");
        input = length == 1 ? UINT64_C(0xf123) : UINT64_C(0xf1234523);
        macho[1].addr = length == 1 ? UINT64_C(0xf100) : UINT64_C(0xf1234500);
        result_value = (uintptr_t)(target + 0x23);
    }
    little_endian_bytes(start + offset, input, width);
    memcpy(expected, source, sizeof(expected));
    if (!invalid_span)
        little_endian_bytes(expected + 16 + offset, result_value, width);

    relocation.r_address = (int32_t)offset;
    relocation.r_symbolnum = internal ? 2 : 0;
    relocation.r_length = length;
    relocation.r_extern = !internal;
    relocation.r_type = X86_64_RELOC_UNSIGNED;
    macho[0].nreloc = 1;
    info[0].macho_section = &macho[0];
    info[0].relocation_info = &relocation;
    info[1].macho_section = &macho[1];
    sections[0].start = start;
    sections[0].exec_start = (uintptr_t)start;
    sections[0].size = span;
    sections[0].info = &info[0];
    sections[1].start = target;
    sections[1].exec_start = (uintptr_t)target;
    sections[1].size = sizeof(target);
    sections[1].info = &info[1];
    nlist.n_type = N_SECT;
    symbol.nlist = &nlist;
    symbol.name = (ll_symbol_name *)"_operand_width_symbol";
    symbol.addr = (ll_symbol_addr *)(uintptr_t)symbols[length];
    format.macho_symbols = &symbol;
    format.n_macho_symbols = 1;
    oc.L = &owner;
    oc.fileName = (pathchar *)"operand-width-control.o";
    oc.info = &format;
    oc.sections = sections;
    oc.n_sections = 2;
    initProddableBlockSet(&oc.proddables);
    addProddableBlock(&owner, &oc.proddables, start, span);
    addProddableBlock(&owner, &oc.proddables, target, sizeof(target));

    ll_error_clear(&owner);
    ll_mutex_lock_for(&owner, &owner.linker_mutex);
    int result = relocateSection(&oc, 0);
    ll_mutex_unlock_for(&owner, &owner.linker_mutex);
    CHECK(result == (invalid_span ? 0 : 1), "operand case returned wrong result");
    CHECK(owner.error_status == (invalid_span ? LL_E_FORMAT : LL_OK),
          "operand case returned wrong status");
    CHECK(memcmp(source, expected, sizeof(source)) == 0,
          "operand bytes or surrounding sentinels differ");
    CHECK(memcmp(target, target_before, sizeof(target)) == 0,
          "operand case modified its internal target storage");
    freeProddableBlocks(&owner, &oc.proddables);
    CHECK(owner_host.live == live_before, "operand case leaked host storage");
    ++operand_cases;
}

int main(void)
{
    ll_compat_host_ops default_ops = make_ops(&default_host);
    ll_compat_host_ops owner_ops = make_ops(&owner_host);
    ll_install_host(&default_ops, NULL);
    ll_install_host_for(&owner, &owner_ops, NULL);
    ll_error_set(&ll_default_linker, LL_E_NOT_FOUND, "%s", default_message);
    ll_init_linker(&owner, 0);
    separate_site_cases();

    for (unsigned length = 0; length < 4; ++length) {
        for (unsigned offset = 0; offset < 8; ++offset)
            operand_case(length, offset, false, false);
        /* Each invalid relocation is first/only, before any payload write. */
        operand_case(length, 1, false, true);
        operand_case(length, 3, false, true);
    }
    for (unsigned length = 1; length <= 2; ++length)
        for (unsigned offset = 0; offset < 8; ++offset)
            operand_case(length, offset, true, false);

    ll_exit_linker(&owner);
    CHECK(operand_cases == 56, "operand matrix case count changed");
    CHECK(owner_host.fatals == 0, "operand case reached fatal callback");
    CHECK(owner_host.live == 0, "operand test teardown leaked host allocations");
    CHECK(default_host.logs == 0 && default_host.fatals == 0,
          "operand cases used default owner callbacks");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND &&
              strcmp(ll_default_linker.error_message, default_message) == 0,
          "operand cases changed default owner diagnostic");
    if (failures != 0) return 1;
    (void)puts("Mach-O x86 operand width matrix: PASS (56 cases)");
    return 0;
}
