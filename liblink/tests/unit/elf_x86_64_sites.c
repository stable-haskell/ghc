/* Data-only tests of the real x86_64 ELF apply table. No code is executed. */
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "LinkerInternals.h"
#include "linker/elf_reloc_x86_64.h"
#include "liblink/elf.h"
#include "ll_path.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !LL_BUILD_ELF || !LL_TARGET_X86_64
#error "elf_x86_64_sites requires the real ELF x86_64 backend"
#endif

static unsigned failures;
static unsigned log_calls;
static char last_log[2048];
#define CHECK(condition, message) do { if (!(condition)) { \
    (void)fprintf(stderr, "elf_x86_64_sites:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

static void capture_log(void *ctx, int kind, const char *format, va_list args)
{
    (void)ctx;
    CHECK(kind == LL_LOG_ERROR, "unexpected log kind");
    int count = vsnprintf(last_log, sizeof(last_log), format, args);
    CHECK(count >= 0 && (size_t)count < sizeof(last_log), "truncated test log");
    ++log_calls;
}

static void reset(ll_linker *owner)
{
    ll_error_clear(owner);
    log_calls = 0;
    last_log[0] = '\0';
}

static int apply(ObjectCode *object, uint32_t type, unsigned char *work,
                 uintptr_t exec, uintptr_t symbol, int64_t addend,
                 ll_sym_type normalized, const ll_reloc_context *context)
{
    const ll_site site = {.work = work, .exec = exec};
    return ll_elf_x86_64_reloc_ops.apply(object, NULL, type, site, symbol,
                                       addend, normalized, context);
}

static void check_simple(ObjectCode *object)
{
    static const struct {
        uint32_t type;
        uintptr_t symbol;
        int64_t addend;
        uintptr_t exec;
        uint64_t expected;
        size_t bytes;
    } cases[] = {
        {COMPAT_R_X86_64_NONE, 0, 0, 0, 0, 0},
        {COMPAT_R_X86_64_64, UINT64_C(0x123456789abcdef0), -16, 1,
                            UINT64_C(0x123456789abcdee0), 8},
        {COMPAT_R_X86_64_64, UINT64_MAX, 2, 1, 1, 8},
        {COMPAT_R_X86_64_PC64, 0x1000, -8, 0x1020, UINT64_MAX - 39, 8},
        {COMPAT_R_X86_64_PC32, 0x1000, -8, 0x1020, UINT32_C(0xffffffd8), 4},
        {COMPAT_R_X86_64_PC32, 0, 0, UINT64_C(0x80000000),
                              UINT32_C(0x80000000), 4},
        {COMPAT_R_X86_64_PC32, UINT64_C(0x17fffffff), 0,
                              UINT64_C(0x100000000), INT32_MAX, 4},
        {COMPAT_R_X86_64_32, UINT32_MAX, 0, 1, UINT32_MAX, 4},
        {COMPAT_R_X86_64_32S, INT32_MAX, 0, 1, INT32_MAX, 4},
        {COMPAT_R_X86_64_32S, 0, INT32_MIN, 1, UINT32_C(0x80000000), 4},
        {COMPAT_R_X86_64_TLSGD, 0x1100, -4, 0x1000, 0xfc, 4},
        {COMPAT_R_X86_64_PLT32, 0x1100, -4, 0x1000, 0xfc, 4}
    };
    const ll_reloc_context named = {31, "site_symbol", LL_ELF_STT_OBJECT};
    const ll_reloc_context unnamed = {0, NULL, 0};
    object->L->services.cfg.always_pic = true;
    for (size_t n = 0; n < sizeof(cases) / sizeof(cases[0]); ++n) {
        unsigned char bytes[24];
        memset(bytes, 0xa5, sizeof(bytes));
        reset(object->L);
        CHECK(apply(object, cases[n].type, bytes + 3, cases[n].exec,
                    cases[n].symbol, cases[n].addend, LL_SYM_DATA,
                    cases[n].type == COMPAT_R_X86_64_NONE ? &unnamed : &named),
              "ordinary relocation failed");
        uint64_t actual = 0;
        if (cases[n].bytes == 8) memcpy(&actual, bytes + 3, 8);
        if (cases[n].bytes == 4) {
            uint32_t word;
            memcpy(&word, bytes + 3, 4);
            actual = word;
        }
        CHECK(actual == cases[n].expected, "wrong work/exec relocation arithmetic");
        for (size_t i = 0; i < sizeof(bytes); ++i)
            if (i < 3 || i >= 3 + cases[n].bytes)
                CHECK(bytes[i] == 0xa5, "store changed adjacent bytes");
        CHECK(object->L->error_status == LL_OK && log_calls == 0,
              "successful relocation changed diagnostics");
    }
}

static void check_rejections(ObjectCode *object)
{
    static const struct { uint32_t type; const char *name; } forms[] = {
        {COMPAT_R_X86_64_PC32, "R_X86_64_PC32"},
        {COMPAT_R_X86_64_32, "R_X86_64_32"},
        {COMPAT_R_X86_64_32S, "R_X86_64_32S"}
    };
    const uint32_t raw_types[] = {LL_ELF_STT_NOTYPE, LL_ELF_STT_OBJECT, 10};
    object->L->services.cfg.always_pic = false;
    for (size_t f = 0; f < sizeof(forms) / sizeof(forms[0]); ++f) {
        for (size_t t = 0; t < sizeof(raw_types) / sizeof(raw_types[0]); ++t) {
            unsigned char bytes[8];
            memset(bytes, 0xa5, sizeof(bytes));
            const ll_reloc_context context = {31, "site_symbol", raw_types[t]};
            reset(object->L);
            CHECK(!apply(object, forms[f].type, bytes, 0,
                         UINT64_C(0x100000000), 0, LL_SYM_CODE, &context),
                  "raw non-function type acquired a jump island");
            char expected[1024];
            int count = snprintf(expected, sizeof(expected),
                "%s overflow for non-function symbol `site_symbol'", forms[f].name);
            CHECK(count >= 0 && (size_t)count < sizeof(expected), "expected error overflow");
            CHECK(object->L->error_status == LL_E_RELOC_RANGE &&
                  strcmp(object->L->error_message, expected) == 0,
                  "non-function status or diagnostic changed");
            count = snprintf(expected, sizeof(expected),
                "%s overflow for non-function symbol `site_symbol' "
                "(type %u, %s 0x100000000).\n"
                "    Jump islands only work for code, not data references.\n"
                "    Recompile site-object.o with -fPIC -fexternal-dynamic-refs.",
                forms[f].name, raw_types[t], f == 0 ? "distance" : "value");
            CHECK(count >= 0 && (size_t)count < sizeof(expected), "expected log overflow");
            CHECK(log_calls == 1 && strcmp(last_log, expected) == 0,
                  "raw numeric type or full log changed");
            for (size_t i = 0; i < sizeof(bytes); ++i)
                CHECK(bytes[i] == 0xa5, "rejected relocation wrote the site");
        }
    }
    object->L->services.cfg.always_pic = true;
    unsigned char bytes[8];
    const ll_reloc_context context = {31, "site_symbol", LL_ELF_STT_FUNC};
    for (size_t f = 0; f < sizeof(forms) / sizeof(forms[0]); ++f) {
        memset(bytes, 0xa5, sizeof(bytes));
        reset(object->L);
        CHECK(!apply(object, forms[f].type, bytes, 0, UINT64_C(0x100000000),
                     0, LL_SYM_CODE, &context), "PIC overflow unexpectedly succeeded");
        char expected[160];
        int count = snprintf(expected, sizeof(expected),
            "%s relocation out of range: site_symbol", forms[f].name);
        CHECK(count >= 0 && (size_t)count < sizeof(expected), "expected range error overflow");
        CHECK(object->L->error_status == LL_E_RELOC_RANGE &&
              strcmp(object->L->error_message, expected) == 0,
              "PIC range diagnostic changed");
        for (size_t i = 0; i < sizeof(bytes); ++i)
            CHECK(bytes[i] == 0xa5, "PIC rejection wrote the site");
    }
    reset(object->L);
    CHECK(!apply(object, 999, bytes, 0, 0, 0, (ll_sym_type)0, &context),
          "unsupported relocation succeeded");
    CHECK(object->L->error_status == LL_E_UNSUPPORTED &&
          strcmp(object->L->error_message,
                 "site-object.o: unhandled ELF relocation(RelA) type 999\n") == 0 &&
          log_calls == 1 && strcmp(last_log, object->L->error_message) == 0,
          "unsupported relocation diagnostics changed");
}

static void check_extras(ObjectCode *object)
{
    enum { STRIDE = 32, COUNT = 2 };
    unsigned char *slots = malloc(STRIDE * COUNT);
    CHECK(slots != NULL, "cannot allocate extras fixture");
    if (slots == NULL) return;
    memset(slots, 0xa5, STRIDE * COUNT);
    object->symbol_extras = (SymbolExtra *)(void *)slots;
    object->symbol_extra_stride = STRIDE;
    object->first_symbol_extra = 31;
    object->n_symbol_extras = COUNT;
    const uintptr_t symbol = UINT64_C(0xfedcba9876543210);
    const uint32_t forms[] = {COMPAT_R_X86_64_GOTPCREL,
        COMPAT_R_X86_64_GOTPCRELX, COMPAT_R_X86_64_REX_GOTPCRELX};
    object->L->services.cfg.always_pic = true;
    for (size_t f = 0; f < sizeof(forms) / sizeof(forms[0]); ++f) {
        for (size_t i = 0; i < 3; ++i) {
            /* A repeated index reuses its slot; equal S must not merge indices. */
            const size_t slot = i % COUNT;
            const ll_reloc_context context = {31 + slot, "site_symbol", LL_ELF_STT_OBJECT};
            unsigned char bytes[4];
            reset(object->L);
            CHECK(apply(object, forms[f], bytes, (uintptr_t)slots - 256,
                        symbol, -4, LL_SYM_DATA, &context), "GOT relocation failed");
            int32_t value;
            memcpy(&value, bytes, sizeof(value));
            CHECK(value == (int32_t)(252 + slot * STRIDE),
                  "GOT relocation lost original symbol index");
            uint64_t address;
            memcpy(&address, slots + slot * STRIDE, sizeof(address));
            CHECK(address == symbol, "GOT extra truncated or changed S");
            CHECK(object->L->error_status == LL_OK && log_calls == 0,
                  "successful GOT relocation changed diagnostics");
        }
    }
    const unsigned char jump[] = {0xff, 0x25, 0xf2, 0xff, 0xff, 0xff, 0, 0};
    for (size_t i = 0; i < COUNT; ++i) {
        CHECK(memcmp(slots + i * STRIDE + 8, jump, sizeof(jump)) == 0,
              "native extra instructions changed");
        for (size_t b = sizeof(ll_symbol_extra_x86_64); b < STRIDE; ++b)
            CHECK(slots[i * STRIDE + b] == 0xa5, "extra wrote stride padding");
    }
    const uint32_t branches[] = {COMPAT_R_X86_64_PC32, COMPAT_R_X86_64_PLT32};
    object->L->services.cfg.always_pic = false;
    for (size_t i = 0; i < sizeof(branches) / sizeof(branches[0]); ++i) {
        unsigned char bytes[4];
        const ll_reloc_context context = {31, "site_symbol", LL_ELF_STT_FUNC};
        const uintptr_t exec = (uintptr_t)slots + 8 - 64;
        reset(object->L);
        CHECK(apply(object, branches[i], bytes, exec, exec + UINT64_C(0x100000000),
                    -4, LL_SYM_DATA, &context), "function island relocation failed");
        int32_t value;
        memcpy(&value, bytes, sizeof(value));
        CHECK(value == 60, "function island did not use native extra exec address");
    }
    free(slots);
    object->symbol_extras = NULL;
}

int main(void)
{
    ll_linker owner = {0};
    ll_compat_host_ops host = {.log = capture_log};
    ll_install_host_for(&owner, &host, NULL);
    ObjectCode object = {0};
    object.L = &owner;
    object.fileName = WSTR("site-object.o");
    CHECK(ll_elf_x86_64_reloc_ops.machine == LL_ELF_EM_X86_64 &&
          ll_elf_x86_64_reloc_ops.extra_bytes_per_symbol == sizeof(ll_symbol_extra_x86_64),
          "x64 relocation table identity or payload size changed");
    check_simple(&object);
    check_rejections(&object);
    check_extras(&object);
    if (failures != 0) return 1;
    (void)puts("elf_x86_64_sites: PASS (work/exec, diagnostics, raw types, extra indices)");
    return 0;
}
