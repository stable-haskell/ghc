/* Format selection and core lifecycle dispatch, independent of file parsing. */
#define _DARWIN_C_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "ll_format.h"
static const struct ll_format_ops *fixture_select(ObjectCode *oc);
#define ll_format_select fixture_select
#include "Linker.c"
#undef ll_format_select

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(mingw32_HOST_OS)
#include <windows.h>
#else
#include <unistd.h>
#endif

static unsigned failures;
static size_t live;
static char events[64];
static size_t event_count;
static bool fail_init;
static bool release_image;
static const struct ll_format_ops *selected;

#define CHECK(c, message) do { if (!(c)) { \
    (void)fprintf(stderr, "format_dispatch:%d: %s\n", __LINE__, message); \
    ++failures; } } while (0)

static void event(char c)
{
    CHECK(event_count + 1 < sizeof(events), "event buffer exhausted");
    if (event_count + 1 < sizeof(events)) {
        events[event_count++] = c;
        events[event_count] = '\0';
    }
}
static void *allocate(void *ctx, size_t n, const char *what)
{
    (void)ctx; (void)what;
    void *p = malloc(n == 0 ? 1 : n);
    if (p == NULL) _Exit(99);
    ++live;
    return p;
}
static void *allocate_zero(void *ctx, size_t n, size_t size, const char *what)
{
    (void)ctx; (void)what;
    void *p = calloc(n == 0 ? 1 : n, size == 0 ? 1 : size);
    if (p == NULL) _Exit(99);
    ++live;
    return p;
}
static void release(void *ctx, void *p)
{
    (void)ctx;
    if (p != NULL) { CHECK(live != 0, "allocation count underflow"); --live; free(p); }
}
static size_t page_size(void *ctx)
{
    (void)ctx;
#if defined(mingw32_HOST_OS)
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return info.dwPageSize;
#else
    long size = sysconf(_SC_PAGESIZE);
    return size > 0 ? (size_t)size : 4096;
#endif
}
static void fatal(void *ctx, const char *fmt, va_list ap)
{
    (void)ctx;
    (void)vfprintf(stderr, fmt, ap);
    _Exit(98);
}
static void before(void *ctx, ObjectCode *oc) { (void)ctx; (void)oc; event('B'); }
static void after(void *ctx, ObjectCode *oc, bool ok)
{
    (void)ctx; (void)oc; CHECK(ok, "successful initializer hook reported failure"); event('A');
}
static const struct ll_format_ops *fixture_select(ObjectCode *oc)
{
    CHECK(oc->type == STATIC_OBJECT, "dynamic object was sniffed");
    event('S');
    return selected;
}
static int prepare(ObjectCode *oc)
{
    event('P');
    if (fail_init) { ll_error_set(oc->L, LL_E_NOMEM, "fixture init failure"); return 0; }
    return 1;
}
static int verify(ObjectCode *oc) { (void)oc; event('V'); return 1; }
static int extras(ObjectCode *oc)
{
#if defined(NEED_SYMBOL_EXTRAS)
    oc->n_symbol_extras = 0;
    oc->first_symbol_extra = 0;
#else
    (void)oc;
#endif
    event('X');
    return 1;
}
static int names(ObjectCode *oc)
{
    event('N');
    if (release_image) {
        LL_FREE_FOR(oc->L, oc->image);
        oc->image = NULL;
    }
    return 1;
}
static int resolve_first(ObjectCode *oc) { (void)oc; event('R'); return 1; }
static int resolve_second(ObjectCode *oc) { (void)oc; event('r'); return 1; }
static int initialize(ObjectCode *oc) { (void)oc; event('I'); return 1; }
static int finalize(ObjectCode *oc) { (void)oc; event('F'); return 1; }
static void cxa_finalize(void *image) { (void)image; event('C'); }
static void deinitialize(ObjectCode *oc)
{
    CHECK(oc->format_ops == selected, "object lost its selected operations");
    CHECK(oc->sections != NULL && oc->sections[0].start != NULL,
          "format teardown ran after section storage was released");
    CHECK((oc->image == NULL) == release_image,
          "format teardown saw unexpected image lifetime");
    event('D');
}
static const struct ll_format_ops first = {
    .name = "fixture-first", .init = prepare, .verify = verify,
    .alloc_extras = extras, .get_names = names, .resolve = resolve_first,
    .run_init = initialize, .run_fini = finalize, .deinit = deinitialize
};
static const struct ll_format_ops second = {
    .name = "fixture-second", .init = prepare, .verify = verify,
    .alloc_extras = extras, .get_names = names, .resolve = resolve_second,
    .run_init = initialize, .run_fini = finalize, .deinit = deinitialize
};

static ObjectCode *object(ll_linker *owner, bool member)
{
    char *image = LL_CALLOC_FOR(owner, 1, 8, "fixture image");
    ObjectCode *oc = mkOc(owner, STATIC_OBJECT, WSTR("fixture.o"), image, 8,
                         false, member ? WSTR("archive.a(member.o)") : NULL, 0);
    oc->n_sections = 1;
    oc->sections = LL_CALLOC_FOR(owner, 1, sizeof(Section), "fixture section");
    oc->sections[0].start = LL_MALLOC_FOR(owner, 1, "fixture section data");
    oc->sections[0].alloc = SECTION_MALLOC;
    oc->sections[0].size = 1;
    return oc;
}

static void check_lifecycle(ll_linker *owner, bool member)
{
    event_count = 0; events[0] = '\0';
    release_image = member;
    selected = member ? &second : &first;
    const size_t initial_live = live;
    ObjectCode *oc = object(owner, member);
    CHECK(ll_init_object(oc) && oc->format_ops == selected,
          "initialization did not retain selected operations");
    CHECK(loadOc(oc) && strcmp(events, "SPVXN") == 0,
          "verify/extras/names dispatch order differs");
    if (member) {
        CHECK(oc->status == OBJECT_LOADED && ocTryLoad(oc), "lazy member load failed");
        CHECK(strcmp(events, "SPVXN") == 0, "unused archive member resolved eagerly");
        oc->status = OBJECT_NEEDED;
    }
    CHECK(ocTryLoad(oc) && oc->status == OBJECT_RESOLVED,
          "selected resolver did not complete");
    CHECK(ocRunInit(oc) && oc->status == OBJECT_READY && oc->initializersRan,
          "selected initializer did not complete");
    CHECK(ocRunInit(oc), "already-ready initializer failed");
    oc->cxa_finalize = cxa_finalize;
    freeObjectCode(oc);
    CHECK(strcmp(events, member ? "SPVXNrBIAFCD" : "SPVXNRBIAFCD") == 0,
          "per-object callbacks, hooks, finalizers, or teardown order differs");
    CHECK(live == initial_live, "lifecycle leaked allocations");
}

static void check_interleaved_objects(ll_linker *owner)
{
    event_count = 0; events[0] = '\0';
    release_image = false;
    const size_t initial_live = live;
    ObjectCode *a = object(owner, false);
    ObjectCode *b = object(owner, false);
    selected = &first;
    CHECK(ll_init_object(a) && loadOc(a), "first interleaved load failed");
    selected = &second;
    CHECK(ll_init_object(b) && loadOc(b), "second interleaved load failed");
    CHECK(ocTryLoad(a) && ocTryLoad(b), "interleaved resolution failed");
    CHECK(strcmp(events, "SPVXNSPVXNRr") == 0,
          "later selection replaced an earlier object's operations");
    selected = &first;
    freeObjectCode(a);
    selected = &second;
    freeObjectCode(b);
    CHECK(strcmp(events, "SPVXNSPVXNRrDD") == 0 && live == initial_live,
          "interleaved disposal ran finalizers or leaked allocations");
}

static void check_detection(ll_linker *owner)
{
    const uint8_t signatures[][20] = {
        {0x7f, 'E', 'L', 'F'},
        {0xcf, 0xfa, 0xed, 0xfe}, {0xfe, 0xed, 0xfa, 0xcf},
        {0xce, 0xfa, 0xed, 0xfe}, {0xfe, 0xed, 0xfa, 0xce},
        {0x64, 0x86}, {0x4c, 0x01}, {0x64, 0xaa},
        {0, 0, 0xff, 0xff}, {0, 'a', 's', 'm'}
    };
    const char *formats[] = {"ELF", "Mach-O", "Mach-O", "Mach-O", "Mach-O",
                             "PEi386", "PEi386", "PEi386", "PEi386", "Wasm"};
    ObjectCode oc = {.L = owner, .fileName = WSTR("signature-only.o")};
    const struct ll_format_ops *native = ll_format_native();
    CHECK(native != NULL, "native test host has no backend");
    for (size_t i = 0; i < sizeof(signatures) / sizeof(signatures[0]); ++i) {
        /* Include an unaligned start; no typed header reads are permitted. */
        uint8_t storage[21] = {0};
        memcpy(storage + 1, signatures[i], sizeof(signatures[i]));
        oc.image = (char *)storage + 1;
        oc.fileSize = 20;
        ll_error_clear(owner);
        const struct ll_format_ops *ops = ll_format_select(&oc);
        const bool available = native != NULL && strcmp(native->name, formats[i]) == 0;
        CHECK(available ? ops == native : ops == NULL && owner->error_status == LL_E_UNSUPPORTED,
              "recognized format selected wrong backend or status");
        for (int n = 0; n < 4; ++n) {
            uint8_t *prefix = malloc(n == 0 ? 1 : (size_t)n);
            if (prefix == NULL) _Exit(99);
            memcpy(prefix, signatures[i], (size_t)n);
            oc.image = (char *)prefix;
            oc.fileSize = n;
            ll_error_clear(owner);
            CHECK(ll_format_select(&oc) == NULL && owner->error_status == LL_E_FORMAT,
                  "short prefix unexpectedly selected a backend");
            free(prefix);
        }
    }
    oc.image = NULL; oc.fileSize = 20;
    CHECK(ll_format_select(&oc) == NULL && owner->error_status == LL_E_FORMAT,
          "NULL image was not rejected");
    char unknown[20] = {0};
    oc.image = unknown; oc.fileSize = 20;
    CHECK(ll_format_select(&oc) == NULL && owner->error_status == LL_E_FORMAT,
          "unrecognized format did not report FORMAT");
}

int main(void)
{
    ll_linker owner = {0};
    ll_compat_host_ops host = {.malloc_ = allocate, .calloc_ = allocate_zero,
        .free_ = release, .page_size = page_size, .fatal = fatal,
        .before_init = before, .after_init = after};
    ll_install_host_for(&owner, &host, NULL);
    check_detection(&owner);
    ll_error_clear(&owner);
    check_lifecycle(&owner, false);
    check_lifecycle(&owner, true);
    check_interleaved_objects(&owner);

    event_count = 0; events[0] = '\0';
    release_image = false; fail_init = true; selected = &first;
    ObjectCode *partial = object(&owner, false);
    CHECK(!ll_init_object(partial) && owner.error_status == LL_E_NOMEM,
          "partial initialization lost its original error");
    ll_discard_object(partial);
    CHECK(strcmp(events, "SPD") == 0 && owner.error_status == LL_E_NOMEM,
          "partial initialization ran finalizers, duplicated teardown, or lost its error");

    event_count = 0; events[0] = '\0';
    ObjectCode *dynamic = mkOc(&owner, DYNAMIC_OBJECT, WSTR("native-library"),
                               NULL, 0, false, NULL, 0);
    CHECK(ll_init_object(dynamic) && dynamic->format_ops == NULL,
          "native shared library was treated as a relocatable image");
    freeObjectCode(dynamic);
    CHECK(event_count == 0, "native shared library used static format callbacks");
    ObjectCode unselected = {.L = &owner, .type = STATIC_OBJECT,
        .fileName = WSTR("unselected.o"), .status = OBJECT_NEEDED};
    ll_error_clear(&owner);
    CHECK(!loadOc(&unselected) && owner.error_status == LL_E_STATE,
          "load without selected operations did not report STATE");
    ll_error_clear(&owner);
    CHECK(!ocTryLoad(&unselected) && owner.error_status == LL_E_STATE,
          "resolve without selected operations did not report STATE");
    unselected.status = OBJECT_RESOLVED;
    ll_error_clear(&owner);
    CHECK(!ocRunInit(&unselected) && owner.error_status == LL_E_STATE,
          "init without selected operations did not report STATE");
    CHECK(!unselected.resolution_failed && event_count == 0,
          "missing operations dispatched hooks or quarantined before dispatch");
    CHECK(live == 0, "format dispatch controls leaked allocations");
    if (failures != 0) return 1;
    (void)puts("format_dispatch: PASS (selection, per-object dispatch, lazy members, lifecycle)");
    return 0;
}
