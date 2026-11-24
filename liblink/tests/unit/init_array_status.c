#define LL_INTERNAL_SOURCE 1
#include "LinkerInternals.h"
#include "linker/InitFini.c"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Initializer callbacks have no context parameter. This serial fixture alone
 * uses active_host to inspect arguments and record their execution order. */
typedef struct {
    void *owned[16];
    size_t live;
    unsigned acquisitions;
    unsigned releases;
    unsigned fatals;
    bool with_args;
    char events[32];
    size_t event_count;
    char **environment;
} TestHost;

static TestHost *active_host;
static unsigned failures;
static char argument[] = "init-array-status";
static char variable_a[] = "INIT_A=one";
static char variable_b[] = "INIT_B=two";
static char *arguments[] = {argument, NULL};

#define CHECK(condition, message) do { \
    if (!(condition)) { \
        (void)fprintf(stderr, "init_array_status:%d: %s\n", __LINE__, message); \
        ++failures; \
    } \
} while (0)

static void event(char value)
{
    TestHost *host = active_host;
    if (host == NULL || host->event_count + 1 >= sizeof(host->events))
        _Exit(99);
    host->events[host->event_count++] = value;
    host->events[host->event_count] = '\0';
}

static void *host_malloc(void *ctx, size_t size, const char *what)
{
    TestHost *host = ctx;
    (void)what;
    void *pointer = malloc(size == 0 ? 1 : size);
    if (pointer == NULL || host->live == sizeof(host->owned) / sizeof(host->owned[0]))
        _Exit(99);
    host->owned[host->live++] = pointer;
    return pointer;
}

static void host_free(void *ctx, void *pointer)
{
    TestHost *host = ctx;
    if (pointer == NULL) return;
    for (size_t i = 0; i < host->live; ++i) {
        if (host->owned[i] == pointer) {
            host->owned[i] = host->owned[--host->live];
            free(pointer);
            return;
        }
    }
    CHECK(false, "free used a pointer outside the owner ledger");
}

static _Noreturn void host_fatal(void *ctx, const char *format, va_list args)
{
    ++((TestHost *)ctx)->fatals;
    (void)vfprintf(stderr, format, args);
    _Exit(98);
}

static void acquire_args(void *ctx, int *argc, char ***argv,
                         int *envc, char ***envp)
{
    TestHost *host = ctx;
    CHECK(host == active_host, "program arguments used another owner");
    ++host->acquisitions;
    host->environment = host_malloc(host, 3 * sizeof(char *), "test environment");
    host->environment[0] = variable_a;
    host->environment[1] = variable_b;
    host->environment[2] = NULL;
    *argc = 1;
    *argv = arguments;
    *envc = 2;
    *envp = host->environment;
    event('G');
}

static void release_args(void *ctx, int count, char **environment)
{
    TestHost *host = ctx;
    CHECK(host == active_host && count == 2, "environment owner/count changed");
    CHECK(environment == host->environment, "environment release pointer changed");
    ++host->releases;
    event('R');
    host_free(host, environment);
    host->environment = NULL;
}

static ll_compat_host_ops host_ops(TestHost *host, bool with_args)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = host;
    ops.malloc_ = host_malloc;
    ops.free_ = host_free;
    ops.fatal = host_fatal;
    if (with_args) {
        ops.prog_args = acquire_args;
        ops.free_prog_env = release_args;
    }
    host->with_args = with_args;
    return ops;
}

static void callback(char marker, int argc, char **argv, char **envp)
{
    if (active_host->with_args) {
        CHECK(argc == 1 && argv == arguments, "borrowed argv changed");
        CHECK(envp == active_host->environment && envp[0] == variable_a &&
                  envp[1] == variable_b && envp[2] == NULL,
              "initializer environment changed");
    } else {
        CHECK(argc == 0 && argv == NULL && envp == NULL,
              "absent argument hook did not supply empty arguments");
    }
    event(marker);
}

static void init_a(int n, char **a, char **e) { callback('a', n, a, e); }
static void init_b(int n, char **a, char **e) { callback('b', n, a, e); }
static void init_c(int n, char **a, char **e) { callback('c', n, a, e); }

static void array_case(unsigned null_index, bool with_args)
{
    TestHost host = {0};
    ll_linker owner = {0};
    ll_compat_host_ops ops = host_ops(&host, with_args);
    init_t entries[3] = {init_a, init_b, init_c};
    Section section = {0};
    struct InitFiniList *head = NULL;
    const bool valid = null_index == 3;
    static const char *const expected_owned[] = {"GR", "GaR", "GabR", "GabcR"};
    if (!valid) entries[null_index] = NULL;
    section.start = entries;
    section.size = sizeof(entries);
    ll_install_host_for(&owner, &ops, NULL);
    active_host = &host;
    addInitFini(&owner, &head, &section, INITFINI_INIT_ARRAY, 10);
    bool result = runInit(&owner, &head);
    CHECK(result == valid, "initializer result differs");
    CHECK(owner.error_status == (valid ? LL_OK : LL_E_INIT),
          "initializer returned wrong owner status");
    CHECK(strcmp(host.events, with_args ? expected_owned[null_index] : "abc") == 0,
          "constructor prefix or environment release order differs");
    CHECK(host.acquisitions == (with_args ? 1u : 0u) &&
              host.releases == host.acquisitions && host.environment == NULL,
          "program environment was not released exactly once");
    if (valid) {
        CHECK(head == NULL && host.live == 0, "successful list was not disposed");
    } else {
        CHECK(strcmp(owner.error_message, "null initializer in .init_array") == 0,
              "first initializer diagnostic changed");
        CHECK(head != NULL && host.live == 1,
              "failed list did not remain owned for normal disposal");
        freeInitFiniList(&owner, head);
        head = NULL;
    }
    CHECK(host.live == 0 && host.fatals == 0, "owner teardown leaked or used fatal");
    active_host = NULL;
}

static void priority_case(void)
{
    TestHost host = {0};
    ll_linker owner = {0};
    ll_compat_host_ops ops = host_ops(&host, true);
    init_t first[] = {init_a};
    init_t second[] = {init_b};
    Section sections[2] = {{0}, {0}};
    struct InitFiniList *head = NULL;
    sections[0].start = first;
    sections[0].size = sizeof(first);
    sections[1].start = second;
    sections[1].size = sizeof(second);
    ll_install_host_for(&owner, &ops, NULL);
    active_host = &host;
    addInitFini(&owner, &head, &sections[1], INITFINI_INIT_ARRAY, 10);
    addInitFini(&owner, &head, &sections[0], INITFINI_INIT_ARRAY, 20);
    CHECK(runInit(&owner, &head), "priority control failed");
    CHECK(strcmp(host.events, "GbaR") == 0 && head == NULL,
          "initializer priority order changed");
    CHECK(host.live == 0 && host.acquisitions == 1 && host.releases == 1 &&
              host.fatals == 0,
          "priority control did not release all owned storage");
    active_host = NULL;
}

int main(void)
{
    TestHost defaults = {0};
    ll_compat_host_ops ops = host_ops(&defaults, false);
    ll_install_host_for(&ll_default_linker, &ops, NULL);
    ll_error_set(&ll_default_linker, LL_E_NOT_FOUND, "default initializer sentinel");
    array_case(3, true);
    array_case(3, false);
    for (unsigned i = 0; i < 3; ++i) array_case(i, true);
    priority_case();
    CHECK(defaults.live == 0 && defaults.fatals == 0,
          "another owner used default callbacks");
    CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND &&
              strcmp(ll_default_linker.error_message,
                     "default initializer sentinel") == 0,
          "another owner changed default error");
    if (failures != 0) return 1;
    (void)puts("init_array_status: PASS (6 cases)");
    return 0;
}
