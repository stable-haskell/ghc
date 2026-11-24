/* Execute one linked function through the stage-2 compatibility interface. */

#include <liblink/compat.h>
#include <liblink/rts_friend.h>
#include <ll_host.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if defined(darwin_HOST_OS)
#define EXTERNAL_CALLBACK_NAME "_external_callback"
#define FIXTURE_ENTRY_NAME "_fixture_entry"
#define MISSING_ENTRY_NAME "_compat_exec_smoke_missing"
#else
#define EXTERNAL_CALLBACK_NAME "external_callback"
#define FIXTURE_ENTRY_NAME "fixture_entry"
#define MISSING_ENTRY_NAME "compat_exec_smoke_missing"
#endif

_Static_assert(sizeof(int (*)(int)) == sizeof(void *),
               "the linker ABI requires equal function/data pointer sizes");

static int external_callback(int value)
{
    return value + 1;
}

static void *callback_address(void)
{
    int (*function)(int) = external_callback;
    void *address = NULL;
    memcpy(&address, &function, sizeof(address));
    return address;
}

static size_t host_page_size(void *ctx)
{
    long size;
    (void)ctx;
    size = sysconf(_SC_PAGESIZE);
    if (size <= 0) {
        fputs("sysconf(_SC_PAGESIZE) failed\n", stderr);
        abort();
    }
    return (size_t)size;
}

static void host_flush_icache(void *ctx, void *start, size_t size)
{
    (void)ctx;
    __builtin___clear_cache((char *)start, (char *)start + size);
}

static void register_object(void *ctx, ObjectCode *object)
{
    ll_gc_state *gc = ll_compat_default_gc_state();
    (void)ctx;
    if (gc->objects != NULL) gc->objects->prev = object;
    object->next = gc->objects;
    gc->objects = object;
}

static int lookup_and_call(void)
{
    void *address = ll_compat_lookupSymbol((char *)FIXTURE_ENTRY_NAME);
    int (*function)(int) = NULL;
    int result;

    if (address == NULL) {
        fprintf(stderr, "could not look up %s\n", FIXTURE_ENTRY_NAME);
        return 0;
    }
    memcpy(&function, &address, sizeof(function));
    result = function(41);
    if (result != 42) {
        fprintf(stderr, "%s(41) returned %d, expected 42\n",
                FIXTURE_ENTRY_NAME, result);
        return 0;
    }
    return 1;
}

int main(int argc, char **argv)
{
    ll_compat_host_ops host = {0};
    ll_compat_config config = {0};
    const char *object_path;
    bool missing_entry = false;
    int result = EXIT_FAILURE;

    if (argc == 3 && strcmp(argv[1], "--missing-entry") == 0) {
        missing_entry = true;
        object_path = argv[2];
    } else if (argc == 2) {
        object_path = argv[1];
    } else {
        fprintf(stderr, "usage: %s [--missing-entry] OBJECT\n", argv[0]);
        return EXIT_FAILURE;
    }

    host.page_size = host_page_size;
    host.flush_icache = host_flush_icache;
    host.object_registered = register_object;
    config.near_hint = callback_address();
    config.always_pic = true;
    ll_install_host(&host, &config);
    initLinkerMMap();
    ll_compat_initLinker();

    if (!ll_compat_insertSymbol((ll_compat_pathchar *)"compat_exec_smoke",
                                (char *)EXTERNAL_CALLBACK_NAME,
                                callback_address())) {
        fprintf(stderr, "could not register %s\n", EXTERNAL_CALLBACK_NAME);
        goto done;
    }
    if (!ll_compat_loadObj((ll_compat_pathchar *)object_path)) {
        fprintf(stderr, "could not load %s\n", object_path);
        goto done;
    }
    if (!ll_compat_resolveObjs()) {
        fputs("could not resolve loaded object\n", stderr);
        goto done;
    }
    if (missing_entry) {
        if (ll_compat_lookupSymbol((char *)MISSING_ENTRY_NAME) != NULL) {
            fputs("missing-entry lookup unexpectedly succeeded\n", stderr);
            goto done;
        }
        puts("compat linker missing-entry control passed");
        result = EXIT_SUCCESS;
        goto done;
    }
    if (!lookup_and_call()) goto done;

    puts("compat linker execution smoke passed");
    result = EXIT_SUCCESS;

done:
    exitLinker();
    return result;
}
