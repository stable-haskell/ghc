/* Check that pathdup returns storage allocated and freed by the linker owner. */

#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_linker.h"
#include "ll_path.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(mingw32_HOST_OS)
#define TEST_PATH(text) L##text
static const pathchar non_ascii_path[] = L"directory/\u03bb.dll";
#else
#define TEST_PATH(text) text
static const pathchar non_ascii_path[] = "directory/\xCE\xBB.so";
#endif

#define MAX_LIVE_ALLOCATIONS 16

typedef struct {
    void *pointer;
    size_t bytes;
    const char *what;
} TrackedAllocation;

typedef struct {
    const char *name;
    TrackedAllocation live[MAX_LIVE_ALLOCATIONS];
    size_t live_count;
    unsigned malloc_calls;
    unsigned free_calls;
    unsigned wrong_owner_frees;
    unsigned untracked_frees;
    size_t last_size;
    const char *last_what;
    bool last_free_was_tracked;
} TestHost;

static TestHost host_a = { .name = "A" };
static TestHost host_b = { .name = "B" };
static TestHost default_host = { .name = "default" };
static TestHost *all_hosts[] = { &host_a, &host_b, &default_host };
static unsigned failures;

static void verify(int condition, const char *message)
{
    if (!condition) {
        (void)fprintf(stderr, "pathdup_ownership: %s\n", message);
        ++failures;
    }
}

static void *host_malloc(void *context, size_t bytes, const char *what)
{
    TestHost *host = context;
    void *pointer = malloc(bytes == 0 ? 1 : bytes);
    if (pointer == NULL) {
        (void)fprintf(stderr, "pathdup_ownership: host %s allocation failed\n",
                      host->name);
        _Exit(2);
    }
    if (host->live_count == MAX_LIVE_ALLOCATIONS) {
        (void)fprintf(stderr, "pathdup_ownership: host %s ledger is full\n",
                      host->name);
        free(pointer);
        _Exit(2);
    }
    host->live[host->live_count++] = (TrackedAllocation){ pointer, bytes, what };
    ++host->malloc_calls;
    host->last_size = bytes;
    host->last_what = what;
    return pointer;
}

static void host_free(void *context, void *pointer)
{
    TestHost *host = context;
    host->last_free_was_tracked = false;
    ++host->free_calls;
    for (size_t owner_index = 0;
         owner_index < sizeof(all_hosts) / sizeof(all_hosts[0]);
         ++owner_index) {
        TestHost *owner = all_hosts[owner_index];
        for (size_t index = 0; index < owner->live_count; ++index) {
            if (owner->live[index].pointer == pointer) {
                if (owner != host) {
                    ++host->wrong_owner_frees;
                    return;
                }
                owner->live[index] = owner->live[--owner->live_count];
                host->last_free_was_tracked = true;
                free(pointer);
                return;
            }
        }
    }
    /* Do not pass an untracked pointer to free: the test owns fallback cleanup. */
    ++host->untracked_frees;
}

static TestHost *find_live_owner(void *pointer)
{
    for (size_t owner_index = 0;
         owner_index < sizeof(all_hosts) / sizeof(all_hosts[0]);
         ++owner_index) {
        TestHost *owner = all_hosts[owner_index];
        for (size_t index = 0; index < owner->live_count; ++index) {
            if (owner->live[index].pointer == pointer) return owner;
        }
    }
    return NULL;
}

static ll_compat_host_ops host_ops(TestHost *host)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = host;
    ops.malloc_ = host_malloc;
    ops.free_ = host_free;
    return ops;
}

static void check_path_case(ll_linker *owner, TestHost *host,
                            const pathchar *path, const char *description)
{
    unsigned malloc_before = host->malloc_calls;
    unsigned frees_before = host->free_calls;
    size_t live_before = host->live_count;
    size_t units = pathlen(path) + 1;
    size_t bytes = units * sizeof(*path);
    pathchar *copy = pathdup(owner, path);

    verify(copy != NULL, "pathdup returned NULL");
    if (copy == NULL) return;
    verify(copy != path, "pathdup returned the original buffer");
    verify(memcmp(copy, path, bytes) == 0, description);
    verify(copy[units - 1] == 0, "copy is missing its exact terminator");
    verify(host->malloc_calls == malloc_before + 1,
           "pathdup did not call this owner's allocator exactly once");
    verify(host->last_size == bytes,
           "pathdup requested the wrong path byte count");
    verify(host->last_what != NULL && strcmp(host->last_what, "pathdup") == 0,
           "pathdup allocation label changed");

    LL_FREE_FOR(owner, copy);
    if (!host->last_free_was_tracked) {
        TestHost *allocation_owner = find_live_owner(copy);
        if (allocation_owner != NULL) {
            /* Clean up a wrong-owner allocation without hiding the failed check. */
            host_free(allocation_owner, copy);
        } else {
            /* The legacy Windows CRT result is outside this fixture's ledger. */
            free(copy);
        }
    }
    verify(host->free_calls == frees_before + 1,
           "pathdup result was not released through the owner callback");
    verify(host->last_free_was_tracked,
           "pathdup result was outside the freeing owner's allocation ledger");
    verify(host->live_count == live_before,
           "pathdup result remained live after owner release");
}

static void check_wrong_owner_detection(ll_linker *owner_a)
{
    unsigned wrong_before = host_b.wrong_owner_frees;
    int *pointer = LL_MALLOC_FOR(owner_a, sizeof(*pointer), "wrong-owner probe");
    size_t live_before = host_a.live_count;

    host_free(&host_b, pointer);
    verify(host_b.wrong_owner_frees == wrong_before + 1,
           "allocator ledger did not detect a wrong-owner free");
    verify(host_a.live_count == live_before,
           "wrong-owner detection removed the true owner's live allocation");
    LL_FREE_FOR(owner_a, pointer);
    verify(host_a.live_count == live_before - 1,
           "true owner could not release the detected allocation");
}

int main(void)
{
    ll_linker owner_a = {0};
    ll_linker owner_b = {0};
    ll_compat_host_ops ops_a = host_ops(&host_a);
    ll_compat_host_ops ops_b = host_ops(&host_b);
    ll_compat_host_ops default_ops = host_ops(&default_host);
    static const pathchar *const paths[] = {
        TEST_PATH(""), TEST_PATH("lib/sample.dll"), non_ascii_path
    };
    static const char *const descriptions[] = {
        "empty path copy differs", "ASCII path copy differs",
        "non-ASCII path copy differs"
    };

    ll_install_host(&default_ops, NULL);
    ll_install_host_for(&owner_a, &ops_a, NULL);
    ll_install_host_for(&owner_b, &ops_b, NULL);

    for (size_t index = 0; index < sizeof(paths) / sizeof(paths[0]); ++index) {
        check_path_case(&owner_a, &host_a, paths[index], descriptions[index]);
        check_path_case(&owner_b, &host_b, paths[index], descriptions[index]);
    }
    check_wrong_owner_detection(&owner_a);

    verify(host_a.live_count == 0 && host_a.wrong_owner_frees == 0
           && host_a.untracked_frees == 0,
           "owner A allocations were not fully released by owner A");
    verify(host_b.live_count == 0 && host_b.wrong_owner_frees == 1
           && host_b.untracked_frees == 0,
           "owner B allocations or wrong-owner detection were inconsistent");
    verify(default_host.malloc_calls == 0 && default_host.free_calls == 0,
           "default host allocator was used for a private owner path");
    if (failures != 0) return 1;
    puts("pathdup_ownership: PASS");
    return 0;
}
