/* Verify hash table values and backing storage are released by the right owner. */

#include "ll_hash.h"
#include "ll_linker.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_TRACKED 128

typedef struct {
    const char *name;
    void *allocations[MAX_TRACKED];
    size_t allocation_count;
    unsigned malloc_calls;
    unsigned free_calls;
    unsigned wrong_owner_frees;
} TestHost;

static TestHost host_a = { .name = "A" };
static TestHost host_b = { .name = "B" };
static TestHost default_host = { .name = "default" };
static TestHost *all_hosts[] = { &host_a, &host_b, &default_host };
static void *legacy_expected[2];
static bool legacy_expected_live[2];
static int legacy_seen_ordinals[2];
static size_t legacy_seen_count;
static unsigned failures;

static void verify(int condition, const char *message)
{
    if (!condition) {
        (void)fprintf(stderr, "hash_destroy_owner: %s\n", message);
        ++failures;
    }
}

static void track_allocation(TestHost *host, void *pointer)
{
    if (pointer == NULL) {
        return;
    }
    if (host->allocation_count < MAX_TRACKED) {
        host->allocations[host->allocation_count++] = pointer;
    } else {
        verify(0, "test allocation ledger is full");
    }
}

static void *host_malloc(void *context, size_t size, const char *what LL_UNUSED)
{
    TestHost *host = context;
    void *pointer = malloc(size == 0 ? 1 : size);
    ++host->malloc_calls;
    track_allocation(host, pointer);
    return pointer;
}

static void host_free(void *context, void *pointer)
{
    TestHost *host = context;
    bool found = false;
    ++host->free_calls;
    for (size_t owner_index = 0;
         owner_index < sizeof(all_hosts) / sizeof(all_hosts[0]);
         ++owner_index) {
        TestHost *owner = all_hosts[owner_index];
        for (size_t i = 0; i < owner->allocation_count; ++i) {
            if (owner->allocations[i] == pointer) {
                if (owner != host) {
                    ++host->wrong_owner_frees;
                }
                owner->allocations[i] = owner->allocations[--owner->allocation_count];
                found = true;
                break;
            }
        }
        if (found) {
            break;
        }
    }
    if (!found) {
        ++host->wrong_owner_frees;
    }
    free(pointer);
}

static ll_compat_host_ops host_ops(TestHost *host)
{
    ll_compat_host_ops ops = {0};
    ops.ctx = host;
    ops.malloc_ = host_malloc;
    ops.free_ = host_free;
    return ops;
}

static void legacy_free(void *pointer)
{
    int ordinal = -1;
    for (size_t i = 0; i < sizeof(legacy_expected) / sizeof(legacy_expected[0]);
         ++i) {
        if (legacy_expected_live[i] && pointer == legacy_expected[i]) {
            ordinal = (int)i;
            legacy_expected_live[i] = false;
            break;
        }
    }
    if (legacy_seen_count
        < sizeof(legacy_seen_ordinals) / sizeof(legacy_seen_ordinals[0])) {
        legacy_seen_ordinals[legacy_seen_count] = ordinal;
    }
    ++legacy_seen_count;
    free(pointer);
}

int main(void)
{
    ll_linker linker_a = {0};
    ll_linker linker_b = {0};
    ll_compat_host_ops ops_a = host_ops(&host_a);
    ll_compat_host_ops ops_b = host_ops(&host_b);
    ll_compat_host_ops default_ops = host_ops(&default_host);
    unsigned default_malloc_before;
    unsigned default_free_before;
    HashTable *table;
    int *value_a;
    int *value_b;

    ll_install_host(&default_ops, NULL);
    ll_install_host_for(&linker_a, &ops_a, NULL);
    ll_install_host_for(&linker_b, &ops_b, NULL);
    default_malloc_before = default_host.malloc_calls;
    default_free_before = default_host.free_calls;

    table = allocHashTable(&linker_a);
    value_a = LL_MALLOC_FOR(&linker_a, sizeof(*value_a), "hash owned value A");
    value_b = LL_MALLOC_FOR(&linker_a, sizeof(*value_b), "hash owned value B");
    *value_a = 11;
    *value_b = 22;
    insertHashTable(table, 0, value_a);
    insertHashTable(table, sizeof(ll_word), value_b);
    ll_hash_free_owned_table(table);
    verify(host_a.allocation_count == 0 && host_a.wrong_owner_frees == 0,
           "owned values or table allocations did not return to owner A");
    verify(host_b.malloc_calls == 0 && host_b.free_calls == 0,
           "owner B was used while freeing owner A's table");

    table = allocHashTable(&linker_b);
    value_a = LL_MALLOC_FOR(&linker_b, sizeof(*value_a), "hash owned value B");
    *value_a = 33;
    insertHashTable(table, 0, value_a);
    ll_hash_free_owned_table(table);
    verify(host_b.allocation_count == 0 && host_b.wrong_owner_frees == 0,
           "owned value or table allocations did not return to owner B");

    table = allocHashTable(&linker_b);
    ll_hash_free_owned_table(table);
    verify(host_b.malloc_calls > 0 && host_b.free_calls == host_b.malloc_calls
           && host_b.allocation_count == 0 && host_b.wrong_owner_frees == 0,
           "empty table backing allocations did not return to owner B");

    table = allocHashTable(&linker_a);
    legacy_expected[0] = malloc(sizeof(int));
    legacy_expected[1] = malloc(sizeof(int));
    legacy_expected_live[0] = true;
    legacy_expected_live[1] = true;
    verify(legacy_expected[0] != NULL && legacy_expected[1] != NULL,
           "legacy callback test allocation failed");
    insertHashTable(table, 0, legacy_expected[0]);
    insertHashTable(table, sizeof(ll_word), legacy_expected[1]);
    legacy_seen_count = 0;
    ll_hash_free_table(table, legacy_free);
    verify(legacy_seen_count == 2 && !legacy_expected_live[0]
           && !legacy_expected_live[1] && legacy_seen_ordinals[0] == 1
           && legacy_seen_ordinals[1] == 0,
           "legacy callback values or callback order changed");
    verify(host_a.allocation_count == 0 && host_a.wrong_owner_frees == 0,
           "legacy table backing allocations did not return to owner A");
    verify(default_host.malloc_calls == default_malloc_before
           && default_host.free_calls == default_free_before,
           "hash table destruction used default host services");

    return failures == 0 ? 0 : 1;
}
