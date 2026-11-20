#include "Rts.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef int (*read_fn)(void);
typedef void (*write_fn)(int);
typedef int *(*address_fn)(void);
typedef void *(*thunk_fn)(void *);
struct descriptor { thunk_fn thunk; unsigned long key; unsigned long offset; };
static read_fn read_value;
static write_fn write_value;
static address_fn address_value;
static struct descriptor *descriptor;
static int initial;
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
static int arrived;
static int failed;
static int *worker_addresses[2];
static int *main_address;
static thunk_fn descriptor_thunk;
static size_t required_alignment;
struct worker_arg { int value; int index; };

static void *worker(void *arg)
{
    struct worker_arg *a = arg;
    int value = a->value;
    int bad = read_value() != initial || descriptor_thunk(descriptor) != address_value() ||
              (uintptr_t)address_value() % required_alignment != 0;
    write_value(value);
    pthread_mutex_lock(&mutex);
    worker_addresses[a->index] = address_value();
    arrived++;
    pthread_cond_broadcast(&cond);
    while (arrived < 2) pthread_cond_wait(&cond, &mutex);
    if (bad || read_value() != value || worker_addresses[0] == worker_addresses[1] ||
        worker_addresses[a->index] == main_address ||
        (void *)worker_addresses[a->index] == (void *)descriptor) failed = 1;
    pthread_mutex_unlock(&mutex);
    return NULL;
}

/* Clang -O0 emits movq disp32(%rip), %rdi for this address helper. */
static int slot_matches(void *code, void *expected)
{
    unsigned char *p = code;
    for (size_t i = 0; i < 16; i++) {
        if (p[i] == 0x48 && p[i + 1] == 0x8b && p[i + 2] == 0x3d) {
            int32_t displacement;
            void *slot_value;
            memcpy(&displacement, p + i + 3, sizeof(displacement));
            uintptr_t slot = (uintptr_t)(p + i + 7) + (intptr_t)displacement;
            memcpy(&slot_value, (void *)slot, sizeof(slot_value));
            return slot_value == expected;
        }
    }
    return 0;
}

static int check(const char *read_name, const char *write_name,
                 const char *address_name, const char *tls_name, int value, size_t alignment)
{
    pthread_t threads[2];
    struct worker_arg values[2] = {{31, 0}, {47, 1}};
    void *symbol = lookupSymbol((char *)read_name);
    if (!symbol || sizeof(symbol) != sizeof(read_value)) return 0;
    memcpy(&read_value, &symbol, sizeof(read_value));
    symbol = lookupSymbol((char *)write_name);
    if (!symbol || sizeof(symbol) != sizeof(write_value)) return 0;
    memcpy(&write_value, &symbol, sizeof(write_value));
    symbol = lookupSymbol((char *)address_name);
    if (!symbol || sizeof(symbol) != sizeof(address_value)) return 0;
    memcpy(&address_value, &symbol, sizeof(address_value));
    descriptor = lookupSymbol((char *)tls_name);
    if (!descriptor) return 0;
    struct descriptor copy;
    memcpy(&copy, descriptor, sizeof(copy));
    descriptor_thunk = copy.thunk;
    main_address = address_value();
    if (!slot_matches(symbol, descriptor) || !descriptor_thunk ||
        descriptor_thunk(descriptor) != main_address ||
        (void *)descriptor == (void *)address_value() || read_value() != value) return 0;
    if ((uintptr_t)main_address % alignment != 0) return 0;
    required_alignment = alignment;
    initial = value;
    arrived = 0;
    failed = 0;
    for (int i = 0; i < 2; i++) {
        if (pthread_create(&threads[i], NULL, worker, &values[i])) return 0;
    }
    for (int i = 0; i < 2; i++) {
        if (pthread_join(threads[i], NULL)) return 0;
    }
    if (getenv("MACHO_TLV_TRACE"))
        fprintf(stderr, "%s: lookupDependentSymbol via lookupSymbol descriptor=%p slot=descriptor main=%p workers=%p,%p isolated=%s\n",
                tls_name, (void *)descriptor, (void *)main_address,
                (void *)worker_addresses[0], (void *)worker_addresses[1],
                failed ? "no" : "yes");
    return !failed && read_value() == value;
}

/* Keep a thread's storage live while the object is unloaded.  After the
 * barrier the worker uses only its storage, never the unloaded code. */
static int unload_ready, unload_done, unload_failed;
static void *unload_worker(void *unused)
{
    (void)unused;
    int *p = address_value();
    *p = 73;
    pthread_mutex_lock(&mutex);
    unload_ready = 1;
    pthread_cond_broadcast(&cond);
    while (!unload_done) pthread_cond_wait(&cond, &mutex);
    if (*p != 73) unload_failed = 1;
    pthread_mutex_unlock(&mutex);
    return NULL;
}

static int check_unload(void)
{
    pthread_t thread;
    unload_ready = unload_done = unload_failed = 0;
    if (pthread_create(&thread, NULL, unload_worker, NULL)) return 0;
    pthread_mutex_lock(&mutex);
    while (!unload_ready) pthread_cond_wait(&cond, &mutex);
    pthread_mutex_unlock(&mutex);
    int ok = unloadObj("macho_tlv_defined.o");
    if (ok) { performMajorGC(); performMajorGC(); }
    pthread_mutex_lock(&mutex);
    unload_done = 1;
    pthread_cond_broadcast(&cond);
    pthread_mutex_unlock(&mutex);
    if (pthread_join(thread, NULL)) return 0;
    return ok && !unload_failed;
}

int main(int argc, char **argv)
{
    hs_init(&argc, &argv);
    initLinker_(0);
    if (addDLL("./macho_tlv_provider.dylib") != NULL) return 1;
    if (!loadObj("macho_tlv_external.o") || !resolveObjs()) return 1;
    if (!check("_macho_tlv_read", "_macho_tlv_write", "_macho_tlv_address",
               "_macho_tlv_value", 17, sizeof(int))) return 1;
    if (!loadObj("macho_tlv_defined.o") || !resolveObjs()) return 1;
    if (!check("_macho_tlv_read_defined", "_macho_tlv_write_defined",
               "_macho_tlv_address_defined", "_macho_tlv_defined", 42, sizeof(int))) return 1;
    if (!check("_macho_tlv_read_zero", "_macho_tlv_write_zero",
               "_macho_tlv_address_zero", "_macho_tlv_zero", 0, 64)) return 1;
    if (!check_unload()) return 1;
    puts("external and object-defined TLS isolated");
    hs_exit();
    return 0;
}
