#ifndef LIBLINK_TEST_RUNNER_INTERNAL_H
#define LIBLINK_TEST_RUNNER_INTERNAL_H

#include <liblink/compat.h>
#include <liblink/rts_friend.h>
#include <ll_host.h>

#include "../../src/linker/MachOTypes.h"

#include <mach-o/reloc.h>
#include <mach-o/arm64/reloc.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define RUNNER_MAX_INPUT_SIZE ((size_t)16 * 1024 * 1024)
#define RUNNER_MAX_SECTION_SIZE ((size_t)64 * 1024 * 1024)
#define RUNNER_MAX_EVENTS 64
#define RUNNER_MAX_FINALIZERS 32
#define RUNNER_MAX_HOST_SYMBOLS 8

typedef struct {
    void *address;
    void *target;
    uint8_t flags;
} RunnerStubCapture;

typedef struct {
    Section *section;
    MachOSection *macho;
    uint8_t *loaded;
    uint8_t *relocated;
    uint8_t *post_init;
    void *stub_start;
    size_t stub_size;
    uint8_t *stub_bytes;
    RunnerStubCapture *stubs;
    size_t stub_count;
} RunnerSectionCapture;

typedef struct {
    void (*function)(void *);
    void *argument;
    void *image;
    bool active;
} RunnerFinalizer;

typedef struct {
    const char *name;
    void *address;
} RunnerHostSymbol;

typedef struct {
    ObjectCode *object;
    RunnerSectionCapture *sections;
    size_t section_count;
    bool captured_before_init;
    void *image_base;
    void *got_start;
    size_t got_size;
    uint8_t *got_bytes;
    RunnerHostSymbol host_symbols[RUNNER_MAX_HOST_SYMBOLS];
    size_t host_symbol_count;
    unsigned events[RUNNER_MAX_EVENTS];
    size_t event_count;
    RunnerFinalizer finalizers[RUNNER_MAX_FINALIZERS];
    size_t finalizer_count;
    const char *input_path;
    uint8_t *input_bytes;
    size_t input_size;
    void *image_anchor;
} RunnerState;

void runner_host_configure(RunnerState *state, ll_host_ops *host,
                           ll_config *config, bool debug);
void runner_host_cleanup(RunnerState *state);
int runner_json_emit(RunnerState *state);

#endif
