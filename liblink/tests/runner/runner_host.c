/* Host callbacks and phase snapshots for the native relocation runner. */

#include "runner_internal.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

_Static_assert(sizeof(void (*)(void)) == sizeof(void *),
               "the native linker ABI requires equal function/data pointer sizes");

/* Fixture callbacks lack context; this one-object process keeps state through exitLinker. */
static RunnerState *active_state;

int ll_fixture_target(void)
{
    return 42;
}

void fixture_record_event(unsigned event)
{
    if (active_state == NULL || active_state->event_count >= RUNNER_MAX_EVENTS) {
        fputs("fixture event buffer overflow\n", stderr);
        abort();
    }
    active_state->events[active_state->event_count++] = event;
}

static int fixture_cxa_atexit(void (*function)(void *), void *argument,
                              void *image)
{
    RunnerFinalizer *entry;
    RunnerState *state = active_state;
    if (state == NULL || state->finalizer_count >= RUNNER_MAX_FINALIZERS) {
        fputs("fixture finalizer buffer overflow\n", stderr);
        return -1;
    }
    entry = &state->finalizers[state->finalizer_count++];
    entry->function = function;
    entry->argument = argument;
    entry->image = image;
    entry->active = true;
    return 0;
}

static void fixture_cxa_finalize(void *image)
{
    RunnerState *state = active_state;
    size_t i;
    if (state == NULL) return;
    i = state->finalizer_count;
    while (i > 0) {
        RunnerFinalizer *entry = &state->finalizers[--i];
        if (entry->active && (image == NULL || entry->image == image)) {
            entry->active = false;
            entry->function(entry->argument);
        }
    }
}

static void *target_address(void)
{
    int (*function)(void) = ll_fixture_target;
    void *address = NULL;
    memcpy(&address, &function, sizeof(address));
    return address;
}

static void add_builtin(RunnerState *state, ll_builtin_fn insert,
                        void *insert_ctx, const char *name, void *address)
{
    if (state->host_symbol_count >= RUNNER_MAX_HOST_SYMBOLS) {
        fputs("host symbol map overflow\n", stderr);
        abort();
    }
    state->host_symbols[state->host_symbol_count].name = name;
    state->host_symbols[state->host_symbol_count].address = address;
    ++state->host_symbol_count;
    insert(insert_ctx, name, address, LL_STRENGTH_STRONG, LL_SYM_CODE);
}

static void populate_builtins(void *ctx, int retain_cafs,
                              ll_builtin_fn insert, void *insert_ctx)
{
    RunnerState *state = ctx;
    void (*record)(unsigned) = fixture_record_event;
    int (*atexit_fn)(void (*)(void *), void *, void *) = fixture_cxa_atexit;
    void (*finalize_fn)(void *) = fixture_cxa_finalize;
    void *address;

    (void)retain_cafs;
    address = target_address();
    add_builtin(state, insert, insert_ctx, "_ll_fixture_target", address);
    address = NULL;
    memcpy(&address, &record, sizeof(address));
    add_builtin(state, insert, insert_ctx, "_fixture_record_event", address);
    address = NULL;
    memcpy(&address, &atexit_fn, sizeof(address));
    add_builtin(state, insert, insert_ctx, "___cxa_atexit", address);
    address = NULL;
    memcpy(&address, &finalize_fn, sizeof(address));
    add_builtin(state, insert, insert_ctx, "___cxa_finalize", address);
}

static void host_log(void *ctx, int kind, const char *fmt, va_list ap)
{
    (void)ctx;
    (void)kind;
    (void)vfprintf(stderr, fmt, ap);
}

static _Noreturn void host_fatal(void *ctx, const char *fmt, va_list ap)
{
    (void)ctx;
    (void)vfprintf(stderr, fmt, ap);
    (void)fputc('\n', stderr);
    abort();
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

static int copy_sections(RunnerState *state, ObjectCode *object, bool relocated)
{
    int i;

    if (object->n_sections < 0) {
        fputs("negative section count\n", stderr);
        return 0;
    }
    if (state->sections == NULL) {
        state->section_count = (size_t)object->n_sections;
        state->sections = calloc(state->section_count == 0 ? 1 : state->section_count,
                                 sizeof(*state->sections));
        if (state->sections == NULL) {
            fputs("could not allocate section capture table\n", stderr);
            return 0;
        }
        for (i = 0; i < object->n_sections; ++i) {
            RunnerSectionCapture *capture = &state->sections[i];
            Section *section = &object->sections[i];
            capture->section = section;
            if (section->info != NULL) {
                capture->macho = section->info->macho_section;
            }
            if (section->size > RUNNER_MAX_SECTION_SIZE) {
                fputs("section exceeds capture limit\n", stderr);
                return 0;
            }
            if (section->size != 0 && section->start == NULL) {
                fputs("non-empty section has null start\n", stderr);
                return 0;
            }
            if (section->size != 0) {
                capture->loaded = malloc((size_t)section->size);
                capture->relocated = malloc((size_t)section->size);
                capture->post_init = malloc((size_t)section->size);
                if (capture->loaded == NULL || capture->relocated == NULL
                    || capture->post_init == NULL) {
                    fputs("could not allocate section byte snapshots\n", stderr);
                    return 0;
                }
            }
        }
    }

    if ((size_t)object->n_sections != state->section_count) {
        fputs("section count changed during capture\n", stderr);
        return 0;
    }
    for (i = 0; i < object->n_sections; ++i) {
        RunnerSectionCapture *capture = &state->sections[i];
        Section *section = &object->sections[i];
        if (section->size != 0) {
            uint8_t *destination = relocated ? capture->relocated : capture->loaded;
            memcpy(destination, section->start, (size_t)section->size);
        }
    }
    return 1;
}

static int copy_preinit_layout(RunnerState *state, ObjectCode *object)
{
    size_t i;
    if (object->info != NULL && object->info->got_start != NULL) {
        if (object->info->got_size > RUNNER_MAX_SECTION_SIZE) {
            fputs("GOT exceeds capture limit\n", stderr);
            return 0;
        }
        state->got_start = object->info->got_start;
        state->got_size = object->info->got_size;
        if (state->got_size != 0) {
            state->got_bytes = malloc(state->got_size);
            if (state->got_bytes == NULL) {
                fputs("could not allocate GOT snapshot\n", stderr);
                return 0;
            }
            memcpy(state->got_bytes, state->got_start, state->got_size);
        }
    }
    for (i = 0; i < state->section_count; ++i) {
        RunnerSectionCapture *capture = &state->sections[i];
        struct SectionFormatInfo *info;
        Stub *stub;
        size_t index = 0;
        if (capture->section->info == NULL) continue;
        info = capture->section->info;
        capture->stub_start = info->stub_offset;
        capture->stub_size = info->stub_size;
        capture->stub_count = info->nstubs;
        if (capture->stub_size > RUNNER_MAX_SECTION_SIZE) {
            fputs("stub reservation exceeds capture limit\n", stderr);
            return 0;
        }
        if (capture->stub_size != 0) {
            capture->stub_bytes = malloc(capture->stub_size);
            if (capture->stub_bytes == NULL) {
                fputs("could not allocate stub snapshot\n", stderr);
                return 0;
            }
            memcpy(capture->stub_bytes, capture->stub_start, capture->stub_size);
        }
        if (capture->stub_count != 0) {
            capture->stubs = calloc(capture->stub_count, sizeof(*capture->stubs));
            if (capture->stubs == NULL) {
                fputs("could not allocate stub metadata\n", stderr);
                return 0;
            }
        }
        for (stub = info->stubs; stub != NULL; stub = stub->next) {
            if (index >= capture->stub_count) {
                fputs("stub list exceeds declared count\n", stderr);
                return 0;
            }
            capture->stubs[index].address = stub->addr;
            capture->stubs[index].target = stub->target;
            capture->stubs[index].flags = stub->flags;
            ++index;
        }
        if (index != capture->stub_count) {
            fputs("stub list count mismatch\n", stderr);
            return 0;
        }
    }
    return 1;
}

static void register_object(void *ctx, ObjectCode *object)
{
    RunnerState *state = ctx;
    if (state->object != NULL) {
        fputs("runner accepts exactly one loaded object\n", stderr);
        abort();
    }
    if (objects != NULL) {
        objects->prev = object;
    }
    object->next = objects;
    objects = object;
    state->object = object;
    if (!copy_sections(state, object, false)) {
        abort();
    }
}

static void before_init(void *ctx, ObjectCode *object)
{
    RunnerState *state = ctx;
    if (object != state->object || !copy_sections(state, object, true)
        || !copy_preinit_layout(state, object)) {
        fputs("could not capture relocated sections before initialization\n", stderr);
        abort();
    }
    state->image_base = object->image;
    state->captured_before_init = true;
}

void runner_host_configure(RunnerState *state, ll_host_ops *host,
                           ll_config *config, bool debug)
{
    active_state = state;
    *host = (ll_host_ops){0};
    host->ctx = state;
    host->log = host_log;
    host->fatal = host_fatal;
    host->page_size = host_page_size;
    host->flush_icache = host_flush_icache;
    host->object_registered = register_object;
    host->before_init = before_init;
    host->populate_builtins = populate_builtins;

    *config = (ll_config){0};
    config->near_hint = &state->image_anchor;
    config->always_pic = true;
    if (debug) {
        config->debug_mask = LL_DBG_LINKER | LL_DBG_LINKER_VERBOSE;
    }
}

void runner_host_cleanup(RunnerState *state)
{
    size_t i;
    for (i = 0; i < state->section_count; ++i) {
        free(state->sections[i].loaded);
        free(state->sections[i].relocated);
        free(state->sections[i].post_init);
        free(state->sections[i].stub_bytes);
        free(state->sections[i].stubs);
    }
    free(state->got_bytes);
    free(state->sections);
    free(state->input_bytes);
    state->sections = NULL;
    state->input_bytes = NULL;
    active_state = NULL;
}
