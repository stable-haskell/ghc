/* One-object native relocation capture for the arm64 Darwin host backend. */

#include "runner_internal.h"

#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

static int read_input(RunnerState *state, const char *path)
{
    FILE *file;
    struct stat st;
    size_t got;

    if (stat(path, &st) != 0 || st.st_size < 0
        || (uintmax_t)st.st_size > (uintmax_t)RUNNER_MAX_INPUT_SIZE) {
        fprintf(stderr, "invalid or oversized input object: %s\n", path);
        return 0;
    }
    state->input_size = (size_t)st.st_size;
    state->input_bytes = malloc(state->input_size == 0 ? 1 : state->input_size);
    if (state->input_bytes == NULL) {
        fputs("could not allocate input snapshot\n", stderr);
        return 0;
    }
    file = fopen(path, "rb");
    if (file == NULL) {
        perror("fopen input object");
        return 0;
    }
    got = fread(state->input_bytes, 1, state->input_size, file);
    if (got != state->input_size || ferror(file)) {
        fputs("could not read complete input object\n", stderr);
        (void)fclose(file);
        return 0;
    }
    if (fclose(file) != 0) {
        fputs("could not close input object\n", stderr);
        return 0;
    }
    state->input_path = path;
    return 1;
}

int main(int argc, char **argv)
{
    RunnerState state = {0};
    ll_compat_host_ops host;
    ll_compat_config config;
    const char *object_path;
    bool debug = argc == 3 && strcmp(argv[1], "--debug") == 0;
    int ok;
    size_t i;

    if ((!debug && argc != 2) || (debug && argc != 3)) {
        fprintf(stderr, "usage: %s [--debug] OBJECT\n", argv[0]);
        return 2;
    }
    object_path = argv[debug ? 2 : 1];
    if (!read_input(&state, object_path)) return 2;

    runner_host_configure(&state, &host, &config, debug);
    ll_install_host(&host, &config);
    initLinkerMMap();
    ll_compat_initLinker();

    if (!ll_compat_loadObj((ll_compat_pathchar *)object_path)) {
        fputs("ll_compat_loadObj failed\n", stderr);
        return 3;
    }
    if (state.object == NULL) {
        fputs("loader did not register the input object\n", stderr);
        return 4;
    }
    ok = ll_compat_resolveObjs();
    if (!ok || !state.captured_before_init) {
        fprintf(stderr,
                "ll_compat_resolveObjs failed before relocation capture "
                "(ok=%d, captured=%d)\n",
                ok, state.captured_before_init ? 1 : 0);
        return 5;
    }
    for (i = 0; i < state.section_count; ++i) {
        RunnerSectionCapture *capture = &state.sections[i];
        if (capture->section->size != 0) {
            memcpy(capture->post_init, capture->section->start,
                   (size_t)capture->section->size);
        }
    }
    if (!runner_json_emit(&state)) return 6;

    /* Finalizers and module term arrays are outside the pre-init snapshot. */
    exitLinker();
    runner_host_cleanup(&state);
    return 0;
}
