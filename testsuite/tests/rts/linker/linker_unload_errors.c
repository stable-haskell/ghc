#include "ghcconfig.h"
#include "Rts.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned operation;
static unsigned target_iteration;
static unsigned iteration;
static unsigned at_target;
static unsigned injection_fired;

static HsInt observed_load(pathchar *path)
{
    ++iteration;
    at_target = iteration == target_iteration;
    if (at_target && operation == 1) { injection_fired = 1; return 0; }
    return loadObj(path);
}

static HsInt observed_resolve(void)
{
    if (at_target && operation == 2) { injection_fired = 1; return 0; }
    return resolveObjs();
}

static void *observed_lookup(char *name)
{
    if (at_target && operation == 3) { injection_fired = 1; return NULL; }
    return lookupSymbol(name);
}

static _Noreturn void checked_exit(int status)
{
    if (status != 1 || !injection_fired || iteration != target_iteration)
        _Exit(96);
    /* A retained capability would block this real collector request. */
    performMajorGC();
    (void)fputs("capability released on error\n", stderr);
    hs_exit();
    _Exit(1);
}

#define main guarded_fixture_main
#define loadObj observed_load
#define resolveObjs observed_resolve
#define lookupSymbol observed_lookup
#define exit checked_exit
#include "linker_unload.c"
#undef main
#undef loadObj
#undef resolveObjs
#undef lookupSymbol
#undef exit

int main(int argc, char **argv)
{
    static const char *const modes[] = {
        "load1", "resolve1", "lookup1", "load2", "resolve2", "lookup2"
    };
    const char *mode = getenv("UNLOAD_CAPABILITY_NEGATIVE");
    if (mode == NULL) return 97;
    for (unsigned i = 0; i < sizeof(modes) / sizeof(modes[0]); ++i) {
        if (strcmp(mode, modes[i]) == 0) {
            operation = i % 3 + 1;
            target_iteration = i < 3 ? 1 : 1001;
            return guarded_fixture_main(argc, argv);
        }
    }
    return 98;
}
