#include "ghcconfig.h"
#include "Rts.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "linker_unload_trace.h"

#define ITERATIONS 1000

#if defined(mingw32_HOST_OS)
#define OBJPATH L"Test.o"
#else
#define OBJPATH "Test.o"
#endif

typedef int testfun(int);

extern void loadPackages(void);

int main (int argc, char *argv[])
{
    testfun *f;
    int i, r;
    Capability *cap;

    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    linker_unload_trace("rts.init.before");
    hs_init_ghc(&argc, &argv, conf);
    linker_unload_trace("rts.init.after");

    linker_unload_trace("linker.init.before");
    /* Exclude GC during raw linker calls. Release the capability before
     * entering Haskell or requesting a collection. */
    cap = rts_lock();
    initLinker_(0);
    rts_unlock(cap);
    linker_unload_trace("linker.init.after");

    linker_unload_trace("packages.load.before");
    loadPackages();
    linker_unload_trace("packages.load.after");

    linker_unload_trace("loop1.begin");
    for (i=0; i < ITERATIONS; i++) {
        if (i == 0) linker_unload_trace("loop1.load.before");
        cap = rts_lock();
        r = loadObj(OBJPATH);
        if (i == 0) linker_unload_trace("loop1.load.after");
        if (!r) {
            rts_unlock(cap);
            errorBelch("loadObj(%" PATH_FMT ") failed", OBJPATH);
            exit(1);
        }
        if (i == 0) linker_unload_trace("loop1.resolve.before");
        r = resolveObjs();
        if (i == 0) linker_unload_trace("loop1.resolve.after");
        if (!r) {
            rts_unlock(cap);
            errorBelch("resolveObjs failed");
            exit(1);
        }
        if (i == 0) linker_unload_trace("loop1.lookup.before");
#if LEADING_UNDERSCORE
        f = lookupSymbol("_f");
#else
        f = lookupSymbol("f");
#endif
        if (i == 0) linker_unload_trace("loop1.lookup.after");
        if (!f) {
            rts_unlock(cap);
            errorBelch("lookupSymbol failed");
            exit(1);
        }
        if (i == 0) linker_unload_trace("loop1.call.before");
        rts_unlock(cap);
        r = f(3);
        if (i == 0) linker_unload_trace("loop1.call.after");
        if (r != 4) {
            errorBelch("call failed; %d", r);
            exit(1);
        }
        if (i == 0) linker_unload_trace("loop1.unload.before");
        cap = rts_lock();
        unloadObj(OBJPATH);
        rts_unlock(cap);
        if (i == 0) linker_unload_trace("loop1.unload.after");
        if (i == 0) linker_unload_trace("loop1.gc.before");
        performMajorGC();
        if (i == 0) linker_unload_trace("loop1.gc.after");
        printf("%d ", i);
        fflush(stdout);
    }

    linker_unload_trace("loop1.end");
    linker_unload_trace("loop2.begin");
    for (i=0; i < ITERATIONS; i++) {
        if (i == 0) linker_unload_trace("loop2.load.before");
        cap = rts_lock();
        r = loadObj(OBJPATH);
        if (i == 0) linker_unload_trace("loop2.load.after");
        if (!r) {
            rts_unlock(cap);
            errorBelch("loadObj(%" PATH_FMT ") failed", OBJPATH);
            exit(1);
        }
        if (i == 0) linker_unload_trace("loop2.resolve.before");
        r = resolveObjs();
        if (i == 0) linker_unload_trace("loop2.resolve.after");
        if (!r) {
            rts_unlock(cap);
            errorBelch("resolveObjs failed");
            exit(1);
        }
        if (i == 0) linker_unload_trace("loop2.lookup.before");
#if LEADING_UNDERSCORE
        f = lookupSymbol("_f");
#else
        f = lookupSymbol("f");
#endif
        if (i == 0) linker_unload_trace("loop2.lookup.after");
        if (!f) {
            rts_unlock(cap);
            errorBelch("lookupSymbol failed");
            exit(1);
        }
        if (i == 0) linker_unload_trace("loop2.call.before");
        rts_unlock(cap);
        r = f(3);
        if (i == 0) linker_unload_trace("loop2.call.after");
        if (r != 4) {
            errorBelch("call failed; %d", r);
            exit(1);
        }
        // check that we can purge first, then unload
        if (i == 0) linker_unload_trace("loop2.purge.before");
        cap = rts_lock();
        purgeObj(OBJPATH);
        rts_unlock(cap);
        if (i == 0) linker_unload_trace("loop2.purge.after");
        if (i == 0) linker_unload_trace("loop2.purge_gc.before");
        performMajorGC();
        if (i == 0) linker_unload_trace("loop2.purge_gc.after");
        if (i == 0) linker_unload_trace("loop2.unload.before");
        cap = rts_lock();
        unloadObj(OBJPATH);
        rts_unlock(cap);
        if (i == 0) linker_unload_trace("loop2.unload.after");
        if (i == 0) linker_unload_trace("loop2.unload_gc.before");
        performMajorGC();
        if (i == 0) linker_unload_trace("loop2.unload_gc.after");
        printf("%d ", i);
        fflush(stdout);
    }

    linker_unload_trace("loop2.end");
    linker_unload_trace("rts.exit.before");
    hs_exit();
    linker_unload_trace("rts.exit.after");
    exit(0);
}
