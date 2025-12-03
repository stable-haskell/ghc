#include <ghcversion.h>
#  include <rts/PosixSource.h>
#include <Rts.h>

#include <HsFFI.h>

/*
 * Note [Boot library symbol visibility]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Boot library promotion to RTLD_GLOBAL is now handled in RTS initialization.
 * See Note [Promoting Boot Libraries to RTLD_GLOBAL] in rts/RtsStartup.c.
 *
 * This ensures all GHC API programs (not just ghc-iserv) properly export
 * boot library symbols before any user code is loaded via dlopen.
 */

int main (int argc, char *argv[])
{
    RtsConfig conf = defaultRtsConfig;

    // We never know what symbols GHC will look up in the future, so
    // we must retain CAFs for running interpreted code.
    conf.keep_cafs = 1;

    conf.rts_opts_enabled = RtsOptsAll;
    extern StgClosure ZCMain_main_closure;
    hs_main(argc, argv, &ZCMain_main_closure, conf);
}
