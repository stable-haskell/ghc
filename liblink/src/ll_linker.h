/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Private state owned by one in-memory linker.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ll_host.h"
#include "liblink/rts_friend.h"

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
#include <regex.h>
#endif

struct ll_linker {
    ll_compat_services services;
    /* One mutex protects this owner's linker state in every RTS way. */
    ll_mutex linker_mutex;
    ll_gc_state gc;
    StrHashTable *symhash;
    int linker_init_done;
    ll_hs_bool missing_update_warn;

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
    void *dl_prog_handle;
    regex_t re_invalid;
    regex_t re_realso;
#endif

#if defined(OBJFORMAT_ELF)
    void *rts_dynamic;
#endif
#if !defined(OBJFORMAT_PEi386)
    void *rts_no_fini;
#endif


};

#include "ll_begin_private.h"

/* Compatibility calls use this fixed owner until they receive an explicit L.
 * Its services exist before adjustor mappings and lazy linker initialization. */
extern ll_linker ll_default_linker;

#include "ll_end_private.h"
