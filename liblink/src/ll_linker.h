/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2026
 *
 * Private state owned by one in-memory linker.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ll_host.h"
#include "ll_error.h"
#include "liblink/rts_friend.h"

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
#include <regex.h>
#endif

#if RTS_LINKER_USE_MMAP && !defined(mingw32_HOST_OS)
struct MemoryRegion {
    void *start;
    void *end;
    void *last;
};
#endif

#if defined(OBJFORMAT_PEi386)
typedef struct {
    HashTable *hash;
} LoadedDllCache;
struct _OpenedDLL;
#endif

struct m32_page_t;
struct ll_failure_scope;
struct ll_m32_reservation;

struct ll_linker {
    ll_compat_services services;
    ll_status error_status;
    char error_message[LL_ERROR_CAPACITY];
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
#if defined(SHN_XINDEX)
    ll_elf_word shndx_table_uninit_label;
#endif
#endif
#if !defined(OBJFORMAT_PEi386)
    void *rts_no_fini;
#endif

    void *mmap_32bit_base;
#if defined(mingw32_HOST_OS)
    void *base_addr;
    void *last_alloca;
    struct ll_m32_reservation *m32_reservations;
#elif RTS_LINKER_USE_MMAP
    struct MemoryRegion mmap_region;
#endif

#if defined(NEED_M32)
    struct m32_page_t *m32_free_page_pool;
    unsigned int m32_free_page_pool_size;
#endif

#if defined(OBJFORMAT_PEi386)
    LoadedDllCache loaded_dll_cache;
    struct _OpenedDLL *opened_dlls;
    bool load_dll_warn;
#endif

    /* Operation-local diagnostics and stack frames; markers live on objects. */
    struct ll_failure_scope *failure_scopes;
    ll_status primary_protection_error;
    char primary_protection_message[LL_ERROR_CAPACITY];
    bool quarantine_rejected;
};

#include "ll_begin_private.h"

/* Compatibility calls use this fixed owner until they receive an explicit L.
 * Its services exist before adjustor mappings and lazy linker initialization. */
extern ll_linker ll_default_linker;

#include "ll_end_private.h"
