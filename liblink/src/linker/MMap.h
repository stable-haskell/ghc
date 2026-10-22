#pragma once

#include "liblink/rts_friend.h"

#include "ll_begin_private.h"

#if defined(aarch64_HOST_ARCH)
// On AArch64 MAP_32BIT is not available but we are still bound by the small
// memory model. Consequently we still try using the MAP_LOW_MEM allocation
// strategy.
#define MAP_LOW_MEM
#endif

/*
 * Note [MAP_LOW_MEM]
 * ~~~~~~~~~~~~~~~~~~
 * Due to the small memory model (see above), on x86_64 and AArch64 we have to
 * map all our non-PIC object files into the low 2Gb of the address space (why
 * 2Gb and not 4Gb?  Because all addresses must be reachable using a 32-bit
 * signed PC-relative offset). On x86_64 Linux we can do this using the
 * MAP_32BIT flag to mmap(), however on other OSs (e.g. *BSD, see #2063, and
 * also on Linux inside Xen, see #2512), we can't do this.  So on these
 * systems, we have to pick a base address in the low 2Gb of the address space
 * and try to allocate memory from there.
 *
 * The same holds for aarch64, where the default, even with PIC, model
 * is 4GB. The linker is free to emit AARCH64_ADR_PREL_PG_HI21
 * relocations.
 *
 * We pick a default address based on the OS, but also make this
 * configurable via an RTS flag (+RTS -xm)
 */

#if defined(aarch64_TARGET_ARCH) || defined(aarch64_HOST_ARCH)
// Try to use stg_upd_frame_info as the base. We need to be within +-4GB of that
// address, otherwise we violate the aarch64 memory model. Any object we load
// can potentially reference any of the ones we bake into the binary (and list)
// in RtsSymbols. Thus we'll need to be within +-4GB of those,
// stg_upd_frame_info is a good candidate as it's referenced often.
#define LINKER_LOAD_BASE(L) ((void *)(L)->services.cfg.near_hint)
#elif defined(x86_64_HOST_ARCH) && defined(mingw32_HOST_OS)
// On Windows (which now uses high-entropy ASLR by default) we need to ensure
// that we map code near the executable image. We use stg_upd_frame_info as a
// proxy for the image location.
#define LINKER_LOAD_BASE(L) ((void *)(L)->services.cfg.near_hint)
#elif defined(MAP_32BIT) || defined(darwin_HOST_OS) || defined(aarch64_HOST_ARCH) || defined(arm_HOST_ARCH)
// Try to use MAP_32BIT
#define LINKER_LOAD_BASE(L) ((void *) 0x0)
#else
// A guess: 1 GB.
#define LINKER_LOAD_BASE(L) ((void *) 0x40000000)
#endif

/* Owner-aware implementations; raw mapping addresses carry no owner. */
void ll_init_linker_mmap(ll_linker *L);
void *ll_mmap_anon(ll_linker *L, size_t bytes);
void *ll_mmap_anon_for_linker(ll_linker *L, size_t bytes);
void ll_mprotect_for_linker(ll_linker *L, void *start, size_t len,
                            MemoryAccess mode);
void ll_munmap_for_linker(ll_linker *L, void *addr, size_t bytes,
                          const char *caller);
#if !defined(mingw32_HOST_OS)
void *ll_mmap_for_linker(ll_linker *L, size_t bytes, MemoryAccess prot,
                         uint32_t flags, int fd, int offset);
#endif

/* Temporary default adapters until the remaining private callers migrate. */
// Map read/write anonymous memory, enforcing the constraint of
// placing the mapping within 4GB of the executable image.
void *mmapAnonForLinker (size_t bytes);

#if !defined(mingw32_HOST_OS)
// Map a file.
//
// Note that this not available on Windows since file mapping on Windows is
// sufficiently different to warrant its own interface.
void *mmapForLinker (size_t bytes, MemoryAccess prot, uint32_t flags, int fd, int offset);
#endif

#include "ll_end_private.h"
