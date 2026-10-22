#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_linker.h"

#include "linker/MMap.h"

#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif

/* Link objects into the lower 2Gb on x86_64 and AArch64.  GHC assumes the
 * small memory model on this architecture (see gcc docs,
 * -mcmodel=small).
 *
 * MAP_32BIT not available on OpenBSD/amd64
 */
#if defined(MAP_32BIT) && (defined(x86_64_HOST_ARCH) || (defined(aarch64_TARGET_ARCH) || defined(aarch64_HOST_ARCH)))
#define MAP_LOW_MEM
#define TRY_MAP_32BIT MAP_32BIT
#else
#define TRY_MAP_32BIT 0
#endif

/* MAP_ANONYMOUS is MAP_ANON on some systems,
   e.g. OS X (before Sierra), OpenBSD etc */
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

/* In order to simplify control flow a bit, some references to mmap-related
   definitions are blocked off by a C-level if statement rather than a CPP-level
   #if statement. Since those are dead branches when !RTS_LINKER_USE_MMAP, we
   just stub out the relevant symbols here
*/
#if !RTS_LINKER_USE_MMAP
#define munmap(x,y) /* nothing */
#define MAP_ANONYMOUS 0
#endif

void ll_init_linker_mmap(ll_linker *L) {
    L->mmap_32bit_base = L->services.cfg.mem_base != 0
        ? (void *)L->services.cfg.mem_base
        : LINKER_LOAD_BASE(L);
}

static const char *memoryAccessDescription(ll_linker *L, MemoryAccess mode)
{
  switch (mode) {
  case MEM_NO_ACCESS:    return "no-access";
  case MEM_READ_ONLY:    return "read-only";
  case MEM_READ_WRITE:   return "read-write";
  case MEM_READ_WRITE_THEN_READ_EXECUTE:
                         return "read-write-then-read-execute";
  case MEM_READ_EXECUTE: return "read-execute";
  case MEM_READ_WRITE_EXECUTE:
                         return "read-write-execute";
  default: LL_FATAL_FOR(L, "invalid MemoryAccess");
  }
}

#if defined(mingw32_HOST_OS)

/* A wrapper for VirtualQuery() providing useful debug output */
static int virtualQuery(ll_linker *L, void *baseAddr, PMEMORY_BASIC_INFORMATION info)
{
    int res = VirtualQuery (baseAddr, info, sizeof (*info));
    LL_IF_DEBUG_FOR(L, linker_verbose,
        LL_DEBUG_FOR(L, "Probing region 0x%p (0x%p) - 0x%p (%" LL_FMT_SIZE_T ") [%ld] with base 0x%p\n",
                   baseAddr,
                   info->BaseAddress,
                   (uint8_t *) info->BaseAddress + info->RegionSize,
                   info->RegionSize, info->State,
                   info->AllocationBase));
    if (!res) {
        LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "Querying 0x%p failed. Aborting..\n", baseAddr));
        return 1;
    }
    return 0;
}

static inline uintptr_t round_up(uintptr_t num, uint64_t factor)
{
    return num + factor - 1 - (num + factor - 1) % factor;
}

/*
 * Try and find a location in the VMMAP to allocate SZ bytes starting at
 * BASEADDR.  If successful then location to use is returned and the amount of
 * bytes you *must* allocate is returned in REQ.  You are free to use less but
 * you must allocate the amount given in REQ.  If not successful NULL.
 */
static void *allocateBytes(ll_linker *L, void* baseAddr, void *endAddr, size_t sz, size_t *req)
{
    SYSTEM_INFO sys;
    GetSystemInfo(&sys);

    LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "Requesting mapping of %" LL_FMT_SIZE_T " bytes between %p and %p\n",
                                sz, baseAddr, endAddr));

    MEMORY_BASIC_INFORMATION info;
    uint8_t *initialAddr = baseAddr;
    uint8_t *region = NULL;
    while (!region
           && initialAddr <= (uint8_t *) endAddr
           && (void *) initialAddr < sys.lpMaximumApplicationAddress)
    {
        int res = virtualQuery(L, initialAddr, &info);
        if (res) {
            return NULL;
        }

        if ((info.State & MEM_FREE) == MEM_FREE) {
            LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "Free range at 0x%p of %zu bytes\n",
                                        info.BaseAddress, info.RegionSize));

            if (info.RegionSize >= sz) {
                if (info.AllocationBase == 0) {
                    size_t needed_sz = round_up (sz, sys.dwAllocationGranularity);
                    if (info.RegionSize >= needed_sz) {
                        LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "Range is unmapped, Allocation "
                                                    "required by granule...\n"));
                        *req = needed_sz;
                        region
                        = (void*)(uintptr_t)round_up ((uintptr_t)initialAddr,
                                                        sys.dwAllocationGranularity);
                        LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "Requested %" PRId64 ", rounded: %"
                                                    PRId64 ".\n", sz, *req));
                        LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "Aligned region claimed 0x%p -> "
                                                    "0x%p.\n", initialAddr, region));
                    }
                } else {
                    LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "Range is usable for us, claiming...\n"));
                    *req = sz;
                    region = initialAddr;
                }
            }
        }
        initialAddr = (uint8_t *) info.BaseAddress + info.RegionSize;
    }

    return region;
}

/* Find free address space for mapping anonymous memory. */
static void *allocateLocalBytes(ll_linker *L, size_t sz, size_t *req)
{
    // We currently don't attempt to take address space from the region below
    // the image as malloc() tends to like to use this space, but we could do if
    // necessary.
    size_t max_range = 0x7fffffff - sz;

    if (L->base_addr == NULL) {
        L->base_addr = GetModuleHandleW(NULL);
    }
    uint8_t *end_addr = (uint8_t *) L->base_addr + max_range;

    // We track the location of the last allocation to avoid having to
    // do a linear search of address space looking for space on every allocation
    // as this can easily devolve into quadratic complexity.
    if (L->last_alloca == NULL) {
        // Start the search at the image base
        L->last_alloca = L->base_addr;
    }

    void *result = NULL;
    result = allocateBytes (L, L->last_alloca, end_addr, sz, req);
    if (result == NULL) {
        // We failed to find suitable address space; restart the search at base_addr.
        result = allocateBytes (L, L->base_addr, end_addr, sz, req);
    }

    if (result != NULL) {
        L->last_alloca = (uint8_t *) result + *req;
    }
    return result;
}

static DWORD
memoryAccessToProt(ll_linker *L, MemoryAccess access)
{
  switch (access) {
  case MEM_NO_ACCESS:    return PAGE_NOACCESS;
  case MEM_READ_ONLY:    return PAGE_READONLY;
  case MEM_READ_WRITE:   return PAGE_READWRITE;
  case MEM_READ_WRITE_THEN_READ_EXECUTE:
                         return PAGE_READWRITE;
  case MEM_READ_EXECUTE: return PAGE_EXECUTE_READ;
  case MEM_READ_WRITE_EXECUTE:
                         return PAGE_EXECUTE_READWRITE;
  default: LL_FATAL_FOR(L, "invalid MemoryAccess");
  }
}

void *
ll_mmap_anon(ll_linker *L LL_UNUSED, size_t bytes)
{
  return VirtualAlloc(NULL, bytes, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
}

//
// Returns NULL on failure.
//
void *
ll_mmap_anon_for_linker(ll_linker *L, size_t bytes)
{
  size_t size = 0;
  /* For linking purposes we want to load code within a 4GB range from the
     load address of the application.  As such we need to find a location to
     allocate at.   */
  void* region = allocateLocalBytes (L, bytes, &size);
  if (region == NULL) {
      return NULL;
  }
  return VirtualAlloc(region, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
}

void
ll_munmap_for_linker(ll_linker *L, void *addr, size_t bytes, const char *caller)
{
  if (VirtualFree(addr, 0, MEM_RELEASE) == 0) {
    LL_LOG_SYSERROR_FOR(L, "munmapForLinker: %s: Failed to unmap %zd bytes at %p",
                  caller, bytes, addr);
  }
}

/**
 * Change the allowed access modes of a region of memory previously allocated
 * with mmapAnonForLinker.
 */
void
ll_mprotect_for_linker(ll_linker *L, void *start, size_t len, MemoryAccess mode)
{
  DWORD old;
  if (len == 0) {
    return;
  }
  DWORD prot = memoryAccessToProt(L, mode);

  if (VirtualProtect(start, len, prot, &old) == 0) {
    LL_LOG_SYSERROR_FOR(L, "mprotectForLinker: failed to protect %zd bytes at %p as %s",
                  len, start, memoryAccessDescription(L, mode));
    LL_ASSERT_FOR(L, false);
  }
}

#elif RTS_LINKER_USE_MMAP

static int
memoryAccessToProt(ll_linker *L, MemoryAccess access)
{
    switch (access) {
    case MEM_NO_ACCESS:    return 0;
    case MEM_READ_ONLY:    return PROT_READ;
    case MEM_READ_WRITE:   return PROT_READ | PROT_WRITE;
    case MEM_READ_WRITE_THEN_READ_EXECUTE:
#  if defined(netbsd_HOST_OS)
        /* PROT_MPROTECT(PROT_EXEC) means that the pages are going to be
         * marked as executable in the future. On NetBSD requesting
         * additional permissions with mprotect(2) only succeeds when
         * permissions were initially requested in this manner.
         */
                           return PROT_READ | PROT_WRITE | PROT_MPROTECT(PROT_EXEC);
#  else
                           return PROT_READ | PROT_WRITE;
#  endif
    case MEM_READ_EXECUTE: return PROT_READ | PROT_EXEC;
    case MEM_READ_WRITE_EXECUTE:
                           return PROT_READ | PROT_WRITE | PROT_EXEC;
    default: LL_FATAL_FOR(L, "invalid MemoryAccess");
    }
}

static void *
doMmap(ll_linker *L, void *map_addr, size_t bytes, int prot, uint32_t flags, int fd, int offset)
{
    flags |= MAP_PRIVATE;

    LL_IF_DEBUG_FOR(L, linker_verbose,
             LL_DEBUG_FOR(L, "mmapForLinker: \tprotection %#0x\n", prot));
    LL_IF_DEBUG_FOR(L, linker_verbose,
             LL_DEBUG_FOR(L, "mmapForLinker: \tflags      %#0x\n", flags));
    LL_IF_DEBUG_FOR(L, linker_verbose,
             LL_DEBUG_FOR(L, "mmapForLinker: \tsize       %#0zx\n", bytes));
    LL_IF_DEBUG_FOR(L, linker_verbose,
             LL_DEBUG_FOR(L, "mmapForLinker: \tmap_addr   %p\n", map_addr));

    void * result = mmap(map_addr, bytes, prot, flags, fd, offset);
    if (result == MAP_FAILED) {
        LL_LOG_SYSERROR_FOR(L, "mmap %zx bytes at %p", bytes, map_addr);
        ll_report_mem_failure_for(L);
        LL_LOG_ERROR_FOR(L, "Try specifying an address with +RTS -xm<addr> -RTS");
        return NULL;
    }
    return result;
}


/* A region of memory that we might map into. Its last field is the end of
 * the last mapping, where we start searching on the next allocation. */
static struct MemoryRegion *
nearImage(ll_linker *L) {
    if (L->mmap_region.end == NULL) {
        L->mmap_region.start = L->mmap_32bit_base;
        L->mmap_region.end = (uint8_t *) L->mmap_region.start + 0x80000000;
        L->mmap_region.last = L->mmap_region.start;
    }
    return &L->mmap_region;
}

static void *
mmapAnywhere (
        ll_linker *L,
        size_t bytes,
        MemoryAccess access,
        uint32_t flags,
        int fd,
        int offset)
{
    int prot = memoryAccessToProt(L, access);
    return doMmap(L, NULL, bytes, prot, flags, fd, offset);
}

static void *
mmapInRegion (
        ll_linker *L,
        struct MemoryRegion *region,
        size_t bytes,
        MemoryAccess access,
        uint32_t flags,
        int fd,
        int offset)
{
    bool wrapped = false;
    int prot = memoryAccessToProt(L, access);
    void *p = region->last;
    while (1) {
        void *result = doMmap(L, p, bytes, prot, flags, fd, offset);
        if (result == NULL) {
            // The mapping failed
            return NULL;
        } else if (result >= region->start && result < region->end) {
            // Success!
            region->last = (uint8_t *) result + bytes;
            return result;
        } else if (wrapped) {
            // We failed to find a suitable mapping
            munmap(result, bytes);
            ll_report_mem_failure_for(L);
            LL_LOG_ERROR_FOR(L, "mmapForLinker: failed to mmap() memory between %p and %p; "
                       "asked for %zu bytes at %p. "
                       "Try specifying an address with +RTS -xm<addr> -RTS",
                       region->start, region->end, bytes, p);
            return NULL;
        } else if (result < region->start) {
            // Uh oh, we assume that mmap() will only give us a
            // an address at or after the requested address.
            // Try bump forward by a bit and try again.
            p = (uint8_t *) p + bytes;
        } else if (result >= region->end) {
            // mmap() gave us too high an address; wrap around and try again
            wrapped = true;
            p = region->start;
       }

        munmap(result, bytes);
    }
}

/*
 * Map memory for code.
 * Returns NULL on failure.
 */
void *
ll_mmap_for_linker(ll_linker *L, size_t bytes, MemoryAccess access, uint32_t flags, int fd, int offset)
{
    bytes = ll_round_up_to_page_for(L, bytes);
    struct MemoryRegion *region;

    LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L, "mmapForLinker: start\n"));
    if (L->services.cfg.always_pic) {
        /* make no attempt at mapping low memory if we are assuming PIC */
        region = NULL;
    } else {
        region = nearImage(L);
    }

    /* Use MAP_32BIT if appropriate */
    if (region && region->end <= (void *) 0xffffffff) {
        flags |= TRY_MAP_32BIT;
    }

    void *result;
    if (region) {
        result = mmapInRegion(L, region, bytes, access, flags, fd, offset);
    }
    else {
        result = mmapAnywhere(L, bytes, access, flags, fd, offset);
    }
    LL_IF_DEBUG_FOR(L, linker_verbose,
             LL_DEBUG_FOR(L, "mmapForLinker: mapped %zd bytes starting at %p\n",
                        bytes, result));
    LL_IF_DEBUG_FOR(L, linker_verbose,
             LL_DEBUG_FOR(L, "mmapForLinker: done\n"));
    return result;
}

/*
 * Map read/write pages anywhere in memory. Returns NULL on failure.
 */
void *
ll_mmap_anon(ll_linker *L, size_t bytes)
{
    return mmapAnywhere(L, bytes, MEM_READ_WRITE_THEN_READ_EXECUTE, MAP_ANONYMOUS, -1, 0);
}

/*
 * Map read/write pages in low memory. Returns NULL on failure.
 */
void *
ll_mmap_anon_for_linker(ll_linker *L, size_t bytes)
{
    return ll_mmap_for_linker (L, bytes, MEM_READ_WRITE_THEN_READ_EXECUTE, MAP_ANONYMOUS, -1, 0);
}

void ll_munmap_for_linker(ll_linker *L, void *addr, size_t bytes, const char *caller)
{
    int r = munmap(addr, bytes);
    if (r == -1) {
        // Should we abort here?
        LL_LOG_SYSERROR_FOR(L, "munmap: %s", caller);
    }
}

/* Note [Memory protection in the linker]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * For many years the linker would simply map all of its memory
 * with PROT_READ|PROT_WRITE|PROT_EXEC. However operating systems have been
 * becoming increasingly reluctant to accept this practice (e.g. #17353,
 * #12657) and for good reason: writable code is ripe for exploitation.
 *
 * Consequently mmapForLinker now maps its memory with PROT_READ|PROT_WRITE.
 * After the linker has finished filling/relocating the mapping it must then
 * call mprotectForLinker on the sections of the mapping which
 * contain executable code.
 *
 * Note that the m32 allocator handles protection of its allocations. For this
 * reason the caller to m32_alloc() must tell the allocator whether the
 * allocation needs to be executable. The caller must then ensure that they
 * call m32_allocator_flush() after they are finished filling the region, which
 * will cause the allocator to change the protection bits to
 * PROT_READ|PROT_EXEC.
 *
 */

/*
 * Mark an portion of a mapping previously reserved by mmapForLinker
 * as executable (but not writable).
 */
void ll_mprotect_for_linker(ll_linker *L, void *start, size_t len, MemoryAccess mode)
{
    if (len == 0) {
        return;
    }
    LL_IF_DEBUG_FOR(L, linker_verbose,
             LL_DEBUG_FOR(L, "mprotectForLinker: protecting %" LL_FMT_WORD
                        " bytes starting at %p as %s\n",
                        (ll_w)len, start, memoryAccessDescription(L, mode)));

    int prot = memoryAccessToProt(L, mode);

    if (mprotect(start, len, prot) == -1) {
        LL_LOG_SYSERROR_FOR(L, "mprotectForLinker: failed to protect %zd bytes at %p as %s",
                      len, start, memoryAccessDescription(L, mode));
    }
}
#endif

/* Frozen RTS friend entry points select the fixed compatibility owner. */
void initLinkerMMap(void)
{
    ll_init_linker_mmap(&ll_default_linker);
}

#if defined(mingw32_HOST_OS) || RTS_LINKER_USE_MMAP
void *mmapAnon(size_t bytes)
{
    return ll_mmap_anon(&ll_default_linker, bytes);
}

void mprotectForLinker(void *start, size_t len, MemoryAccess mode)
{
    ll_mprotect_for_linker(&ll_default_linker, start, len, mode);
}

void munmapForLinker(void *addr, size_t bytes, const char *caller)
{
    ll_munmap_for_linker(&ll_default_linker, addr, bytes, caller);
}

#endif
