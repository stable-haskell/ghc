/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "linker/M32Alloc.h"
#include "ll_linker.h"
#include "linker/MMap.h"
#include "linker/M32Reservations.h"

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*
Note [Compile Time Trickery]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This file implements two versions of each of the `m32_*` functions. At the top
of the file there is the real implementation (compiled in when
`NEED_M32` is true) and a dummy implementation that exists only to
satisfy the compiler and which should never be called. If any of these dummy
implementations are called the program will abort.

The rationale for this is to allow the calling code to be written without using
the C pre-processor (CPP) `#if` hackery. The value of `NEED_M32` is
known at compile time, allowing code like:

    if (NEED_M32)
        m32_allocator_init();

will be compiled to call to `m32_allocator_init` if  `NEED_M32` is
true and will be optimised away to nothing if `NEED_M32` is false.
However, regardless of the value of `NEED_M32` the compiler will
still check the call for syntax and correct function parameter types.

*/

#if defined(NEED_M32)

/* Keep generic mapping semantics separate from Windows M32 ownership. */
static void *m32_map(ll_linker *L, size_t bytes)
{
#if defined(mingw32_HOST_OS)
    return ll_m32_map_for(L, bytes);
#else
    return ll_mmap_anon_for_linker(L, bytes);
#endif
}

static void m32_unmap(ll_linker *L, void *address, size_t bytes,
                       const char *caller LL_UNUSED)
{
#if defined(mingw32_HOST_OS)
    (void)ll_m32_unmap_for(L, address, bytes);
    (void)ll_m32_release_idle_for(L, false);
#else
    ll_munmap_for_linker(L, address, bytes, caller);
#endif
}

/*
Note [M32 Allocator]
~~~~~~~~~~~~~~~~~~~~
A memory allocator that allocates only pages in the 32-bit range (lower 2GB).
This is useful on 64-bit platforms to ensure that addresses of allocated
objects can be referenced with a 32-bit relative offset.

Initially, the linker used `mmap` to allocate a page per object. Hence it
wasted a lot of space for small objects (see #9314). With this allocator, we
try to fill pages as much as we can for small objects.

The interface
-------------

The allocator exposes three operations:

 * m32_allocator_new creates a new allocator; an allocator may be configured
   to allocate code (readable/executable) pages or data (readable/writeable)
   pages.

 * m32_alloc uses an allocator to allocated a (aligned) bit of memory
 *
 * m32_allocator_flush is used to indicate that the pages allocated thusfar
   have been filled. This will protect the pages.

 * m32_allocator_free is used to free the allocator and the pages it has
   allocated.

How does it work?
-----------------

The allocator manages two kinds of allocations:

 * small allocations, which are allocated into a set of "nursery" pages
   (recorded in m32_allocator_t.pages; the size of the set is <= M32_MAX_PAGES)

 * large allocations are those larger than a page and are mapped directly

Each page (or the first page of a large allocation) begins with a m32_page_t
header, which records various information depending upon what stage of its
life-cycle it is in:

 * in the case of a page in the small-allocation nursery: the number of bytes
   allocated into the page thusfar

 * in the case of a filled page:
    * the size of the mapping (PAGE_SIZE in the case
      of a nursery page, or greater in the case of a page arising from a large
      allocation)

Allocation (in the case of a small request) consists of walking the nursery to
find a page that will accommodate the request. If none exists then we allocate a
new nursery page (flushing an existing one to the filled list if the nursery is
full).

The allocator maintains two linked lists of filled pages, both linked together
with m32_page_t.link:

 * unprotected_pages records pages that have been filled but have not yet been
   protected (e.g. due to a call to m32_allocator_flush)

 * protect_pages records pages that have been filled and protected

m32_allocator_flush does two things:

 * it flushes all pages from the nursery

 * it protects the pages in m32_allocator_t.unprotected_list (and formerly in
   the nursery) and moves them
   to protected_list.

For large objects, the remaining space at the end of the last page is left
unused by the allocator. It can be used with care as it will be freed with the
associated large object. GHC linker uses this feature/hack, hence changing the
implementation of the M32 allocator must be done with care (i.e. do not try to
improve the allocator to avoid wasting this space without modifying the linker
code accordingly).

To avoid unnecessary mapping/unmapping we maintain a global list of free pages
(which can grow up to M32_MAX_FREE_PAGE_POOL_SIZE long). Pages on this list
have the usual m32_page_t header and are linked together with
m32_page_t.free_page.next. When run out of free pages we allocate a chunk of
M32_MAP_PAGES to both avoid fragmenting our address space and amortize the
runtime cost of the mapping.

The allocator is *not* thread-safe.

*/

// Enable internal consistency checking
#if LL_DEBUG_CODE
#define M32_DEBUG
#endif

#define ROUND_UP(x,size) ((x + size - 1) & ~(size - 1))
#define ROUND_DOWN(x,size) (x & ~(size - 1))

/****************************************************************************
 * M32 ALLOCATOR (see Note [M32 Allocator]
 ***************************************************************************/

/* How many open pages each allocator will keep around? */
#define M32_MAX_PAGES 32
/* How many pages should we map at once when re-filling the free page pool? */
#define M32_MAP_PAGES 32
/* Upper bound on the number of pages to keep in the free page pool */
#define M32_MAX_FREE_PAGE_POOL_SIZE 256

/* A utility to verify that a given address is "acceptable" for use by m32. */
static bool
is_okay_address(ll_linker *L, void *p) {
  int8_t *here = LINKER_LOAD_BASE(L);
  ssize_t displacement = (int8_t *) p - here;
  // if we assume -fPIC, we don't care where we load code.
  // But we still want to use the m32 allocator to avoid fragmentation (#24432)
  return L->services.cfg.always_pic
         || ((displacement > -0x7fffffff) && (displacement < 0x7fffffff));
}

enum m32_page_type {
  FREE_PAGE,    // a page in the free page pool
  NURSERY_PAGE, // a nursery page
  FILLED_PAGE,  // a page on the filled list
};

/**
 * Page header
 *
 * Every page (or large allocation) allocated with m32 has one of these at its
 * start.
 */
struct m32_page_t {
  union {
    // Pages (or large allocations) that have been filled and are in either the
    // unprotected_list or protected_list are linked together with this field.
    struct {
      uint32_t size;
      struct m32_page_t *next;
    } filled_page;

    // Pages in the small-allocation nursery encode their current allocation
    // offset here.
    size_t current_size;

    // Pages in the global free page pool are linked via this field.
    struct {
      struct m32_page_t *next;
    } free_page;
  };
#if defined(M32_DEBUG)
  enum m32_page_type type;
#endif
  uint8_t contents[];
};

/* Consistency-checking infrastructure */
#if defined(M32_DEBUG)
static void ASSERT_PAGE_ALIGNED(ll_linker *L, void *page) {
  const size_t pgsz = ll_page_size_for(L);
  if ((((uintptr_t) page) & (pgsz-1)) != 0) {
    LL_FATAL_FOR(L, "m32: invalid page alignment");
  }
}
static void ASSERT_VALID_PAGE(ll_linker *L, struct m32_page_t *page) {
  ASSERT_PAGE_ALIGNED(L, page);
  switch (page->type) {
  case FREE_PAGE:
  case NURSERY_PAGE:
  case FILLED_PAGE:
    break;
  default:
    LL_FATAL_FOR(L, "m32: invalid page state\n");
  }
}
static void ASSERT_PAGE_TYPE(ll_linker *L, struct m32_page_t *page, enum m32_page_type ty) {
  if (page->type != ty) { LL_FATAL_FOR(L, "m32: unexpected page type"); }
}
static void ASSERT_PAGE_NOT_FREE(ll_linker *L, struct m32_page_t *page) {
  if (page->type == FREE_PAGE) { LL_FATAL_FOR(L, "m32: unexpected free page"); }
}
static void SET_PAGE_TYPE(struct m32_page_t *page, enum m32_page_type ty) {
  page->type = ty;
}
#else
#define ASSERT_PAGE_ALIGNED(L, page)
#define ASSERT_VALID_PAGE(L, page)
#define ASSERT_PAGE_NOT_FREE(L, page)
#define ASSERT_PAGE_TYPE(L, page, ty)
#define SET_PAGE_TYPE(page, ty)
#endif

/* Accessors */
static void
m32_filled_page_set_next(ll_linker *L, struct m32_page_t *page, struct m32_page_t *next)
{
  ASSERT_PAGE_TYPE(L, page, FILLED_PAGE);
  if (next != NULL && ! is_okay_address(L, next)) {
    LL_FATAL_FOR(L, "m32_filled_page_set_next: Page %p not within 4GB of program text", next);
  }
  page->filled_page.next = next;
}

static struct m32_page_t *
m32_filled_page_get_next(ll_linker *L LL_UNUSED, struct m32_page_t *page)
{
  ASSERT_PAGE_TYPE(L, page, FILLED_PAGE);
  return (struct m32_page_t *) (uintptr_t) page->filled_page.next;
}

/**
 * Allocator
 *
 * Currently an allocator is just a set of pages being filled. The maximum
 * number of pages can be configured with M32_MAX_PAGES.
 */
struct m32_allocator_t {
   ll_linker *L;
   bool executable;
   // List of pages that have been filled but not yet protected.
   struct m32_page_t *unprotected_list;
   // List of pages that have been filled and protected.
   struct m32_page_t *protected_list;
   // Pages in the small-allocation nursery
   struct m32_page_t *pages[M32_MAX_PAGES];
   /* A failed flush can leave some pages RX and others RW. Never retry it. */
   bool protection_failed;
};

/* NULL keeps its original operation-specific behavior; only nonnull retained
 * allocators can be checked for an owner mismatch. */
static void
m32_check_owner(ll_linker *L, const m32_allocator *alloc)
{
  if (alloc != NULL) {
    LL_CHECK_FOR(L, alloc->L == L);
  }
}

/**
 * Per-linker free page pool
 *
 * We keep a small pool of free pages around to avoid fragmentation.
 */
/**
 * Free a filled page or, if possible, place it in the free page pool.
 */
static bool
m32_release_page(ll_linker *L, struct m32_page_t *page)
{
  // Some sanity-checking
  ASSERT_VALID_PAGE(L, page);
  ASSERT_PAGE_NOT_FREE(L, page);

  const size_t pgsz = ll_page_size_for(L);
  ssize_t sz = page->filled_page.size;

  // Break the page, which may be a large multi-page allocation, into
  // individual pages for the page pool
  while (sz > 0) {
    if (L->m32_free_page_pool_size < M32_MAX_FREE_PAGE_POOL_SIZE) {
      if (!ll_mprotect_for_linker(L, page, pgsz, MEM_READ_WRITE)) {
        /* No writable permission was established. Do not touch the page's
         * freelist fields; release this and the remaining pages instead. */
        m32_unmap(L, page, ROUND_UP(sz, pgsz), "m32_release_page");
        return false;
      }
      LL_IF_DEBUG_FOR(L, sanity, memset(page, 0xaa, pgsz));
      SET_PAGE_TYPE(page, FREE_PAGE);
      page->free_page.next = L->m32_free_page_pool;
      L->m32_free_page_pool = page;
      L->m32_free_page_pool_size ++;
    } else {
      break;
    }
    page = (struct m32_page_t *) ((uint8_t *) page + pgsz);
    sz -= pgsz;
  }

  // The free page pool is full, release the rest back to the system
  if (sz > 0) {
    m32_unmap(L, (void *) page, ROUND_UP(sz, pgsz), "m32_release_page");
  }
  return true;
}

/**
 * Allocate a page from the free page pool or operating system. No guarantee is
 * made regarding the state of the m32_page_t fields.
 */
static struct m32_page_t *
m32_alloc_page(ll_linker *L)
{
  if (L->m32_free_page_pool_size == 0) {
    /*
     * Free page pool is empty; refill it with a new batch of M32_MAP_PAGES
     * pages.
     */
    const size_t pgsz = ll_page_size_for(L);
    const size_t map_sz = pgsz * M32_MAP_PAGES;
    uint8_t *chunk = m32_map(L, map_sz);
    if (chunk == NULL) {
#if !defined(mingw32_HOST_OS)
      ll_error_set(L, LL_E_NOMEM, "m32_alloc_page: failed to map pages");
#endif
      /* The Windows seam always supplies this attempt's status/diagnostic. */
      return NULL;
    }
    if (! is_okay_address(L, chunk + map_sz)) {
      ll_error_set(L, LL_E_NOMEM,
                   "m32_alloc_page: failed to allocate pages within 4GB of program text (got %p)",
                   (void *)chunk);
      m32_unmap(L, chunk, map_sz, "m32_alloc_page");
      ll_report_mem_failure_for(L);
      LL_LOG_ERROR_FOR(L, "m32_alloc_page: failed to allocate pages within 4GB of program text (got %p)", (void *)chunk);
      return NULL;
    }
    LL_IF_DEBUG_FOR(L, sanity, memset(chunk, 0xaa, map_sz));

#define GET_PAGE(i) ((struct m32_page_t *) (chunk + (i) * pgsz))
    for (int i=0; i < M32_MAP_PAGES; i++) {
      struct m32_page_t *page = GET_PAGE(i);
      SET_PAGE_TYPE(page, FREE_PAGE);
      page->free_page.next = GET_PAGE(i+1);
    }

    GET_PAGE(M32_MAP_PAGES-1)->free_page.next = L->m32_free_page_pool;
    L->m32_free_page_pool = (struct m32_page_t *) chunk;
    L->m32_free_page_pool_size += M32_MAP_PAGES;
#undef GET_PAGE
  }

  struct m32_page_t *page = L->m32_free_page_pool;
  L->m32_free_page_pool = page->free_page.next;
  L->m32_free_page_pool_size --;
  ASSERT_PAGE_TYPE(L, page, FREE_PAGE);
  return page;
}

#if defined(mingw32_HOST_OS)
bool ll_m32_shutdown_for(ll_linker *L)
{
    bool success = true;
    const size_t page_size = ll_page_size_for(L);
    while (L->m32_free_page_pool != NULL) {
        struct m32_page_t *page = L->m32_free_page_pool;
        /* Read linkage before decommit makes the page inaccessible. */
        L->m32_free_page_pool = page->free_page.next;
        L->m32_free_page_pool_size--;
        if (!ll_m32_unmap_for(L, page, page_size)) success = false;
    }
    /* Do not release reservations still owned by ObjectCode allocators. */
    if (!ll_m32_release_idle_for(L, true)) success = false;
    return success;
}
#endif

/**
 * Initialize the allocator structure
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
m32_allocator *
m32_allocator_new_for(ll_linker *L, bool executable)
{
  m32_allocator *alloc =
    LL_MALLOC_FOR(L, sizeof(m32_allocator), "m32_new_allocator");
  memset(alloc, 0, sizeof(struct m32_allocator_t));
  alloc->L = L;
  alloc->executable = executable;
  return alloc;
}

/**
 * Unmap all pages on the given list.
 */
static void
m32_allocator_unmap_list(ll_linker *L, struct m32_page_t *head)
{
  while (head != NULL) {
    ASSERT_VALID_PAGE(L, head);
    struct m32_page_t *next = m32_filled_page_get_next(L, head);
    m32_release_page(L, head);
    head = next;
  }
}

/**
 * Free an m32_allocator and the pages that it has allocated.
 */
void m32_allocator_free_for(ll_linker *L, m32_allocator *alloc)
{
  m32_check_owner(L, alloc);
  /* Cleanup must not replace the load failure that caused it. Protection
   * failures still reach the logger through the mapping helper. */
  const ll_status saved_status = L->error_status;
  char saved_message[LL_ERROR_CAPACITY];
  if (saved_status != LL_OK) {
    memcpy(saved_message, L->error_message, sizeof(saved_message));
  }
  /* free filled pages */
  m32_allocator_unmap_list(L, alloc->unprotected_list);
  m32_allocator_unmap_list(L, alloc->protected_list);

  /* free partially-filled pages */
  for (int i=0; i < M32_MAX_PAGES; i++) {
    if (alloc->pages[i]) {
      m32_release_page(L, alloc->pages[i]);
    }
  }

  LL_FREE_FOR(L, alloc);
  if (saved_status != LL_OK) {
    L->error_status = saved_status;
    memcpy(L->error_message, saved_message, sizeof(saved_message));
  }
}

/**
 * Push a page onto the given filled page list.
 */
static void
m32_allocator_push_filled_list(ll_linker *L, struct m32_page_t **head, struct m32_page_t *page)
{
  ASSERT_PAGE_TYPE(L, page, FILLED_PAGE);
    // N.B. it's the caller's responsibility to set the pagetype to FILLED_PAGE
  m32_filled_page_set_next(L, page, *head);
  *head = page;
}

/**
 * Release the allocator's reference to pages on the "filling" list. This
 * should be called when it is believed that no more allocations will be needed
 * from the allocator to ensure that empty pages waiting to be filled aren't
 * unnecessarily held.
 *
 * If this allocator is for executable allocations this is where we mark the
 * filled pages as executable (and no longer writable).
 *
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
bool
m32_allocator_flush_for(ll_linker *L, m32_allocator *alloc) {
   m32_check_owner(L, alloc);
   if (alloc->protection_failed) {
     ll_error_set(L, LL_E_STATE, "m32 allocator requires disposal after a protection failure");
     return false;
   }
   for (int i=0; i<M32_MAX_PAGES; i++) {
     if (alloc->pages[i] == NULL) {
       continue;
     } else if (alloc->pages[i]->current_size == sizeof(struct m32_page_t)) {
       // the page is empty, release our reference even on an unmap fallback
       struct m32_page_t *page = alloc->pages[i];
       alloc->pages[i] = NULL;
       if (!m32_release_page(L, page)) {
         alloc->protection_failed = true;
         return false;
       }
     } else {
       // the page contains data, move it to the unprotected list
       SET_PAGE_TYPE(alloc->pages[i], FILLED_PAGE);
       m32_allocator_push_filled_list(L, &alloc->unprotected_list, alloc->pages[i]);
     }
     alloc->pages[i] = NULL;
   }

   // Write-protect pages if this is an executable-page allocator.
   if (alloc->executable) {
     while (alloc->unprotected_list != NULL) {
       struct m32_page_t *page = alloc->unprotected_list;
       ASSERT_PAGE_TYPE(L, page, FILLED_PAGE);
       alloc->unprotected_list = m32_filled_page_get_next(L, page);
       /* Link the page before protection makes its header read-only. Each
        * page remains on exactly one owned list if protection fails. */
       m32_allocator_push_filled_list(L, &alloc->protected_list, page);
       if (!ll_mprotect_for_linker(L, page, page->filled_page.size,
                                   MEM_READ_EXECUTE)) {
         alloc->protection_failed = true;
         return false;
       }
     }
   }
   return true;
}

/**
 * Return true if the allocation request should be considered "large".
 */
static bool
m32_is_large_object(ll_linker *L, size_t size, size_t alignment)
{
   return size >= ll_page_size_for(L) - ROUND_UP(sizeof(struct m32_page_t), alignment);
}

static void
m32_report_allocation(ll_linker *L, struct m32_allocator_t *alloc LL_UNUSED, void *addr LL_UNUSED, size_t size LL_UNUSED)
{
    LL_IF_DEBUG_FOR(L, linker_verbose, LL_DEBUG_FOR(L,
      "m32_allocated(%p:%s): %p - %p\n",
      alloc, alloc->executable ? "RX": "RW",
      addr, (uint8_t*) addr + size));
}

/**
 * Allocate `size` bytes of memory with the given alignment.
 *
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
void *
m32_alloc_for(ll_linker *L, struct m32_allocator_t *alloc, size_t size, size_t alignment)
{
   m32_check_owner(L, alloc);
   if (alloc->protection_failed) {
     ll_error_set(L, LL_E_STATE, "m32 allocator requires disposal after a protection failure");
     return NULL;
   }
   size_t pgsz = ll_page_size_for(L);

   if (m32_is_large_object(L, size,alignment)) {
      // large object
      size_t alsize = ROUND_UP(sizeof(struct m32_page_t), alignment);
      // TODO: lower-bound allocation size to allocation granularity and return
      // remainder to free pool.
      struct m32_page_t *page = m32_map(L, alsize+size);
      if (page == NULL) {
#if !defined(mingw32_HOST_OS)
          ll_error_set(L, LL_E_NOMEM,
                       "m32_alloc: Failed to map pages for %zu bytes", size);
#endif
          LL_LOG_SYSERROR_FOR(L, "m32_alloc: Failed to map pages for %zu bytes", size);
          return NULL;
      } else if (! is_okay_address(L, page)) {
          ll_error_set(L, LL_E_NOMEM,
                       "m32_alloc: Allocation of %zu bytes resulted in pages above 4GB (%p)",
                       size, (void *)page);
          m32_unmap(L, page, alsize + size, "m32_alloc");
          ll_report_mem_failure_for(L);
          LL_LOG_ERROR_FOR(L, "m32_alloc: Allocation of %zu bytes resulted in pages above 4GB (%p)",
               size, (void *)page);
          return NULL;
      }
      SET_PAGE_TYPE(page, FILLED_PAGE);
      page->filled_page.size = alsize + size;
      m32_allocator_push_filled_list(L, &alloc->unprotected_list, (struct m32_page_t *) page);
      uint8_t *res = (uint8_t *) page + alsize;
      m32_report_allocation(L, alloc, res, size);
      return res;
   }

   // small object
   // Try to find a page that can contain it
   int empty = -1;
   int most_filled = -1;
   int i;
   for (i=0; i<M32_MAX_PAGES; i++) {
      // empty page
      if (alloc->pages[i] == NULL) {
         empty = empty == -1 ? i : empty;
         continue;
      }

      // page can contain the buffer?
      ASSERT_VALID_PAGE(L, alloc->pages[i]);
      ASSERT_PAGE_TYPE(L, alloc->pages[i], NURSERY_PAGE);
      size_t alsize = ROUND_UP(alloc->pages[i]->current_size, alignment);
      if (size <= pgsz - alsize) {
         void * addr = (char*)alloc->pages[i] + alsize;
         alloc->pages[i]->current_size = alsize + size;
         m32_report_allocation(L, alloc, addr, size);
         return addr;
      }

      // is this the most filled page we've seen so far?
      if (most_filled == -1
       || alloc->pages[most_filled]->current_size < alloc->pages[i]->current_size)
      {
         most_filled = i;
      }
   }

   // If we haven't found an empty page, flush the most filled one
   if (empty == -1) {
      SET_PAGE_TYPE(alloc->pages[most_filled], FILLED_PAGE);
      m32_allocator_push_filled_list(L, &alloc->unprotected_list, alloc->pages[most_filled]);
      alloc->pages[most_filled] = NULL;
      empty = most_filled;
   }

   // Allocate a new page
   struct m32_page_t *page = m32_alloc_page(L);
   if (page == NULL) {
      return NULL;
   }
   SET_PAGE_TYPE(page, NURSERY_PAGE);
   alloc->pages[empty]               = page;
   // Add header size and padding
   alloc->pages[empty]->current_size = size + ROUND_UP(sizeof(struct m32_page_t),alignment);
   uint8_t *res = (uint8_t *) page + ROUND_UP(sizeof(struct m32_page_t), alignment);
   m32_report_allocation(L, alloc, res, size);
   return res;
}

#else

// The following implementations of these functions should never be called. If
// they are, there is a bug at the call site.
// See the note titled "Compile Time Trickery" at the top of this file.

m32_allocator *
m32_allocator_new_for(ll_linker *L, bool executable LL_UNUSED)
{
    LL_FATAL_FOR(L, "%s: RTS_LINKER_USE_MMAP is %d", "m32_allocator_new", RTS_LINKER_USE_MMAP);
}

void m32_allocator_free_for(ll_linker *L, m32_allocator *alloc LL_UNUSED)
{
    LL_FATAL_FOR(L, "%s: RTS_LINKER_USE_MMAP is %d", "m32_allocator_free", RTS_LINKER_USE_MMAP);
}

bool
m32_allocator_flush_for(ll_linker *L, m32_allocator *alloc LL_UNUSED)
{
    LL_FATAL_FOR(L, "%s: RTS_LINKER_USE_MMAP is %d", "m32_allocator_flush", RTS_LINKER_USE_MMAP);
}

void *
m32_alloc_for(ll_linker *L, m32_allocator *alloc LL_UNUSED,
          size_t size LL_UNUSED,
          size_t alignment LL_UNUSED)
{
    LL_FATAL_FOR(L, "%s: RTS_LINKER_USE_MMAP is %d", "m32_alloc", RTS_LINKER_USE_MMAP);
}

#endif
