/* --------------------------------------------------------------------------
 * Symbol Extras.
 * This is about allocating a small chunk of memory for every symbol in the
 * object file. We make sure that the SymboLExtras are always "in range" of
 * limited-range PC-relative instructions on various platforms by allocating
 * them right next to the object code itself.
 *
 * This implementation is shared by the MachO and ELF implementations. Windows'
 * PEi386 has its own implementation of symbol extras.
 */

#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "LinkerInternals.h"
#include "ll_host.h"
#include "linker/MMap.h"


#include "linker/SymbolExtras.h"
#include "linker/M32Alloc.h"

#include <string.h>
#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif /* RTS_LINKER_USE_MMAP */

/*
  ocAllocateExtras

  Allocate additional space at the end of the object file image to make room
  for jump islands (powerpc, x86_64, arm), GOT entries (x86_64) and
  bss sections.

  PowerPC relative branch instructions have a 24 bit displacement field.
  As PPC code is always 4-byte-aligned, this yields a +-32MB range.
  If a particular imported symbol is outside this range, we have to redirect
  the jump to a short piece of new code that just loads the 32bit absolute
  address and jumps there.
  On x86_64, PC-relative jumps and PC-relative accesses to the GOT are limited
  to 32 bits (+-2GB).

  This function just allocates space for one SymbolExtra for every
  undefined symbol in the object file. The code for the jump islands is
  filled in by makeSymbolExtra below.
*/

int ocAllocateExtras(ObjectCode *oc, int count, int first, int bssSize,
                     size_t extra_stride, ll_extra_refresh_fn refresh)
{
  /* Record the storage path before any allocation or refresh can fail. PE's
   * independently allocated M32 extras never enter this shared path. */
  oc->symbol_extras_origin = LL_EXTRAS_SHARED;
  const uintptr_t old_image_address = (uintptr_t)oc->image;
  const size_t extras_size = extra_stride * count;

  if (count > 0 || bssSize > 0) {
    if (!RTS_LINKER_USE_MMAP) {
      /* N.B. We currently can't mark symbol extras as non-executable in this
       * case. */

      // round up to the nearest 4
      int aligned = (oc->fileSize + 3) & ~3;
      int misalignment = oc->misalignment;

      oc->image -= misalignment;
      oc->image = LL_REALLOC_FOR(oc->L, oc->image,
                               misalignment +
                               aligned + extras_size,
                               "ocAllocateExtras" );
      oc->image += misalignment;

      oc->symbol_extras = (SymbolExtra *) (oc->image + aligned);
    } else if (USE_CONTIGUOUS_MMAP || oc->L->services.cfg.always_pic) {
      /* Keep image, bssExtras and symbol_extras contiguous */
      /* N.B. We currently can't mark symbol extras as non-executable in this
       * case. */
      size_t n = ll_round_up_to_page_for(oc->L, oc->fileSize);
      // round bssSize up to the nearest page size since we need to ensure that
      // symbol_extras is aligned to a page boundary so it can be mprotect'd.
      bssSize = ll_round_up_to_page_for(oc->L, bssSize);
      size_t allocated_size = n + bssSize + extras_size;
      void *new = ll_mmap_anon_for_linker(oc->L, allocated_size);
      if (new) {
          memcpy(new, oc->image, oc->fileSize);
          if (oc->imageMapped) {
              ll_munmap_for_linker(oc->L, oc->image, n, "ocAllocateExtras");
          } else {
              LL_FREE_FOR(oc->L, oc->image - oc->misalignment);
          }
          oc->image = new;
          oc->imageMapped = true;
          oc->fileSize = allocated_size;
          oc->symbol_extras = (SymbolExtra *) (oc->image + n + bssSize);
          oc->bssBegin = oc->image + n;
          oc->bssEnd = oc->image + n + bssSize;
      }
      else {
          oc->symbol_extras = NULL;
          return 0;
      }
    } else {
        /* m32_allocator_flush ensures that these are marked as executable when
         * we finish building them. */
        oc->symbol_extras = m32_alloc_for(oc->L, oc->rx_m32, extras_size, 8);
        if (oc->symbol_extras == NULL) return 0;
    }
  }

  if (oc->symbol_extras != NULL) {
      memset( oc->symbol_extras, 0, extras_size );
  }

  // ObjectCodeFormatInfo contains computed addresses based on offset to
  // image, if the address of image changes, we need to invalidate
  // the ObjectCodeFormatInfo and recompute it.
  if ((uintptr_t)oc->image != old_image_address && refresh != NULL) {
    if (!refresh(oc)) return 0;
  }

  oc->symbol_extra_stride = extra_stride;
  oc->symbol_extras_exec = (uintptr_t)oc->symbol_extras;
  oc->first_symbol_extra = first;
  oc->n_symbol_extras = count;

  return 1;
}

/**
 * Mark the symbol extras as non-writable.
 */
bool ocProtectExtras(ObjectCode* oc)
{
  if (oc->symbol_extras_origin != LL_EXTRAS_SHARED) return true;
  if (oc->n_symbol_extras == 0) return true;

  if (!RTS_LINKER_USE_MMAP) {
    /*
     * In this case the symbol extras were allocated via malloc. All we can do
     * in this case is hope that the platform doesn't mark such allocations as
     * non-executable.
     */
  } else if (USE_CONTIGUOUS_MMAP || oc->L->services.cfg.always_pic) {
    return ll_mprotect_for_linker(oc->L, oc->symbol_extras,
                                 oc->symbol_extra_stride * oc->n_symbol_extras,
                                 MEM_READ_EXECUTE);
  } else {
    /*
     * The symbol extras were allocated via m32. They will be protected when
     * the m32 allocator is finalized
     */
  }
  return true;
}


/* The allocation records its target stride. The native compatibility pointer
 * is only an address here: indexing it as SymbolExtra[] would use host size. */
static void *symbolExtraSlot(ObjectCode const *oc, unsigned long symbolNumber)
{
    LL_ASSERT_FOR(oc->L, symbolNumber >= oc->first_symbol_extra
            && symbolNumber - oc->first_symbol_extra < oc->n_symbol_extras);
    return (unsigned char *)(void *)oc->symbol_extras +
        (symbolNumber - oc->first_symbol_extra) * oc->symbol_extra_stride;
}

ll_symbol_extra_x86_64 *ll_make_symbol_extra_x86_64(
    ObjectCode const *oc, unsigned long symbolNumber, uint64_t target)
{
    ll_symbol_extra_x86_64 *extra = symbolExtraSlot(oc, symbolNumber);
    // jmp *-14(%rip)
    // 0xFF 25 is opcode + ModRM of near absolute indirect jump
    // Two bytes trailing padding, needed for TLSGD GOT entries
    // See Note [TLSGD relocation] in elf_tlsgd.c
    static uint8_t jmp[] = { 0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF, 0x00, 0x00 };
    extra->addr = target;
    memcpy(extra->jumpIsland, jmp, 8);
    return extra;
}

ll_site ll_make_symbol_extra_x86_64_site(
    ObjectCode const *oc, unsigned long symbolNumber, uint64_t target)
{
    ll_symbol_extra_x86_64 *extra =
        ll_make_symbol_extra_x86_64(oc, symbolNumber, target);
    const uintptr_t offset = (symbolNumber - oc->first_symbol_extra) *
                             oc->symbol_extra_stride;
    return (ll_site){.work = (uint8_t *)(void *)extra,
                     .exec = oc->symbol_extras_exec + offset};
}

ll_symbol_extra_riscv64 *ll_make_symbol_extra_riscv64(
    ObjectCode const *oc, unsigned long symbolNumber, uint64_t target)
{
    ll_symbol_extra_riscv64 *extra = symbolExtraSlot(oc, symbolNumber);
    // Fake GOT entry (used like GOT, but located in symbol extras)
    extra->addr = target;
    return extra;
}

ll_symbol_extra_powerpc *ll_make_symbol_extra_powerpc(
    ObjectCode const *oc, unsigned long symbolNumber, uint32_t target)
{
    ll_symbol_extra_powerpc *extra = symbolExtraSlot(oc, symbolNumber);
    /* Preserve native field stores. Foreign instruction byte order is a
     * separate target execution/relocation policy, not supplied here. */
    // lis r12, hi16(target)
    extra->jumpIsland.lis_r12     = 0x3d80;
    extra->jumpIsland.hi_addr     = target >> 16;

    // ori r12, r12, lo16(target)
    extra->jumpIsland.ori_r12_r12 = 0x618c;
    extra->jumpIsland.lo_addr     = target & 0xffff;

    // mtctr r12
    extra->jumpIsland.mtctr_r12   = 0x7d8903a6;

    // bctr
    extra->jumpIsland.bctr        = 0x4e800420;
    return extra;
}

ll_symbol_extra_arm *ll_symbol_extra_arm_slot(
    ObjectCode const *oc, unsigned long symbolNumber)
{
    /* ARM instructions are emitted by the existing section-PLT implementation.
     * There is no current standalone makeArmSymbolExtra implementation to move. */
    return symbolExtraSlot(oc, symbolNumber);
}

/* Retain the old native entry point for existing private clients. Backends use
 * the explicit target functions above, including on a different compiler host. */
#if defined(NEED_SYMBOL_EXTRAS)
#if !defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS)
#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(riscv64_HOST_ARCH)
SymbolExtra *makeSymbolExtra(ObjectCode const *oc,
                             unsigned long symbolNumber,
                             unsigned long target)
{
#if defined(powerpc_HOST_ARCH)
    return ll_make_symbol_extra_powerpc(oc, symbolNumber, target);
#elif defined(x86_64_HOST_ARCH)
    return ll_make_symbol_extra_x86_64(oc, symbolNumber, target);
#else
    return ll_make_symbol_extra_riscv64(oc, symbolNumber, target);
#endif
}
#endif /* powerpc_HOST_ARCH || x86_64_HOST_ARCH || riscv64_HOST_ARCH */
#endif /* !x86_64_HOST_ARCH || !mingw32_HOST_OS */
#endif /* NEED_SYMBOL_EXTRAS */
