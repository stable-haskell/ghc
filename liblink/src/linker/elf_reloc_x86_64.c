/* x86_64 ELF relocation stores and arithmetic, independent of host format. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"

#if LL_BUILD_ELF && LL_TARGET_X86_64
#include "elf_compat.h"
#include "LinkerInternals.h"
#include "linker/elf_reloc_x86_64.h"
#include "linker/SymbolExtras.h"
#include "liblink/elf.h"
#include <string.h>
#if defined(dragonfly_HOST_OS)
#include <sys/tls.h>
#endif

/* Preserve the existing eligibility policy in Elf.c's
 * Note [X86_64_ELF_NONPIC_HACK and data references]. */
#define X86_64_ELF_NONPIC_HACK(L) (!(L)->services.cfg.always_pic)

static int apply_x86_64(ObjectCode *oc, Section *section, uint32_t type,
                        ll_site site, uintptr_t symbol_address, int64_t addend,
                        ll_sym_type st, const ll_reloc_context *context)
{
    (void)section;
    (void)st;
    /* The original ELF loop used unsigned target-width operands, including
     * its addend. Keep those conversions and wrap semantics at this boundary. */
    ll_elf64_addr P = site.exec;
    ll_elf64_addr S = symbol_address;
    ll_elf64_addr A = addend;
    ll_elf64_addr value = S + A;
    const char *symbol = context->symbol_name;
    /* Extras still have native storage/execution addresses. This first slice
     * separates the relocation site only; NO_EXEC extras mapping comes later. */
    switch (type) {
      case COMPAT_R_X86_64_NONE:
          break;

      case COMPAT_R_X86_64_64:
      {
          ll_elf64_xword payload = value;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }

      case COMPAT_R_X86_64_PC32:
      {
          ll_int64 off = value - P;
          if (off != (ll_elf64_sword)off && X86_64_ELF_NONPIC_HACK(oc->L)) {
              /* Check symbol type: jump islands only work for code (functions).
               * For data references (STT_OBJECT, STT_NOTYPE), the jump island
               * address would be used as a data pointer (e.g., info table pointer),
               * causing GC crashes ("strange closure type").
               * See Note [X86_64_ELF_NONPIC_HACK and data references] */
              unsigned char symtype = context->format_symbol_type;
              if (symtype != LL_ELF_STT_FUNC) {
                  ll_error_set(oc->L, LL_E_RELOC_RANGE,
                               "R_X86_64_PC32 overflow for non-function symbol `%s'",
                               symbol);
                  LL_LOG_ERROR_FOR(oc->L,
                      "R_X86_64_PC32 overflow for non-function symbol `%s' "
                      "(type %d, distance 0x%" PRIx64 ").\n"
                      "    Jump islands only work for code, not data references.\n"
                      "    Recompile %s with -fPIC -fexternal-dynamic-refs.",
                      symbol, symtype, (uint64_t)(value - P), oc->fileName);
                  return 0;
              }
              ll_int64 pltAddress =
                  (ll_int64) &ll_make_symbol_extra_x86_64(oc, context->symbol_index, S)
                                            -> jumpIsland;
              off = pltAddress + A - P;
          }
          if (off != (ll_elf64_sword)off) {
              ll_error_set(oc->L, LL_E_RELOC_RANGE,
                           "R_X86_64_PC32 relocation out of range: %s", symbol);
              LL_LOG_ERROR_FOR(oc->L,
                  "R_X86_64_PC32 relocation out of range: %s = %" PRIx64
                  "\nRecompile %s with -fPIC -fexternal-dynamic-refs.",
                  symbol, off, oc->fileName);
              return 0;
          }
          ll_elf64_sword payload = off;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }

      case COMPAT_R_X86_64_PC64:
      {
          ll_elf64_sxword payload = value - P;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }

      case COMPAT_R_X86_64_32:
      {
          if (value != (ll_elf64_word)value && X86_64_ELF_NONPIC_HACK(oc->L)) {
              /* See Note [X86_64_ELF_NONPIC_HACK and data references] */
              unsigned char symtype = context->format_symbol_type;
              if (symtype != LL_ELF_STT_FUNC) {
                  ll_error_set(oc->L, LL_E_RELOC_RANGE,
                               "R_X86_64_32 overflow for non-function symbol `%s'",
                               symbol);
                  LL_LOG_ERROR_FOR(oc->L,
                      "R_X86_64_32 overflow for non-function symbol `%s' "
                      "(type %d, value 0x%" PRIx64 ").\n"
                      "    Jump islands only work for code, not data references.\n"
                      "    Recompile %s with -fPIC -fexternal-dynamic-refs.",
                      symbol, symtype, (uint64_t)value, oc->fileName);
                  return 0;
              }
              ll_int64 pltAddress =
                  (ll_int64) &ll_make_symbol_extra_x86_64(oc, context->symbol_index, S)
                                            -> jumpIsland;
              value = pltAddress + A;
          }
          if (value != (ll_elf64_word)value) {
              ll_error_set(oc->L, LL_E_RELOC_RANGE,
                           "R_X86_64_32 relocation out of range: %s", symbol);
              LL_LOG_ERROR_FOR(oc->L,
                  "R_X86_64_32 relocation out of range: %s = %" PRIx64
                  "\nRecompile %s with -fPIC -fexternal-dynamic-refs.",
                  symbol, value, oc->fileName);
              return 0;
          }
          ll_elf64_word payload = value;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }

      case COMPAT_R_X86_64_32S:
      {
          if ((ll_int64)value != (ll_elf64_sword)value && X86_64_ELF_NONPIC_HACK(oc->L)) {
              /* See Note [X86_64_ELF_NONPIC_HACK and data references] */
              unsigned char symtype = context->format_symbol_type;
              if (symtype != LL_ELF_STT_FUNC) {
                  ll_error_set(oc->L, LL_E_RELOC_RANGE,
                               "R_X86_64_32S overflow for non-function symbol `%s'",
                               symbol);
                  LL_LOG_ERROR_FOR(oc->L,
                      "R_X86_64_32S overflow for non-function symbol `%s' "
                      "(type %d, value 0x%" PRIx64 ").\n"
                      "    Jump islands only work for code, not data references.\n"
                      "    Recompile %s with -fPIC -fexternal-dynamic-refs.",
                      symbol, symtype, (uint64_t)value, oc->fileName);
                  return 0;
              }
              ll_int64 pltAddress =
                  (ll_int64) &ll_make_symbol_extra_x86_64(oc, context->symbol_index, S)
                                            -> jumpIsland;
              value = pltAddress + A;
          }
          if ((ll_int64)value != (ll_elf64_sword)value) {
              ll_error_set(oc->L, LL_E_RELOC_RANGE,
                           "R_X86_64_32S relocation out of range: %s", symbol);
              LL_LOG_ERROR_FOR(oc->L,
                  "R_X86_64_32S relocation out of range: %s = %" PRIx64
                  "\nRecompile %s with -fPIC -fexternal-dynamic-refs.",
                  symbol, value, oc->fileName);
              return 0;
          }
          ll_elf64_sword payload = value;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }
      case COMPAT_R_X86_64_REX_GOTPCRELX:
      case COMPAT_R_X86_64_GOTPCRELX:
      case COMPAT_R_X86_64_GOTPCREL:
      {
          ll_int64 gotAddress = (ll_int64) &ll_make_symbol_extra_x86_64(oc, context->symbol_index, S)->addr;
          ll_int64 off = gotAddress + A - P;
          if (off != (ll_elf64_sword)off) {
              ll_error_set(oc->L, LL_E_RELOC_RANGE,
                  "COMPAT_R_X86_64_GOTPCREL relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, (uint64_t)off, oc->fileName);
              LL_LOG_ERROR_FOR(oc->L,
                  "COMPAT_R_X86_64_GOTPCREL relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, off, oc->fileName);
              return 0;
          }
          ll_elf64_sword payload = off;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }
      case COMPAT_R_X86_64_TLSGD:
      {
          ll_int64 off = S + A - P;
          if (off != (ll_elf64_sword)off) {
              ll_error_set(oc->L, LL_E_RELOC_RANGE,
                  "COMPAT_R_X86_64_TLSGD relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, (uint64_t)off, oc->fileName);
              LL_LOG_ERROR_FOR(oc->L,
                  "COMPAT_R_X86_64_TLSGD relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, off, oc->fileName);
              return 0;
          }
          ll_elf64_sword payload = off;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }
#if defined(dragonfly_HOST_OS)
      case COMPAT_R_X86_64_GOTTPOFF:
      {
        /* determine the offset of S to the current thread's tls
           area
           XXX: Move this to the beginning of function */
          struct tls_info ti;
          get_tls_area(0, &ti, sizeof(ti));
          /* make entry in GOT that contains said offset */
          ll_int64 gotEntry = (ll_int64) &ll_make_symbol_extra_x86_64(oc, context->symbol_index,
                                         (S - (ll_elf64_addr)(ti.base)))->addr;
          ll_int64 off = gotEntry + A - P;
          if (off != (ll_elf64_sword)off) {
              ll_error_set(oc->L, LL_E_RELOC_RANGE,
                  "COMPAT_R_X86_64_GOTTPOFF relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, (uint64_t)off, oc->fileName);
              LL_LOG_ERROR_FOR(oc->L,
                  "COMPAT_R_X86_64_GOTTPOFF relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, off, oc->fileName);
              return 0;
          }
          Elf64_SWord payload = off;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }
#endif

      case COMPAT_R_X86_64_PLT32:
      {
          ll_int64 off = value - P;
          if (off != (ll_elf64_sword)off) {
              ll_int64 pltAddress = (ll_int64) &ll_make_symbol_extra_x86_64(oc, context->symbol_index, S)
                                                    -> jumpIsland;
              off = pltAddress + A - P;
          }
          if (off != (ll_elf64_sword)off) {
              ll_error_set(oc->L, LL_E_RELOC_RANGE,
                  "R_X86_64_PLT32 relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, (uint64_t)off, oc->fileName);
              LL_LOG_ERROR_FOR(oc->L,
                  "R_X86_64_PLT32 relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, off, oc->fileName);
              return 0;
          }
          ll_elf64_sword payload = off;
          memcpy(site.work, &payload, sizeof(payload));
          break;
      }
    default:
        ll_error_set(oc->L, LL_E_UNSUPPORTED,
                     "%s: unhandled ELF relocation(RelA) type %" LL_FMT_WORD "\n",
                     oc->fileName, (ll_w)type);
        LL_LOG_ERROR_FOR(oc->L,
                     "%s: unhandled ELF relocation(RelA) type %" LL_FMT_WORD "\n",
                     oc->fileName, (ll_w)type);
        return 0;
    }
    return 1;
}

const struct ll_reloc_ops ll_elf_x86_64_reloc_ops = {
    .machine = LL_ELF_EM_X86_64,
    .apply = apply_x86_64,
    .extra_bytes_per_symbol = sizeof(ll_symbol_extra_x86_64)
};
#endif
