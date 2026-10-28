#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "elf_reloc_riscv64.h"
#include "LinkerInternals.h"
#include "SymbolExtras.h"
#include "linker/ElfTypes.h"
#include "elf_plt.h"
#include "elf_util.h"
#include "util.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if LL_TARGET_RISCV64

#if LL_BUILD_ELF

#include <stdatomic.h>

typedef uint64_t addr_t;

/* regular instructions are 32bit */
typedef uint32_t inst_t;

/* compressed instructions are 16bit */
typedef uint16_t cinst_t;

// TODO: These instances could be static. They are not yet, because we might
// need their debugging symbols.
char *relocationTypeToString(ll_elf64_xword type);
bool decodeAddendRISCV64(ll_linker *L, Section *section, ll_elf_target_rel *rel,
                         int32_t *addend);
bool encodeAddendRISCV64(ll_linker *L, Section *section, ll_elf_target_rel *rel, int32_t addend);
void write8le(uint8_t *p, uint8_t v);
uint8_t read8le(const uint8_t *P);
void write16le(cinst_t *p, uint16_t v);
uint16_t read16le(const cinst_t *P);
uint32_t read32le(const inst_t *P);
void write32le(inst_t *p, uint32_t v);
uint64_t read64le(const uint64_t *P);
void write64le(uint64_t *p, uint64_t v);
uint32_t extractBits(uint64_t v, uint32_t begin, uint32_t end);
bool setCJType(ll_linker *L, cinst_t *loc, uint32_t val);
bool setCBType(ll_linker *L, cinst_t *loc, uint32_t val);
bool setBType(ll_linker *L, inst_t *loc, uint32_t val);
void setSType(inst_t *loc, uint32_t val);
bool setJType(ll_linker *L, inst_t *loc, uint32_t val);
void setIType(ll_linker *L, inst_t *loc, int32_t val);
bool checkInt(ll_linker *L, const void *loc, int32_t v, int n);
bool setUType(ll_linker *L, inst_t *loc, int32_t val);


char *relocationTypeToString(ll_elf64_xword type) {
  switch (LL_ELF64_R_TYPE(type)) {
  case LL_ELF_R_RISCV_NONE:
    return "R_RISCV_NONE";
  case LL_ELF_R_RISCV_32:
    return "R_RISCV_32";
  case LL_ELF_R_RISCV_64:
    return "R_RISCV_64";
  case LL_ELF_R_RISCV_RELATIVE:
    return "R_RISCV_RELATIVE";
  case LL_ELF_R_RISCV_COPY:
    return "R_RISCV_COPY";
  case LL_ELF_R_RISCV_JUMP_SLOT:
    return "R_RISCV_JUMP_SLOT";
  case LL_ELF_R_RISCV_TLS_DTPMOD32:
    return "R_RISCV_TLS_DTPMOD32";
  case LL_ELF_R_RISCV_TLS_DTPMOD64:
    return "R_RISCV_TLS_DTPMOD64";
  case LL_ELF_R_RISCV_TLS_DTPREL32:
    return "R_RISCV_TLS_DTPREL32";
  case LL_ELF_R_RISCV_TLS_DTPREL64:
    return "R_RISCV_TLS_DTPREL64";
  case LL_ELF_R_RISCV_TLS_TPREL32:
    return "R_RISCV_TLS_TPREL32";
  case LL_ELF_R_RISCV_TLS_TPREL64:
    return "R_RISCV_TLS_TPREL64";
  case LL_ELF_R_RISCV_BRANCH:
    return "R_RISCV_BRANCH";
  case LL_ELF_R_RISCV_JAL:
    return "R_RISCV_JAL";
  case LL_ELF_R_RISCV_CALL:
    return "R_RISCV_CALL";
  case LL_ELF_R_RISCV_CALL_PLT:
    return "R_RISCV_CALL_PLT";
  case LL_ELF_R_RISCV_GOT_HI20:
    return "R_RISCV_GOT_HI20";
  case LL_ELF_R_RISCV_PCREL_HI20:
    return "R_RISCV_PCREL_HI20";
  case LL_ELF_R_RISCV_LO12_I:
    return "R_RISCV_LO12_I";
  case LL_ELF_R_RISCV_PCREL_LO12_I:
    return "R_RISCV_PCREL_LO12_I";
  case LL_ELF_R_RISCV_HI20:
    return "R_RISCV_HI20";
  case LL_ELF_R_RISCV_LO12_S:
    return "R_RISCV_LO12_S";
  case LL_ELF_R_RISCV_PCREL_LO12_S:
    return "R_RISCV_PCREL_LO12_S";
  case LL_ELF_R_RISCV_RELAX:
    return "R_RISCV_RELAX";
  case LL_ELF_R_RISCV_RVC_BRANCH:
    return "R_RISCV_RVC_BRANCH";
  case LL_ELF_R_RISCV_RVC_JUMP:
    return "R_RISCV_RVC_JUMP";
  default:
    return "Unknown relocation type";
  }
}

bool decodeAddendRISCV64(ll_linker *L, Section *section LL_UNUSED,
                         ll_elf_target_rel *rel LL_UNUSED, int32_t *addend LL_UNUSED) {
  ll_error_set(L, LL_E_UNSUPPORTED,
               "decodeAddendRISCV64: Relocations without an explicit addend are not supported.");
  return false;
}

// Make sure that V can be represented as an N bit signed integer.
bool checkInt(ll_linker *L, const void *loc, int32_t v, int n) {
  if (!isInt(n, v)) {
    /* Compressed relocations may have only two readable bytes at loc. */
    ll_error_set(L, LL_E_RELOC_RANGE,
                 "Relocation at %p is out of range. value: 0x%x (%d), "
                 "max bits 0x%x (%d)", (void *)loc, (unsigned)v, v,
                 (unsigned)n, n);
    return false;
  }
  return true;
}

// RISCV helpers: use memcpy for alignment-safe access.
uint8_t read8le(const uint8_t *p) { return *p; }
void write8le(uint8_t *p, uint8_t v) { *p = v; }

void write16le(cinst_t *p, uint16_t v) { memcpy(p, &v, sizeof(v)); }
uint16_t read16le(const cinst_t *p) { uint16_t v; memcpy(&v, p, sizeof(v)); return v; }

uint32_t read32le(const inst_t *p) { uint32_t v; memcpy(&v, p, sizeof(v)); return v; }
void write32le(inst_t *p, uint32_t v) { memcpy(p, &v, sizeof(v)); }

uint64_t read64le(const uint64_t *p) { uint64_t v; memcpy(&v, p, sizeof(v)); return v; }
void write64le(uint64_t *p, uint64_t v) { memcpy(p, &v, sizeof(v)); }

uint32_t extractBits(uint64_t v, uint32_t begin, uint32_t end) {
  return (v & ((1ULL << (begin + 1)) - 1)) >> end;
}

// Set immediate val in the instruction at *loc. In U-type instructions the
// upper 20bits carry the upper 20bits of the immediate.
bool setUType(ll_linker *L, inst_t *loc, int32_t val) {
  const unsigned bits = 32;
  uint32_t hi = (uint32_t)val + UINT32_C(0x800);
  if (!checkInt(L, loc, signExtend32(bits, hi) >> 12, 20)) return false;
  LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "setUType: hi 0x%x val 0x%x\n", hi, val));

  uint32_t imm = hi & 0xFFFFF000;
  write32le(loc, (read32le(loc) & 0xFFF) | imm);
  return true;
}

// Set immediate val in the instruction at *loc. In I-type instructions the
// upper 12bits carry the lower 12bit of the immediate.
void setIType(ll_linker *L, inst_t *loc, int32_t val) {
  uint64_t hi = (val + 0x800) >> 12;
  uint64_t lo = val - (hi << 12);

  LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "setIType: hi 0x%lx lo 0x%lx\n", hi, lo));
  LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "setIType: loc %p  *loc 0x%x  val 0x%x\n", loc,
                              *loc, val));

  uint32_t imm = lo & 0xfff;
  uint32_t instr = (read32le(loc) & 0xfffff) | (imm << 20);

  LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "setIType: insn 0x%x\n", instr));
  write32le(loc, instr);
  LL_IF_DEBUG_FOR(L, linker, LL_DEBUG_FOR(L, "setIType: loc %p  *loc' 0x%x  val 0x%x\n", loc,
                              *loc, val));
}

// Set immediate val in the instruction at *loc. In S-type instructions the
// lower 12 bits of the immediate are at bits 7 to 11 ([0:4]) and 25 to 31
// ([5:11]).
void setSType(inst_t *loc, uint32_t val) {
  uint64_t hi = (val + 0x800) >> 12;
  uint64_t lo = val - (hi << 12);

  uint32_t imm = lo;
  uint32_t instr = (read32le(loc) & 0x1fff07f) | (extractBits(imm, 11, 5) << 25) |
         (extractBits(imm, 4, 0) << 7);

  write32le(loc, instr);
}

// Set immediate val in the instruction at *loc. In J-type instructions the
// immediate has 20bits which are pretty scattered:
// instr bit -> imm bit
// 31 -> 20
// [30:21] -> [10:1]
// 20 -> 11
// [19:12] -> [19:12]
//
// N.B. bit 0 of the immediate is missing!
bool setJType(ll_linker *L, inst_t *loc, uint32_t val) {
  if (!checkInt(L, loc, val, 21)) return false;

  uint32_t insn = read32le(loc) & 0xFFF;
  uint32_t imm20 = extractBits(val, 20, 20) << 31;
  uint32_t imm10_1 = extractBits(val, 10, 1) << 21;
  uint32_t imm11 = extractBits(val, 11, 11) << 20;
  uint32_t imm19_12 = extractBits(val, 19, 12) << 12;
  insn |= imm20 | imm10_1 | imm11 | imm19_12;

  write32le(loc, insn);
  return true;
}

// Set immediate val in the instruction at *loc. In B-type instructions the
// immediate has 12bits which are pretty scattered:
// instr bit -> imm bit
// 31 -> 12
// [30:25] -> [10:5]
// [11:8] -> [4:1]
// 7 -> 11
//
// N.B. bit 0 of the immediate is missing!
bool setBType(ll_linker *L, inst_t *loc, uint32_t val) {
  if (!checkInt(L, loc, val, 13)) return false;

  uint32_t insn = read32le(loc) & 0x1FFF07F;
  uint32_t imm12 = extractBits(val, 12, 12) << 31;
  uint32_t imm10_5 = extractBits(val, 10, 5) << 25;
  uint32_t imm4_1 = extractBits(val, 4, 1) << 8;
  uint32_t imm11 = extractBits(val, 11, 11) << 7;
  insn |= imm12 | imm10_5 | imm4_1 | imm11;

  write32le(loc, insn);
  return true;
}


// Set immediate val in the instruction at *loc. CB-type instructions have a
// lenght of 16 bits (half-word, compared to the usual 32bit/word instructions.)
// The immediate has 8bits which are pretty scattered:
// instr bit -> imm bit
// 12 -> 8
// [11:10] -> [4:3]
// [6:5] -> [7:6]
// [4:3] -> [2:1]
// 2 -> 5
//
// N.B. bit 0 of the immediate is missing!
bool setCBType(ll_linker *L, cinst_t *loc, uint32_t val) {
  if (!checkInt(L, loc, val, 9)) return false;
  uint16_t insn = read16le(loc) & 0xE383;
  uint16_t imm8 = extractBits(val, 8, 8) << 12;
  uint16_t imm4_3 = extractBits(val, 4, 3) << 10;
  uint16_t imm7_6 = extractBits(val, 7, 6) << 5;
  uint16_t imm2_1 = extractBits(val, 2, 1) << 3;
  uint16_t imm5 = extractBits(val, 5, 5) << 2;
  insn |= imm8 | imm4_3 | imm7_6 | imm2_1 | imm5;

  write16le(loc, insn);
  return true;
}

// Set immediate val in the instruction at *loc. CJ-type instructions have a
// lenght of 16 bits (half-word, compared to the usual 32bit/word instructions.)
// The immediate has 11bits which are pretty scattered:
// instr bit -> imm bit
// 12 -> 11
// 11 -> 4
// [10:9] ->[9:8]
// 8 -> 10
// 7 -> 6
// 6 -> 7
// [5:3] -> [3:1]
// 2 -> 5
//
// N.B. bit 0 of the immediate is missing!
bool setCJType(ll_linker *L, cinst_t *loc, uint32_t val) {
  if (!checkInt(L, loc, val, 12)) return false;
  uint16_t insn = read16le(loc) & 0xE003;
  uint16_t imm11 = extractBits(val, 11, 11) << 12;
  uint16_t imm4 = extractBits(val, 4, 4) << 11;
  uint16_t imm9_8 = extractBits(val, 9, 8) << 9;
  uint16_t imm10 = extractBits(val, 10, 10) << 8;
  uint16_t imm6 = extractBits(val, 6, 6) << 7;
  uint16_t imm7 = extractBits(val, 7, 7) << 6;
  uint16_t imm3_1 = extractBits(val, 3, 1) << 3;
  uint16_t imm5 = extractBits(val, 5, 5) << 2;
  insn |= imm11 | imm4 | imm9_8 | imm10 | imm6 | imm7 | imm3_1 | imm5;

  write16le(loc, insn);
  return true;
}

// Encode the addend according to the relocaction into the instruction.
static bool encodeAddendRISCV64Site(ll_linker *L, ll_site site,
                                    ll_elf_target_rel *rel, int64_t addend) {
  // Encoding writes through work; address arithmetic is performed by the caller.
  addr_t P = (addr_t)site.work;
  LL_IF_DEBUG_FOR(L, linker,
           LL_DEBUG_FOR(L,
               "Relocation type %s 0x%lx (%lu) symbol 0x%lx addend 0x%llx "
               "P 0x%lx\n",
               relocationTypeToString(rel->r_info), LL_ELF64_R_TYPE(rel->r_info),
               LL_ELF64_R_TYPE(rel->r_info), LL_ELF64_R_SYM(rel->r_info),
               (unsigned long long)addend, P));
  switch (LL_ELF64_R_TYPE(rel->r_info)) {
  case LL_ELF_R_RISCV_32_PCREL:
  case LL_ELF_R_RISCV_32:
    write32le((inst_t *)P, addend);
    break;
  case LL_ELF_R_RISCV_64:
    write64le((uint64_t *)P, addend);
    break;
  case LL_ELF_R_RISCV_GOT_HI20:
  case LL_ELF_R_RISCV_PCREL_HI20:
  case LL_ELF_R_RISCV_HI20: {
    if (!setUType(L, (inst_t *)P, addend)) return EXIT_FAILURE;
    break;
  }
  case LL_ELF_R_RISCV_PCREL_LO12_I:
  case LL_ELF_R_RISCV_LO12_I: {
    setIType(L, (inst_t *)P, addend);
    break;
  }
  case LL_ELF_R_RISCV_RVC_JUMP: {
    if (!setCJType(L, (cinst_t *)P, addend)) return EXIT_FAILURE;
    break;
  }
  case LL_ELF_R_RISCV_RVC_BRANCH: {
    if (!setCBType(L, (cinst_t *)P, addend)) return EXIT_FAILURE;
    break;
  }
  case LL_ELF_R_RISCV_BRANCH: {
    if (!setBType(L, (inst_t *)P, addend)) return EXIT_FAILURE;
    break;
  }
  case LL_ELF_R_RISCV_CALL:
  case LL_ELF_R_RISCV_CALL_PLT: {
    // We could relax more (in some cases) but right now most important is to
    // make it work.
    if (!setUType(L, (inst_t *)P, addend)) return EXIT_FAILURE;
    setIType(L, ((inst_t *)P) + 1, addend);
    break;
  }
  case LL_ELF_R_RISCV_JAL: {
    if (!setJType(L, (inst_t *)P, addend)) return EXIT_FAILURE;
    break;
  }
  case LL_ELF_R_RISCV_ADD8:
    write8le((uint8_t *)P, read8le((uint8_t *)P) + addend);
    break;
  case LL_ELF_R_RISCV_ADD16:
    write16le((cinst_t *)P, read16le((cinst_t *)P) + addend);
    break;
  case LL_ELF_R_RISCV_ADD32:
    write32le((inst_t *)P, read32le((inst_t *)P) + addend);
    break;
  case LL_ELF_R_RISCV_ADD64:
    write64le((uint64_t *)P, read64le((uint64_t *)P) + addend);
    break;
  case LL_ELF_R_RISCV_SUB6: {
    uint8_t keep = *((uint8_t *)P) & 0xc0;
    uint8_t imm = (((*(uint8_t *)P) & 0x3f) - addend) & 0x3f;

    write8le((uint8_t *)P, keep | imm);
    break;
  }
  case LL_ELF_R_RISCV_SUB8:
    write8le((uint8_t *)P, read8le((uint8_t *)P) - addend);
    break;
  case LL_ELF_R_RISCV_SUB16:
    write16le((cinst_t *)P, read16le((cinst_t *)P) - addend);
    break;
  case LL_ELF_R_RISCV_SUB32:
    write32le((inst_t *)P, read32le((inst_t *)P) - addend);
    break;
  case LL_ELF_R_RISCV_SUB64:
    write64le((uint64_t *)P, read64le((uint64_t *)P) - addend);
    break;
  case LL_ELF_R_RISCV_SET6: {
    uint8_t keep = *((uint8_t *)P) & 0xc0;
    uint8_t imm = (addend & 0x3f) & 0x3f;

    write8le((uint8_t *)P, keep | imm);
    break;
  }
  case LL_ELF_R_RISCV_SET8:
    write8le((uint8_t *)P, addend);
    break;
  case LL_ELF_R_RISCV_SET16:
    write16le((cinst_t *)P, addend);
    break;
  case LL_ELF_R_RISCV_SET32:
    write32le((inst_t *)P, addend);
    break;
  case LL_ELF_R_RISCV_PCREL_LO12_S:
  case LL_ELF_R_RISCV_TPREL_LO12_S:
  case LL_ELF_R_RISCV_LO12_S: {
    setSType((inst_t *)P, addend);
    break;
  }
  case LL_ELF_R_RISCV_RELAX:
  case LL_ELF_R_RISCV_ALIGN:
    // Implementing relaxations (rewriting instructions to more efficient ones)
    // could be implemented in future. As the code already is aligned and we do
    // not change the instruction sizes, we should get away with not aligning
    // (though, that is cheating.) To align or change the instruction count, we
    // would need machinery to squeeze or extend memory at the current location.
    break;
  default:
    ll_error_set(L, LL_E_UNSUPPORTED, "Missing relocation 0x%lx\n",
                 LL_ELF64_R_TYPE(rel->r_info));
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

bool encodeAddendRISCV64(ll_linker *L, Section *section,
                         ll_elf_target_rel *rel, int32_t addend) {
  uint8_t *work = (uint8_t *)section->start + rel->r_offset;
  const ll_site site = {.work = work, .exec = (uintptr_t)work};
  return encodeAddendRISCV64Site(L, site, rel, addend);
}

/**
 * Compute the *new* addend for a relocation, given a pre-existing addend.
 * Compute using the execution address, retaining the existing 32-bit result.
 */
static bool computeAddendSite(ObjectCode *oc, Section *section,
                              ll_elf_target_rel *rel, ll_site site,
                              addr_t S, int64_t addend,
                              const ll_reloc_context *context,
                              int64_t *computed) {
  const addr_t work = (addr_t)site.work;
  const addr_t P = site.exec;
  const addr_t GOT_S = context->got_exec;
  LL_CHECK_FOR(oc->L, work != 0);
  LL_CHECK_FOR(oc->L, (uint64_t)section->start <= work);
  LL_CHECK_FOR(oc->L, work <= (uint64_t)section->start + section->size);

  // A: Addend field in the relocation entry associated with the symbol
  int64_t A = addend;

  switch (LL_ELF64_R_TYPE(rel->r_info)) {
  case LL_ELF_R_RISCV_32:
    *computed = S + A;
    return true;
  case LL_ELF_R_RISCV_64:
    *computed = S + A;
    return true;
  case LL_ELF_R_RISCV_HI20:
    *computed = S + A;
    return true;
  case LL_ELF_R_RISCV_JUMP_SLOT:
    *computed = S;
    return true;
  case LL_ELF_R_RISCV_JAL:
    *computed = S + A - P;
    return true;
  case LL_ELF_R_RISCV_PCREL_HI20:
    *computed = S + A - P;
    return true;
  case LL_ELF_R_RISCV_LO12_I:
    *computed = S + A;
    return true;
    // Quoting LLVM docs: For R_RISCV_PC_INDIRECT (R_RISCV_PCREL_LO12_{I,S}),
    // the symbol actually points the corresponding R_RISCV_PCREL_HI20
    // relocation, and the target VA is calculated using PCREL_HI20's symbol.
  case LL_ELF_R_RISCV_PCREL_LO12_S:
  case LL_ELF_R_RISCV_PCREL_LO12_I:
    if (!context->paired_hi_valid) {
      LL_DEBUG_FOR(oc->L, "Missing HI relocation for %s: P 0x%lx S 0x%lx %s\n",
                   relocationTypeToString(rel->r_info), P, S, context->symbol_name);
      ll_error_set(oc->L, LL_E_FORMAT,
                   "Missing HI relocation for %s: P 0x%lx S 0x%lx %s",
                   relocationTypeToString(rel->r_info), P, S, context->symbol_name);
      return false;
    }
    *computed = context->paired_hi_value;
    return true;

  case LL_ELF_R_RISCV_RVC_JUMP:
    *computed = S + A - P;
    return true;
  case LL_ELF_R_RISCV_RVC_BRANCH:
    *computed = S + A - P;
    return true;
  case LL_ELF_R_RISCV_BRANCH:
    *computed = S + A - P;
    return true;
  case LL_ELF_R_RISCV_CALL:
  case LL_ELF_R_RISCV_CALL_PLT: {
    addr_t GOT_Target;
    if (GOT_S != 0) {
      // 1. Public symbol with GOT entry.
      GOT_Target = GOT_S + A;
    } else {
      // 2. Fake GOT entry with symbol extra entry.
      ll_site extra = ll_make_symbol_extra_riscv64_site(oc, context->symbol_index, S + A);
      GOT_Target = extra.exec;
    }

    ll_site stub;
    void *stub_target = (void *)(uintptr_t)(S + A);
    if (elf_findStubSite(section, stub_target, 0, &stub)) {
      if (elf_makeStubSite(oc->L, section, stub_target,
                           (void *)(uintptr_t)GOT_Target, 0, &stub)) {
        abort(/* could not find or make stub */);
      }
    }
    S = stub.exec;
    LL_IF_DEBUG_FOR(oc->L, linker, LL_DEBUG_FOR(oc->L, "R_RISCV_CALL_PLT: S = 0x%lx A = 0x%lx P = "
                                "0x%lx S - P = 0x%lx \n",
                                S, A, P, S - P));
    *computed = S - P;
    return true;
  }
  case LL_ELF_R_RISCV_ADD8:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_ADD16:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_ADD32:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_ADD64:
    *computed = S + A; // Add V when the value is set
    return true;
  case LL_ELF_R_RISCV_SUB6:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_SUB8:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_SUB16:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_SUB32:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_SUB64:
    *computed = S + A; // Subtract from V when value is set
    return true;
  case LL_ELF_R_RISCV_SET6:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_SET8:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_SET16:
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_SET32:
    *computed = S + A;
    return true;
  case LL_ELF_R_RISCV_RELAX:
    // This "relocation" has no addend.
    LL_FALLTHROUGH;
  case LL_ELF_R_RISCV_ALIGN:
    // I guess we don't need to implement this relaxation. Otherwise, this
    // should return the number of blank bytes to insert via NOPs.
    *computed = 0;
    return true;
  case LL_ELF_R_RISCV_32_PCREL:
    *computed = S + A - P;
    return true;
  case LL_ELF_R_RISCV_GOT_HI20: {
    // TODO: Allocating extra memory for every symbol just to play this trick
    // seems to be a bit obscene. (GOT relocations hitting local symbols
    // happens, but not very often.) It would be better to allocate only what we
    // really need.

    // There are two cases here: 1. The symbol is public and has an entry in the
    // GOT. 2. It's local and has no corresponding GOT entry. The first case is
    // easy: We simply calculate the addend with the GOT address. In the second
    // case we create a symbol extra entry and pretend it's the GOT.
    if (GOT_S != 0) {
      // 1. Public symbol with GOT entry.
      *computed = GOT_S + A - P;
      return true;
    } else {
      // 2. Fake GOT entry with symbol extra entry.
      ll_site extra = ll_make_symbol_extra_riscv64_site(oc, context->symbol_index, S);
      addr_t res = extra.exec + A - P;
      *computed = res;
      return true;
    }
  }
  default:
    ll_error_set(oc->L, LL_E_UNSUPPORTED,
               "Unimplemented relocation: 0x%lx\n (%lu)",
               LL_ELF64_R_TYPE(rel->r_info), LL_ELF64_R_TYPE(rel->r_info));
    return false;
  }
}

static int applyRISCV64(ObjectCode *oc, Section *section, uint32_t type,
                        ll_site site, uintptr_t S, int64_t A, ll_sym_type st,
                        const ll_reloc_context *context)
{
  (void)st;
  ll_elf_target_rel rel = {
    .r_info = LL_ELF64_R_INFO(context->symbol_index, type)
  };
  int64_t value;
  if (!computeAddendSite(oc, section, &rel, site, S, A, context, &value))
    return 0;
  return encodeAddendRISCV64Site(oc->L, site, &rel, value) == EXIT_SUCCESS;
}

const struct ll_reloc_ops ll_elf_riscv64_reloc_ops = {
  .machine = LL_ELF_EM_RISCV,
  .apply = applyRISCV64,
  .extra_bytes_per_symbol = sizeof(ll_symbol_extra_riscv64)
};

/* Preserve the selected table's GOT slot even when symbols share a value. */
static ll_reloc_context contextForSymbol(ObjectCode *oc,
                                         ll_elf_target_rel *rel,
                                         const ElfSymbol *symbol)
{
  uintptr_t got_exec = 0;
  if (symbol->got_addr != NULL) {
    const uintptr_t work = (uintptr_t)symbol->got_addr;
    const uintptr_t start = (uintptr_t)oc->elf_info->got_start;
    LL_CHECK_FOR(oc->L, work >= start);
    const uintptr_t offset = work - start;
    LL_CHECK_FOR(oc->L, offset <= oc->elf_info->got_size &&
                 sizeof(void *) <= oc->elf_info->got_size - offset);
    LL_CHECK_FOR(oc->L, offset <= UINTPTR_MAX - oc->elf_info->got_exec);
    got_exec = oc->elf_info->got_exec + offset;
  }
  const ll_reloc_context context = {
    .symbol_index = LL_ELF_R_SYM(rel->r_info),
    .symbol_name = symbol->name,
    .format_symbol_type = LL_ELF_ST_TYPE(symbol->elf_sym->st_info),
    .got_exec = got_exec
  };
  return context;
}

static void traceAddend(ObjectCode *oc, ll_elf_target_rel *rel, ll_site site,
                        addr_t S, int64_t addend,
                        const ll_reloc_context *context, unsigned relNo)
{
  LL_IF_DEBUG_FOR(oc->L, linker,
      LL_DEBUG_FOR(oc->L, "%s: P 0x%lx S 0x%lx %s GOT_S 0x%lx A 0x%lx relNo %u\n",
                   relocationTypeToString(rel->r_info), (addr_t)site.exec, S,
                   context->symbol_name, (addr_t)context->got_exec, addend, relNo));
}

/* A low relocation refers to a preceding HI in this same RELA table. */
static bool pairedHiValue(ObjectCode *oc, Section *section,
                          ElfRelocationATable *table, unsigned relNo,
                          addr_t target, bool *found, int32_t *value)
{
  *found = false;
  for (int64_t i = relNo; i >= 0; --i) {
    ll_elf_target_rela *hi = &table->relocations[i];
    if (section->exec_start + hi->r_offset != target) continue;
    ElfSymbol *symbol = findSymbol(oc, table->sectionHeader->sh_link,
                                  LL_ELF64_R_SYM((ll_elf64_xword)hi->r_info));
    LL_CHECK_FOR(oc->L, symbol != NULL);
    uint64_t type = LL_ELF64_R_TYPE(hi->r_info);
    if (type == LL_ELF_R_RISCV_PCREL_HI20 ||
        type == LL_ELF_R_RISCV_GOT_HI20 ||
        type == LL_ELF_R_RISCV_TLS_GD_HI20 ||
        type == LL_ELF_R_RISCV_TLS_GOT_HI20) {
      ll_elf_target_rel *rel = (ll_elf_target_rel *)hi;
      const ll_reloc_context context = contextForSymbol(oc, rel, symbol);
      const ll_site site = {.work = (uint8_t *)section->start + hi->r_offset,
                            .exec = section->exec_start + hi->r_offset};
      traceAddend(oc, rel, site, (addr_t)symbol->addr, hi->r_addend,
                  &context, (unsigned)i);
      int64_t computed = 0;
      if (!computeAddendSite(oc, section, rel, site, (addr_t)symbol->addr,
                             hi->r_addend, &context, &computed)) {
        return false;
      }
      *value = (int32_t)computed;
      *found = true;
      return true;
    }
  }
  return true;
}

static int applySymbolRISCV64(ObjectCode *oc, Section *section,
                              ElfRelocationATable *table, unsigned relNo,
                              ll_elf_target_rel *rel, const ElfSymbol *symbol,
                              int64_t addend)
{
  ll_reloc_context context = contextForSymbol(oc, rel, symbol);
  const ll_site site = {.work = (uint8_t *)section->start + rel->r_offset,
                        .exec = section->exec_start + rel->r_offset};
  const uint32_t type = LL_ELF64_R_TYPE(rel->r_info);
  traceAddend(oc, rel, site, (addr_t)symbol->addr, addend, &context, relNo);
  if (type == LL_ELF_R_RISCV_PCREL_LO12_I ||
      type == LL_ELF_R_RISCV_PCREL_LO12_S) {
    if (!pairedHiValue(oc, section, table, relNo, (addr_t)symbol->addr,
                       &context.paired_hi_valid, &context.paired_hi_value))
      return 0;
  }
  return ll_elf_riscv64_reloc_ops.apply(oc, section, type, site,
                                        (uintptr_t)symbol->addr, addend,
                                        (ll_sym_type)0, &context);
}

// Iterate over all relocations and perform them.
bool relocateObjectCodeRISCV64(ObjectCode *oc) {
  for (ElfRelocationTable *relTab = oc->elf_info->relTable; relTab != NULL;
       relTab = relTab->next) {
    /* only relocate interesting sections */
    if (SECTIONKIND_OTHER == oc->sections[relTab->targetSectionIndex].kind)
      continue;

    Section *targetSection = &oc->sections[relTab->targetSectionIndex];

    for (unsigned i = 0; i < relTab->n_relocations; i++) {
      ll_elf_target_rel *rel = &relTab->relocations[i];

      ElfSymbol *symbol = findSymbol(oc, relTab->sectionHeader->sh_link,
                                     LL_ELF64_R_SYM((ll_elf64_xword)rel->r_info));

      LL_CHECK_FOR(oc->L, 0x0 != symbol);

      // This always fails, because we don't support Rel locations, yet: Do we
      // need this case? Leaving it in to spot the potential bug when it
      // appears.
      /* decode implicit addend */
      int32_t addend;
      if (!decodeAddendRISCV64(oc->L, targetSection, rel, &addend))
        return EXIT_FAILURE;
      if (!applySymbolRISCV64(oc, targetSection, (ElfRelocationATable *)relTab,
                              i, rel, symbol, addend))
        return EXIT_FAILURE;
    }
  }
  for (ElfRelocationATable *relaTab = oc->elf_info->relaTable; relaTab != NULL;
       relaTab = relaTab->next) {
    /* only relocate interesting sections */
    if (SECTIONKIND_OTHER == oc->sections[relaTab->targetSectionIndex].kind)
      continue;

    Section *targetSection = &oc->sections[relaTab->targetSectionIndex];

    for (unsigned i = 0; i < relaTab->n_relocations; i++) {

      ll_elf_target_rela *rel = &relaTab->relocations[i];

      ElfSymbol *symbol = findSymbol(oc, relaTab->sectionHeader->sh_link,
                                     LL_ELF64_R_SYM((ll_elf64_xword)rel->r_info));

      LL_CHECK_FOR(oc->L, 0x0 != symbol);

      /* take explicit addend */
      if (!applySymbolRISCV64(oc, targetSection, relaTab, i,
                              (ll_elf_target_rel *)rel, symbol, rel->r_addend))
        return EXIT_FAILURE;
    }
  }
  return EXIT_SUCCESS;
}

void flushInstructionCacheRISCV64(ObjectCode *oc) {
  // Synchronize the memory and instruction cache to prevent illegal instruction
  // exceptions. On Linux the parameters of __builtin___clear_cache are
  // currently unused. Add them anyways for future compatibility. (I.e. the
  // parameters couldn't be checked during development.)

  /* The main object code */
  void *codeBegin = oc->image + oc->misalignment;
  __builtin___clear_cache(codeBegin, (char *)codeBegin + oc->fileSize);

  /* Jump Islands */
  __builtin___clear_cache((void *)oc->symbol_extras,
                          (char *)oc->symbol_extras +
                              oc->symbol_extra_stride * oc->n_symbol_extras);

  // Memory barrier to ensure nothing circumvents the fence.i / cache flushes.
  atomic_thread_fence(memory_order_seq_cst);
}

#endif /* OBJECTFORMAT_ELF */
#endif /* riscv64_HOST_ARCH */
