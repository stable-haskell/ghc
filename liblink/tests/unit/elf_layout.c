/* Fixed ABI offsets and aligned wire copies, with optional comparisons to
 * the real native ELF SDK. No platform/configuration substitutes. */
#include <stddef.h>
#include "liblink/elf.h"
#ifdef LL_ELF_COMPARE_SDK
#if defined(__OpenBSD__)
#include <elf_abi.h>
#else
#include <elf.h>
#endif
#define SDK_RECORD(type, sdk) \
    _Static_assert(sizeof(type) == sizeof(sdk), "SDK size"); \
    _Static_assert(_Alignof(type) == _Alignof(sdk), "SDK alignment")
#define SDK_FIELD(type, sdk, field) \
    _Static_assert(offsetof(type, field) == offsetof(sdk, field), "SDK offset")
#else
#define SDK_RECORD(type, sdk) _Static_assert(1, "portable-only size")
#define SDK_FIELD(type, sdk, field) _Static_assert(1, "portable-only offset")
#endif
/* Some ABIs align a scalar more strictly outside a structure. */
struct elf32_alignment { uint32_t value; };
struct elf64_alignment { uint64_t value; };
#define RECORD(type, sdk, bytes, align_type) \
    _Static_assert(sizeof(type) == (bytes), "wire size"); \
    _Static_assert(_Alignof(type) == _Alignof(align_type), "wire alignment"); \
    SDK_RECORD(type, sdk)
#define FIELD(type, sdk, field, offset) \
    _Static_assert(offsetof(type, field) == (offset), "wire offset"); \
    SDK_FIELD(type, sdk, field)

RECORD(ll_elf32_ehdr, Elf32_Ehdr, 52, struct elf32_alignment);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_ident, 0);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_type, 16);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_machine, 18);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_version, 20);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_entry, 24);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_phoff, 28);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_shoff, 32);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_flags, 36);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_ehsize, 40);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_phentsize, 42);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_phnum, 44);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_shentsize, 46);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_shnum, 48);
FIELD(ll_elf32_ehdr, Elf32_Ehdr, e_shstrndx, 50);
RECORD(ll_elf32_phdr, Elf32_Phdr, 32, struct elf32_alignment);
FIELD(ll_elf32_phdr, Elf32_Phdr, p_type, 0);
FIELD(ll_elf32_phdr, Elf32_Phdr, p_offset, 4);
FIELD(ll_elf32_phdr, Elf32_Phdr, p_vaddr, 8);
FIELD(ll_elf32_phdr, Elf32_Phdr, p_paddr, 12);
FIELD(ll_elf32_phdr, Elf32_Phdr, p_filesz, 16);
FIELD(ll_elf32_phdr, Elf32_Phdr, p_memsz, 20);
FIELD(ll_elf32_phdr, Elf32_Phdr, p_flags, 24);
FIELD(ll_elf32_phdr, Elf32_Phdr, p_align, 28);
RECORD(ll_elf32_shdr, Elf32_Shdr, 40, struct elf32_alignment);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_name, 0);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_type, 4);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_flags, 8);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_addr, 12);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_offset, 16);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_size, 20);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_link, 24);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_info, 28);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_addralign, 32);
FIELD(ll_elf32_shdr, Elf32_Shdr, sh_entsize, 36);
RECORD(ll_elf32_sym, Elf32_Sym, 16, struct elf32_alignment);
FIELD(ll_elf32_sym, Elf32_Sym, st_name, 0);
FIELD(ll_elf32_sym, Elf32_Sym, st_value, 4);
FIELD(ll_elf32_sym, Elf32_Sym, st_size, 8);
FIELD(ll_elf32_sym, Elf32_Sym, st_info, 12);
FIELD(ll_elf32_sym, Elf32_Sym, st_other, 13);
FIELD(ll_elf32_sym, Elf32_Sym, st_shndx, 14);
RECORD(ll_elf32_rel, Elf32_Rel, 8, struct elf32_alignment);
FIELD(ll_elf32_rel, Elf32_Rel, r_offset, 0);
FIELD(ll_elf32_rel, Elf32_Rel, r_info, 4);
RECORD(ll_elf32_rela, Elf32_Rela, 12, struct elf32_alignment);
FIELD(ll_elf32_rela, Elf32_Rela, r_offset, 0);
FIELD(ll_elf32_rela, Elf32_Rela, r_info, 4);
FIELD(ll_elf32_rela, Elf32_Rela, r_addend, 8);
SDK_RECORD(ll_elf32_half, Elf32_Half);
SDK_RECORD(ll_elf32_word, Elf32_Word);
SDK_RECORD(ll_elf32_sword, Elf32_Sword);
SDK_RECORD(ll_elf32_xword, Elf32_Xword);
SDK_RECORD(ll_elf32_sxword, Elf32_Sxword);
SDK_RECORD(ll_elf32_addr, Elf32_Addr);
SDK_RECORD(ll_elf32_off, Elf32_Off);
RECORD(ll_elf64_ehdr, Elf64_Ehdr, 64, struct elf64_alignment);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_ident, 0);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_type, 16);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_machine, 18);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_version, 20);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_entry, 24);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_phoff, 32);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_shoff, 40);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_flags, 48);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_ehsize, 52);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_phentsize, 54);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_phnum, 56);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_shentsize, 58);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_shnum, 60);
FIELD(ll_elf64_ehdr, Elf64_Ehdr, e_shstrndx, 62);
RECORD(ll_elf64_phdr, Elf64_Phdr, 56, struct elf64_alignment);
FIELD(ll_elf64_phdr, Elf64_Phdr, p_type, 0);
FIELD(ll_elf64_phdr, Elf64_Phdr, p_offset, 8);
FIELD(ll_elf64_phdr, Elf64_Phdr, p_vaddr, 16);
FIELD(ll_elf64_phdr, Elf64_Phdr, p_paddr, 24);
FIELD(ll_elf64_phdr, Elf64_Phdr, p_filesz, 32);
FIELD(ll_elf64_phdr, Elf64_Phdr, p_memsz, 40);
FIELD(ll_elf64_phdr, Elf64_Phdr, p_flags, 4);
FIELD(ll_elf64_phdr, Elf64_Phdr, p_align, 48);
RECORD(ll_elf64_shdr, Elf64_Shdr, 64, struct elf64_alignment);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_name, 0);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_type, 4);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_flags, 8);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_addr, 16);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_offset, 24);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_size, 32);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_link, 40);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_info, 44);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_addralign, 48);
FIELD(ll_elf64_shdr, Elf64_Shdr, sh_entsize, 56);
RECORD(ll_elf64_sym, Elf64_Sym, 24, struct elf64_alignment);
FIELD(ll_elf64_sym, Elf64_Sym, st_name, 0);
FIELD(ll_elf64_sym, Elf64_Sym, st_value, 8);
FIELD(ll_elf64_sym, Elf64_Sym, st_size, 16);
FIELD(ll_elf64_sym, Elf64_Sym, st_info, 4);
FIELD(ll_elf64_sym, Elf64_Sym, st_other, 5);
FIELD(ll_elf64_sym, Elf64_Sym, st_shndx, 6);
RECORD(ll_elf64_rel, Elf64_Rel, 16, struct elf64_alignment);
FIELD(ll_elf64_rel, Elf64_Rel, r_offset, 0);
FIELD(ll_elf64_rel, Elf64_Rel, r_info, 8);
RECORD(ll_elf64_rela, Elf64_Rela, 24, struct elf64_alignment);
FIELD(ll_elf64_rela, Elf64_Rela, r_offset, 0);
FIELD(ll_elf64_rela, Elf64_Rela, r_info, 8);
FIELD(ll_elf64_rela, Elf64_Rela, r_addend, 16);
SDK_RECORD(ll_elf64_half, Elf64_Half);
SDK_RECORD(ll_elf64_word, Elf64_Word);
SDK_RECORD(ll_elf64_sword, Elf64_Sword);
SDK_RECORD(ll_elf64_xword, Elf64_Xword);
SDK_RECORD(ll_elf64_sxword, Elf64_Sxword);
SDK_RECORD(ll_elf64_addr, Elf64_Addr);
SDK_RECORD(ll_elf64_off, Elf64_Off);

#ifdef LL_ELF_COMPARE_SDK
#ifdef EI_CLASS
_Static_assert(LL_ELF_EI_CLASS == EI_CLASS, "EI_CLASS");
#endif
#ifdef EI_DATA
_Static_assert(LL_ELF_EI_DATA == EI_DATA, "EI_DATA");
#endif
#ifdef EI_MAG0
_Static_assert(LL_ELF_EI_MAG0 == EI_MAG0, "EI_MAG0");
#endif
#ifdef EI_MAG1
_Static_assert(LL_ELF_EI_MAG1 == EI_MAG1, "EI_MAG1");
#endif
#ifdef EI_MAG2
_Static_assert(LL_ELF_EI_MAG2 == EI_MAG2, "EI_MAG2");
#endif
#ifdef EI_MAG3
_Static_assert(LL_ELF_EI_MAG3 == EI_MAG3, "EI_MAG3");
#endif
#ifdef EI_NIDENT
_Static_assert(LL_ELF_EI_NIDENT == EI_NIDENT, "EI_NIDENT");
#endif
#ifdef EI_VERSION
_Static_assert(LL_ELF_EI_VERSION == EI_VERSION, "EI_VERSION");
#endif
#ifdef ELFCLASS32
_Static_assert(LL_ELF_ELFCLASS32 == ELFCLASS32, "ELFCLASS32");
#endif
#ifdef ELFCLASS64
_Static_assert(LL_ELF_ELFCLASS64 == ELFCLASS64, "ELFCLASS64");
#endif
#ifdef ELFDATA2LSB
_Static_assert(LL_ELF_ELFDATA2LSB == ELFDATA2LSB, "ELFDATA2LSB");
#endif
#ifdef ELFDATA2MSB
_Static_assert(LL_ELF_ELFDATA2MSB == ELFDATA2MSB, "ELFDATA2MSB");
#endif
#ifdef ELFMAG0
_Static_assert(LL_ELF_ELFMAG0 == ELFMAG0, "ELFMAG0");
#endif
#ifdef ELFMAG1
_Static_assert(LL_ELF_ELFMAG1 == ELFMAG1, "ELFMAG1");
#endif
#ifdef ELFMAG2
_Static_assert(LL_ELF_ELFMAG2 == ELFMAG2, "ELFMAG2");
#endif
#ifdef ELFMAG3
_Static_assert(LL_ELF_ELFMAG3 == ELFMAG3, "ELFMAG3");
#endif
#ifdef EM_386
_Static_assert(LL_ELF_EM_386 == EM_386, "EM_386");
#endif
#ifdef EM_AARCH64
_Static_assert(LL_ELF_EM_AARCH64 == EM_AARCH64, "EM_AARCH64");
#endif
#ifdef EM_AMD64
_Static_assert(LL_ELF_EM_AMD64 == EM_AMD64, "EM_AMD64");
#endif
#ifdef EM_ARM
_Static_assert(LL_ELF_EM_ARM == EM_ARM, "EM_ARM");
#endif
#ifdef EM_IA_64
_Static_assert(LL_ELF_EM_IA_64 == EM_IA_64, "EM_IA_64");
#endif
#ifdef EM_LOONGARCH
_Static_assert(LL_ELF_EM_LOONGARCH == EM_LOONGARCH, "EM_LOONGARCH");
#endif
#ifdef EM_PPC
_Static_assert(LL_ELF_EM_PPC == EM_PPC, "EM_PPC");
#endif
#ifdef EM_PPC64
_Static_assert(LL_ELF_EM_PPC64 == EM_PPC64, "EM_PPC64");
#endif
#ifdef EM_RISCV
_Static_assert(LL_ELF_EM_RISCV == EM_RISCV, "EM_RISCV");
#endif
#ifdef EM_S390
_Static_assert(LL_ELF_EM_S390 == EM_S390, "EM_S390");
#endif
#ifdef EM_SPARC
_Static_assert(LL_ELF_EM_SPARC == EM_SPARC, "EM_SPARC");
#endif
#ifdef EM_SPARC32PLUS
_Static_assert(LL_ELF_EM_SPARC32PLUS == EM_SPARC32PLUS, "EM_SPARC32PLUS");
#endif
#ifdef EM_X86_64
_Static_assert(LL_ELF_EM_X86_64 == EM_X86_64, "EM_X86_64");
#endif
#ifdef ET_REL
_Static_assert(LL_ELF_ET_REL == ET_REL, "ET_REL");
#endif
#ifdef EV_CURRENT
_Static_assert(LL_ELF_EV_CURRENT == EV_CURRENT, "EV_CURRENT");
#endif
#ifdef R_PPC_ADDR16_HA
_Static_assert(LL_ELF_R_PPC_ADDR16_HA == R_PPC_ADDR16_HA, "R_PPC_ADDR16_HA");
#endif
#ifdef R_PPC_ADDR16_HI
_Static_assert(LL_ELF_R_PPC_ADDR16_HI == R_PPC_ADDR16_HI, "R_PPC_ADDR16_HI");
#endif
#ifdef R_PPC_ADDR16_LO
_Static_assert(LL_ELF_R_PPC_ADDR16_LO == R_PPC_ADDR16_LO, "R_PPC_ADDR16_LO");
#endif
#ifdef R_PPC_ADDR32
_Static_assert(LL_ELF_R_PPC_ADDR32 == R_PPC_ADDR32, "R_PPC_ADDR32");
#endif
#ifdef R_PPC_PLTREL24
_Static_assert(LL_ELF_R_PPC_PLTREL24 == R_PPC_PLTREL24, "R_PPC_PLTREL24");
#endif
#ifdef R_PPC_REL16_HA
_Static_assert(LL_ELF_R_PPC_REL16_HA == R_PPC_REL16_HA, "R_PPC_REL16_HA");
#endif
#ifdef R_PPC_REL16_HI
_Static_assert(LL_ELF_R_PPC_REL16_HI == R_PPC_REL16_HI, "R_PPC_REL16_HI");
#endif
#ifdef R_PPC_REL16_LO
_Static_assert(LL_ELF_R_PPC_REL16_LO == R_PPC_REL16_LO, "R_PPC_REL16_LO");
#endif
#ifdef R_PPC_REL24
_Static_assert(LL_ELF_R_PPC_REL24 == R_PPC_REL24, "R_PPC_REL24");
#endif
#ifdef R_PPC_REL32
_Static_assert(LL_ELF_R_PPC_REL32 == R_PPC_REL32, "R_PPC_REL32");
#endif
#ifdef R_RISCV_32
_Static_assert(LL_ELF_R_RISCV_32 == R_RISCV_32, "R_RISCV_32");
#endif
#ifdef R_RISCV_32_PCREL
_Static_assert(LL_ELF_R_RISCV_32_PCREL == R_RISCV_32_PCREL, "R_RISCV_32_PCREL");
#endif
#ifdef R_RISCV_64
_Static_assert(LL_ELF_R_RISCV_64 == R_RISCV_64, "R_RISCV_64");
#endif
#ifdef R_RISCV_ADD16
_Static_assert(LL_ELF_R_RISCV_ADD16 == R_RISCV_ADD16, "R_RISCV_ADD16");
#endif
#ifdef R_RISCV_ADD32
_Static_assert(LL_ELF_R_RISCV_ADD32 == R_RISCV_ADD32, "R_RISCV_ADD32");
#endif
#ifdef R_RISCV_ADD64
_Static_assert(LL_ELF_R_RISCV_ADD64 == R_RISCV_ADD64, "R_RISCV_ADD64");
#endif
#ifdef R_RISCV_ADD8
_Static_assert(LL_ELF_R_RISCV_ADD8 == R_RISCV_ADD8, "R_RISCV_ADD8");
#endif
#ifdef R_RISCV_ALIGN
_Static_assert(LL_ELF_R_RISCV_ALIGN == R_RISCV_ALIGN, "R_RISCV_ALIGN");
#endif
#ifdef R_RISCV_BRANCH
_Static_assert(LL_ELF_R_RISCV_BRANCH == R_RISCV_BRANCH, "R_RISCV_BRANCH");
#endif
#ifdef R_RISCV_CALL
_Static_assert(LL_ELF_R_RISCV_CALL == R_RISCV_CALL, "R_RISCV_CALL");
#endif
#ifdef R_RISCV_CALL_PLT
_Static_assert(LL_ELF_R_RISCV_CALL_PLT == R_RISCV_CALL_PLT, "R_RISCV_CALL_PLT");
#endif
#ifdef R_RISCV_COPY
_Static_assert(LL_ELF_R_RISCV_COPY == R_RISCV_COPY, "R_RISCV_COPY");
#endif
#ifdef R_RISCV_GOT_HI20
_Static_assert(LL_ELF_R_RISCV_GOT_HI20 == R_RISCV_GOT_HI20, "R_RISCV_GOT_HI20");
#endif
#ifdef R_RISCV_HI20
_Static_assert(LL_ELF_R_RISCV_HI20 == R_RISCV_HI20, "R_RISCV_HI20");
#endif
#ifdef R_RISCV_JAL
_Static_assert(LL_ELF_R_RISCV_JAL == R_RISCV_JAL, "R_RISCV_JAL");
#endif
#ifdef R_RISCV_JUMP_SLOT
_Static_assert(LL_ELF_R_RISCV_JUMP_SLOT == R_RISCV_JUMP_SLOT, "R_RISCV_JUMP_SLOT");
#endif
#ifdef R_RISCV_LO12_I
_Static_assert(LL_ELF_R_RISCV_LO12_I == R_RISCV_LO12_I, "R_RISCV_LO12_I");
#endif
#ifdef R_RISCV_LO12_S
_Static_assert(LL_ELF_R_RISCV_LO12_S == R_RISCV_LO12_S, "R_RISCV_LO12_S");
#endif
#ifdef R_RISCV_NONE
_Static_assert(LL_ELF_R_RISCV_NONE == R_RISCV_NONE, "R_RISCV_NONE");
#endif
#ifdef R_RISCV_PCREL_HI20
_Static_assert(LL_ELF_R_RISCV_PCREL_HI20 == R_RISCV_PCREL_HI20, "R_RISCV_PCREL_HI20");
#endif
#ifdef R_RISCV_PCREL_LO12_I
_Static_assert(LL_ELF_R_RISCV_PCREL_LO12_I == R_RISCV_PCREL_LO12_I, "R_RISCV_PCREL_LO12_I");
#endif
#ifdef R_RISCV_PCREL_LO12_S
_Static_assert(LL_ELF_R_RISCV_PCREL_LO12_S == R_RISCV_PCREL_LO12_S, "R_RISCV_PCREL_LO12_S");
#endif
#ifdef R_RISCV_RELATIVE
_Static_assert(LL_ELF_R_RISCV_RELATIVE == R_RISCV_RELATIVE, "R_RISCV_RELATIVE");
#endif
#ifdef R_RISCV_RELAX
_Static_assert(LL_ELF_R_RISCV_RELAX == R_RISCV_RELAX, "R_RISCV_RELAX");
#endif
#ifdef R_RISCV_RVC_BRANCH
_Static_assert(LL_ELF_R_RISCV_RVC_BRANCH == R_RISCV_RVC_BRANCH, "R_RISCV_RVC_BRANCH");
#endif
#ifdef R_RISCV_RVC_JUMP
_Static_assert(LL_ELF_R_RISCV_RVC_JUMP == R_RISCV_RVC_JUMP, "R_RISCV_RVC_JUMP");
#endif
#ifdef R_RISCV_SET16
_Static_assert(LL_ELF_R_RISCV_SET16 == R_RISCV_SET16, "R_RISCV_SET16");
#endif
#ifdef R_RISCV_SET32
_Static_assert(LL_ELF_R_RISCV_SET32 == R_RISCV_SET32, "R_RISCV_SET32");
#endif
#ifdef R_RISCV_SET6
_Static_assert(LL_ELF_R_RISCV_SET6 == R_RISCV_SET6, "R_RISCV_SET6");
#endif
#ifdef R_RISCV_SET8
_Static_assert(LL_ELF_R_RISCV_SET8 == R_RISCV_SET8, "R_RISCV_SET8");
#endif
#ifdef R_RISCV_SUB16
_Static_assert(LL_ELF_R_RISCV_SUB16 == R_RISCV_SUB16, "R_RISCV_SUB16");
#endif
#ifdef R_RISCV_SUB32
_Static_assert(LL_ELF_R_RISCV_SUB32 == R_RISCV_SUB32, "R_RISCV_SUB32");
#endif
#ifdef R_RISCV_SUB6
_Static_assert(LL_ELF_R_RISCV_SUB6 == R_RISCV_SUB6, "R_RISCV_SUB6");
#endif
#ifdef R_RISCV_SUB64
_Static_assert(LL_ELF_R_RISCV_SUB64 == R_RISCV_SUB64, "R_RISCV_SUB64");
#endif
#ifdef R_RISCV_SUB8
_Static_assert(LL_ELF_R_RISCV_SUB8 == R_RISCV_SUB8, "R_RISCV_SUB8");
#endif
#ifdef R_RISCV_TLS_DTPMOD32
_Static_assert(LL_ELF_R_RISCV_TLS_DTPMOD32 == R_RISCV_TLS_DTPMOD32, "R_RISCV_TLS_DTPMOD32");
#endif
#ifdef R_RISCV_TLS_DTPMOD64
_Static_assert(LL_ELF_R_RISCV_TLS_DTPMOD64 == R_RISCV_TLS_DTPMOD64, "R_RISCV_TLS_DTPMOD64");
#endif
#ifdef R_RISCV_TLS_DTPREL32
_Static_assert(LL_ELF_R_RISCV_TLS_DTPREL32 == R_RISCV_TLS_DTPREL32, "R_RISCV_TLS_DTPREL32");
#endif
#ifdef R_RISCV_TLS_DTPREL64
_Static_assert(LL_ELF_R_RISCV_TLS_DTPREL64 == R_RISCV_TLS_DTPREL64, "R_RISCV_TLS_DTPREL64");
#endif
#ifdef R_RISCV_TLS_GD_HI20
_Static_assert(LL_ELF_R_RISCV_TLS_GD_HI20 == R_RISCV_TLS_GD_HI20, "R_RISCV_TLS_GD_HI20");
#endif
#ifdef R_RISCV_TLS_GOT_HI20
_Static_assert(LL_ELF_R_RISCV_TLS_GOT_HI20 == R_RISCV_TLS_GOT_HI20, "R_RISCV_TLS_GOT_HI20");
#endif
#ifdef R_RISCV_TLS_TPREL32
_Static_assert(LL_ELF_R_RISCV_TLS_TPREL32 == R_RISCV_TLS_TPREL32, "R_RISCV_TLS_TPREL32");
#endif
#ifdef R_RISCV_TLS_TPREL64
_Static_assert(LL_ELF_R_RISCV_TLS_TPREL64 == R_RISCV_TLS_TPREL64, "R_RISCV_TLS_TPREL64");
#endif
#ifdef R_RISCV_TPREL_LO12_S
_Static_assert(LL_ELF_R_RISCV_TPREL_LO12_S == R_RISCV_TPREL_LO12_S, "R_RISCV_TPREL_LO12_S");
#endif
#ifdef SELFMAG
_Static_assert(LL_ELF_SELFMAG == SELFMAG, "SELFMAG");
#endif
#ifdef SHF_ALLOC
_Static_assert(LL_ELF_SHF_ALLOC == SHF_ALLOC, "SHF_ALLOC");
#endif
#ifdef SHF_EXECINSTR
_Static_assert(LL_ELF_SHF_EXECINSTR == SHF_EXECINSTR, "SHF_EXECINSTR");
#endif
#ifdef SHF_WRITE
_Static_assert(LL_ELF_SHF_WRITE == SHF_WRITE, "SHF_WRITE");
#endif
#ifdef SHN_ABS
_Static_assert(LL_ELF_SHN_ABS == SHN_ABS, "SHN_ABS");
#endif
#ifdef SHN_COMMON
_Static_assert(LL_ELF_SHN_COMMON == SHN_COMMON, "SHN_COMMON");
#endif
#ifdef SHN_LORESERVE
_Static_assert(LL_ELF_SHN_LORESERVE == SHN_LORESERVE, "SHN_LORESERVE");
#endif
#ifdef SHN_UNDEF
_Static_assert(LL_ELF_SHN_UNDEF == SHN_UNDEF, "SHN_UNDEF");
#endif
#ifdef SHN_XINDEX
_Static_assert(LL_ELF_SHN_XINDEX == SHN_XINDEX, "SHN_XINDEX");
#endif
#ifdef SHT_FINI_ARRAY
_Static_assert(LL_ELF_SHT_FINI_ARRAY == SHT_FINI_ARRAY, "SHT_FINI_ARRAY");
#endif
#ifdef SHT_INIT_ARRAY
_Static_assert(LL_ELF_SHT_INIT_ARRAY == SHT_INIT_ARRAY, "SHT_INIT_ARRAY");
#endif
#ifdef SHT_NOBITS
_Static_assert(LL_ELF_SHT_NOBITS == SHT_NOBITS, "SHT_NOBITS");
#endif
#ifdef SHT_NULL
_Static_assert(LL_ELF_SHT_NULL == SHT_NULL, "SHT_NULL");
#endif
#ifdef SHT_PROGBITS
_Static_assert(LL_ELF_SHT_PROGBITS == SHT_PROGBITS, "SHT_PROGBITS");
#endif
#ifdef SHT_REL
_Static_assert(LL_ELF_SHT_REL == SHT_REL, "SHT_REL");
#endif
#ifdef SHT_RELA
_Static_assert(LL_ELF_SHT_RELA == SHT_RELA, "SHT_RELA");
#endif
#ifdef SHT_STRTAB
_Static_assert(LL_ELF_SHT_STRTAB == SHT_STRTAB, "SHT_STRTAB");
#endif
#ifdef SHT_SYMTAB
_Static_assert(LL_ELF_SHT_SYMTAB == SHT_SYMTAB, "SHT_SYMTAB");
#endif
#ifdef SHT_SYMTAB_SHNDX
_Static_assert(LL_ELF_SHT_SYMTAB_SHNDX == SHT_SYMTAB_SHNDX, "SHT_SYMTAB_SHNDX");
#endif
#ifdef STB_GLOBAL
_Static_assert(LL_ELF_STB_GLOBAL == STB_GLOBAL, "STB_GLOBAL");
#endif
#ifdef STB_LOCAL
_Static_assert(LL_ELF_STB_LOCAL == STB_LOCAL, "STB_LOCAL");
#endif
#ifdef STB_WEAK
_Static_assert(LL_ELF_STB_WEAK == STB_WEAK, "STB_WEAK");
#endif
#ifdef STT_FILE
_Static_assert(LL_ELF_STT_FILE == STT_FILE, "STT_FILE");
#endif
#ifdef STT_FUNC
_Static_assert(LL_ELF_STT_FUNC == STT_FUNC, "STT_FUNC");
#endif
#ifdef STT_NOTYPE
_Static_assert(LL_ELF_STT_NOTYPE == STT_NOTYPE, "STT_NOTYPE");
#endif
#ifdef STT_OBJECT
_Static_assert(LL_ELF_STT_OBJECT == STT_OBJECT, "STT_OBJECT");
#endif
#ifdef STT_SECTION
_Static_assert(LL_ELF_STT_SECTION == STT_SECTION, "STT_SECTION");
#endif
#ifdef STV_HIDDEN
_Static_assert(LL_ELF_STV_HIDDEN == STV_HIDDEN, "STV_HIDDEN");
#endif
#endif

static void copy_bytes(void *destination, const unsigned char *source, size_t n)
{
    unsigned char *out = destination;
    for (size_t i = 0; i < n; ++i) out[i] = source[i];
}

int main(void)
{
    /* Literal little-endian wire fields, deliberately unaligned in the input.
     * The driver skips this runtime check on a big-endian host. */
    const unsigned char sym32_bytes[17] = {
        0, 0x78, 0x56, 0x34, 0x12, 0xef, 0xcd, 0xab, 0x89,
        0x10, 0x32, 0x54, 0x76, 0x12, 0x03, 0x34, 0x12
    };
    const unsigned char sym64_bytes[25] = {
        0, 0x78, 0x56, 0x34, 0x12, 0x12, 0x03, 0x34, 0x12,
        0xef, 0xcd, 0xab, 0x89, 0x67, 0x45, 0x23, 0x01,
        0x10, 0x32, 0x54, 0x76, 0x98, 0xba, 0xdc, 0xfe
    };
    ll_elf32_sym sym32;
    ll_elf64_sym sym64;
    copy_bytes(&sym32, sym32_bytes + 1, sizeof(sym32));
    copy_bytes(&sym64, sym64_bytes + 1, sizeof(sym64));
    if (sym32.st_name != UINT32_C(0x12345678) ||
        sym32.st_value != UINT32_C(0x89abcdef) ||
        sym32.st_size != UINT32_C(0x76543210) ||
        sym32.st_info != 0x12 || sym32.st_other != 3 || sym32.st_shndx != 0x1234)
        return 1;
    if (sym64.st_name != UINT32_C(0x12345678) ||
        sym64.st_value != UINT64_C(0x0123456789abcdef) ||
        sym64.st_size != UINT64_C(0xfedcba9876543210) ||
        sym64.st_info != 0x12 || sym64.st_other != 3 || sym64.st_shndx != 0x1234)
        return 2;
    for (unsigned int value = 0; value < 256; ++value) {
        if (LL_ELF32_ST_BIND(value) != value / 16 ||
            LL_ELF64_ST_TYPE(value) != value % 16 ||
            LL_ELF32_ST_VISIBILITY(value) != value % 4 ||
            LL_ELF64_ST_INFO(value / 16, value % 16) != value) return 3;
#ifdef LL_ELF_COMPARE_SDK
        if (LL_ELF32_ST_BIND(value) != ELF32_ST_BIND(value) ||
            LL_ELF64_ST_TYPE(value) != ELF64_ST_TYPE(value) ||
            LL_ELF64_ST_INFO(value / 16, value % 16) != ELF64_ST_INFO(value / 16, value % 16))
            return 4;
#endif
    }
    const uint32_t symbols[] = {0, 1, 0x123456, 0xffffff};
    const uint32_t types[] = {0, 1, 0x80, 0xff};
    for (size_t i = 0; i < sizeof(symbols) / sizeof(symbols[0]); ++i) {
        for (size_t j = 0; j < sizeof(types) / sizeof(types[0]); ++j) {
            uint32_t info32 = LL_ELF32_R_INFO(symbols[i], types[j]);
            uint64_t info64 = LL_ELF64_R_INFO(symbols[i], types[j]);
            if (LL_ELF32_R_SYM(info32) != symbols[i] ||
                LL_ELF32_R_TYPE(info32) != types[j] ||
                LL_ELF64_R_SYM(info64) != symbols[i] ||
                LL_ELF64_R_TYPE(info64) != types[j]) return 5;
#ifdef LL_ELF_COMPARE_SDK
            if (info32 != ELF32_R_INFO(symbols[i], types[j]) ||
                info64 != ELF64_R_INFO(symbols[i], types[j]) ||
                LL_ELF32_R_SYM(info32) != ELF32_R_SYM(info32) ||
                LL_ELF64_R_TYPE(info64) != ELF64_R_TYPE(info64)) return 6;
#endif
        }
    }
    const unsigned char *magic = (const unsigned char *)LL_ELF_ELFMAG;
    if (magic[0] != 0x7f || magic[1] != 'E' || magic[2] != 'L' ||
        magic[3] != 'F' || magic[4] != 0) return 9;
#ifdef LL_ELF_COMPARE_SDK
    const unsigned char *sdk_magic = (const unsigned char *)ELFMAG;
    for (size_t i = 0; i <= LL_ELF_SELFMAG; ++i)
        if (magic[i] != sdk_magic[i]) return 10;
#endif
    uint64_t limit = LL_ELF64_R_INFO(UINT32_MAX, UINT32_MAX);
    if (limit != UINT64_MAX || LL_ELF64_R_SYM(limit) != UINT32_MAX ||
        LL_ELF64_R_TYPE(limit) != UINT32_MAX) return 7;
#ifdef LL_ELF_COMPARE_SDK
    if (limit != ELF64_R_INFO(UINT32_MAX, UINT32_MAX)) return 8;
#endif
    return 0;
}
