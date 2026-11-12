/* ELF object-file declarations, independent of the host ELF SDK.
 * Fixed-width records follow the ELF32/ELF64 ABI. They describe native-endian
 * file data; byte-order conversion and validation remain backend policy.
 * Access records at their required alignment, or copy into an aligned record.
 */
#pragma once
#include <stdint.h>

typedef uint16_t ll_elf32_half;
typedef uint32_t ll_elf32_word;
typedef int32_t ll_elf32_sword;
typedef uint64_t ll_elf32_xword;
typedef int64_t ll_elf32_sxword;
typedef uint32_t ll_elf32_addr;
typedef uint32_t ll_elf32_off;

typedef struct ll_elf32_ehdr {
    unsigned char e_ident[16];
    ll_elf32_half e_type, e_machine;
    ll_elf32_word e_version;
    ll_elf32_addr e_entry;
    ll_elf32_off e_phoff, e_shoff;
    ll_elf32_word e_flags;
    ll_elf32_half e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx;
} ll_elf32_ehdr;

typedef struct ll_elf32_phdr {
    ll_elf32_word p_type;
    ll_elf32_off p_offset;
    ll_elf32_addr p_vaddr, p_paddr;
    ll_elf32_word p_filesz, p_memsz, p_flags, p_align;
} ll_elf32_phdr;

typedef struct ll_elf32_shdr {
    ll_elf32_word sh_name, sh_type;
    ll_elf32_word sh_flags;
    ll_elf32_addr sh_addr;
    ll_elf32_off sh_offset;
    ll_elf32_word sh_size;
    ll_elf32_word sh_link, sh_info;
    ll_elf32_word sh_addralign, sh_entsize;
} ll_elf32_shdr;

typedef struct ll_elf32_sym {
    ll_elf32_word st_name;
    ll_elf32_addr st_value;
    ll_elf32_word st_size;
    unsigned char st_info, st_other;
    ll_elf32_half st_shndx;
} ll_elf32_sym;

typedef struct ll_elf32_rel {
    ll_elf32_addr r_offset;
    ll_elf32_word r_info;
} ll_elf32_rel;

typedef struct ll_elf32_rela {
    ll_elf32_addr r_offset;
    ll_elf32_word r_info;
    ll_elf32_sword r_addend;
} ll_elf32_rela;

typedef uint16_t ll_elf64_half;
typedef uint32_t ll_elf64_word;
typedef int32_t ll_elf64_sword;
typedef uint64_t ll_elf64_xword;
typedef int64_t ll_elf64_sxword;
typedef uint64_t ll_elf64_addr;
typedef uint64_t ll_elf64_off;

typedef struct ll_elf64_ehdr {
    unsigned char e_ident[16];
    ll_elf64_half e_type, e_machine;
    ll_elf64_word e_version;
    ll_elf64_addr e_entry;
    ll_elf64_off e_phoff, e_shoff;
    ll_elf64_word e_flags;
    ll_elf64_half e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx;
} ll_elf64_ehdr;

typedef struct ll_elf64_phdr {
    ll_elf64_word p_type, p_flags;
    ll_elf64_off p_offset;
    ll_elf64_addr p_vaddr, p_paddr;
    ll_elf64_xword p_filesz, p_memsz, p_align;
} ll_elf64_phdr;

typedef struct ll_elf64_shdr {
    ll_elf64_word sh_name, sh_type;
    ll_elf64_xword sh_flags;
    ll_elf64_addr sh_addr;
    ll_elf64_off sh_offset;
    ll_elf64_xword sh_size;
    ll_elf64_word sh_link, sh_info;
    ll_elf64_xword sh_addralign, sh_entsize;
} ll_elf64_shdr;

typedef struct ll_elf64_sym {
    ll_elf64_word st_name;
    unsigned char st_info, st_other;
    ll_elf64_half st_shndx;
    ll_elf64_addr st_value;
    ll_elf64_xword st_size;
} ll_elf64_sym;

typedef struct ll_elf64_rel {
    ll_elf64_addr r_offset;
    ll_elf64_xword r_info;
} ll_elf64_rel;

typedef struct ll_elf64_rela {
    ll_elf64_addr r_offset;
    ll_elf64_xword r_info;
    ll_elf64_sxword r_addend;
} ll_elf64_rela;

#define LL_ELF_EI_CLASS 4
#define LL_ELF_EI_DATA 5
#define LL_ELF_EI_MAG0 0
#define LL_ELF_EI_MAG1 1
#define LL_ELF_EI_MAG2 2
#define LL_ELF_EI_MAG3 3
#define LL_ELF_EI_NIDENT (16)
#define LL_ELF_EI_VERSION 6
#define LL_ELF_ELFCLASS32 1
#define LL_ELF_ELFCLASS64 2
#define LL_ELF_ELFDATA2LSB 1
#define LL_ELF_ELFDATA2MSB 2
#define LL_ELF_ELFMAG "\177ELF"
#define LL_ELF_ELFMAG0 0x7f
#define LL_ELF_ELFMAG1 'E'
#define LL_ELF_ELFMAG2 'L'
#define LL_ELF_ELFMAG3 'F'
#define LL_ELF_EM_386 3
#define LL_ELF_EM_AARCH64 183
#define LL_ELF_EM_AMD64 62
#define LL_ELF_EM_ARM 40
#define LL_ELF_EM_IA_64 50
#define LL_ELF_EM_LOONGARCH 258
#define LL_ELF_EM_PPC 20
#define LL_ELF_EM_PPC64 21
#define LL_ELF_EM_RISCV 243
#define LL_ELF_EM_S390 22
#define LL_ELF_EM_SPARC 2
#define LL_ELF_EM_SPARC32PLUS 18
#define LL_ELF_EM_X86_64 62
#define LL_ELF_ET_REL 1
#define LL_ELF_EV_CURRENT 1
#define LL_ELF_R_PPC_ADDR16_HA 6
#define LL_ELF_R_PPC_ADDR16_HI 5
#define LL_ELF_R_PPC_ADDR16_LO 4
#define LL_ELF_R_PPC_ADDR32 1
#define LL_ELF_R_PPC_PLTREL24 18
#define LL_ELF_R_PPC_REL16_HA 252
#define LL_ELF_R_PPC_REL16_HI 251
#define LL_ELF_R_PPC_REL16_LO 250
#define LL_ELF_R_PPC_REL24 10
#define LL_ELF_R_PPC_REL32 26
#define LL_ELF_R_RISCV_32 1
#define LL_ELF_R_RISCV_32_PCREL 57
#define LL_ELF_R_RISCV_64 2
#define LL_ELF_R_RISCV_ADD16 34
#define LL_ELF_R_RISCV_ADD32 35
#define LL_ELF_R_RISCV_ADD64 36
#define LL_ELF_R_RISCV_ADD8 33
#define LL_ELF_R_RISCV_ALIGN 43
#define LL_ELF_R_RISCV_BRANCH 16
#define LL_ELF_R_RISCV_CALL 18
#define LL_ELF_R_RISCV_CALL_PLT 19
#define LL_ELF_R_RISCV_COPY 4
#define LL_ELF_R_RISCV_GOT_HI20 20
#define LL_ELF_R_RISCV_HI20 26
#define LL_ELF_R_RISCV_JAL 17
#define LL_ELF_R_RISCV_JUMP_SLOT 5
#define LL_ELF_R_RISCV_LO12_I 27
#define LL_ELF_R_RISCV_LO12_S 28
#define LL_ELF_R_RISCV_NONE 0
#define LL_ELF_R_RISCV_PCREL_HI20 23
#define LL_ELF_R_RISCV_PCREL_LO12_I 24
#define LL_ELF_R_RISCV_PCREL_LO12_S 25
#define LL_ELF_R_RISCV_RELATIVE 3
#define LL_ELF_R_RISCV_RELAX 51
#define LL_ELF_R_RISCV_RVC_BRANCH 44
#define LL_ELF_R_RISCV_RVC_JUMP 45
#define LL_ELF_R_RISCV_SET16 55
#define LL_ELF_R_RISCV_SET32 56
#define LL_ELF_R_RISCV_SET6 53
#define LL_ELF_R_RISCV_SET8 54
#define LL_ELF_R_RISCV_SUB16 38
#define LL_ELF_R_RISCV_SUB32 39
#define LL_ELF_R_RISCV_SUB6 52
#define LL_ELF_R_RISCV_SUB64 40
#define LL_ELF_R_RISCV_SUB8 37
#define LL_ELF_R_RISCV_TLS_DTPMOD32 6
#define LL_ELF_R_RISCV_TLS_DTPMOD64 7
#define LL_ELF_R_RISCV_TLS_DTPREL32 8
#define LL_ELF_R_RISCV_TLS_DTPREL64 9
#define LL_ELF_R_RISCV_TLS_GD_HI20 22
#define LL_ELF_R_RISCV_TLS_GOT_HI20 21
#define LL_ELF_R_RISCV_TLS_TPREL32 10
#define LL_ELF_R_RISCV_TLS_TPREL64 11
#define LL_ELF_R_RISCV_TPREL_LO12_S 31
#define LL_ELF_SELFMAG 4
#define LL_ELF_SHF_ALLOC (1 << 1)
#define LL_ELF_SHF_EXECINSTR (1 << 2)
#define LL_ELF_SHF_WRITE (1 << 0)
#define LL_ELF_SHN_ABS 0xfff1
#define LL_ELF_SHN_COMMON 0xfff2
#define LL_ELF_SHN_LORESERVE 0xff00
#define LL_ELF_SHN_UNDEF 0
#define LL_ELF_SHN_XINDEX 0xffff
#define LL_ELF_SHT_FINI_ARRAY 15
#define LL_ELF_SHT_INIT_ARRAY 14
#define LL_ELF_SHT_NOBITS 8
#define LL_ELF_SHT_NULL 0
#define LL_ELF_SHT_PROGBITS 1
#define LL_ELF_SHT_REL 9
#define LL_ELF_SHT_RELA 4
#define LL_ELF_SHT_STRTAB 3
#define LL_ELF_SHT_SYMTAB 2
#define LL_ELF_SHT_SYMTAB_SHNDX 18
#define LL_ELF_STB_GLOBAL 1
#define LL_ELF_STB_LOCAL 0
#define LL_ELF_STB_WEAK 2
#define LL_ELF_STT_FILE 4
#define LL_ELF_STT_FUNC 2
#define LL_ELF_STT_NOTYPE 0
#define LL_ELF_STT_OBJECT 1
#define LL_ELF_STT_SECTION 3
#define LL_ELF_STV_HIDDEN 2

#define LL_ELF32_ST_BIND(value) ((value) >> 4)
#define LL_ELF32_ST_TYPE(value) ((value) & 0xf)
#define LL_ELF32_ST_INFO(bind, type) (((bind) << 4) + ((type) & 0xf))
#define LL_ELF32_ST_VISIBILITY(value) ((value) & 0x3)
#define LL_ELF64_ST_BIND(value) LL_ELF32_ST_BIND(value)
#define LL_ELF64_ST_TYPE(value) LL_ELF32_ST_TYPE(value)
#define LL_ELF64_ST_INFO(bind, type) LL_ELF32_ST_INFO(bind, type)
#define LL_ELF64_ST_VISIBILITY(value) LL_ELF32_ST_VISIBILITY(value)
#define LL_ELF32_R_SYM(value) ((value) >> 8)
#define LL_ELF32_R_TYPE(value) ((value) & 0xff)
#define LL_ELF32_R_INFO(symbol, type) (((symbol) << 8) + (type))
#define LL_ELF64_R_SYM(value) ((value) >> 32)
#define LL_ELF64_R_TYPE(value) ((value) & 0xffffffff)
#define LL_ELF64_R_INFO(symbol, type) ((((ll_elf64_xword)(symbol)) << 32) + (type))
