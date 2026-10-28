/* Mach-O file records used by the 64-bit backends. No host SDK dependencies.
 * These are storage declarations, not a byte-swapping or execution interface.
 * The existing backend reads native-endian records; supported native Mach-O
 * targets are little-endian x86_64 and AArch64. Relocation bitfield layout is
 * checked against the SDK and independent wire bytes by the unit controls.
 */
#pragma once

#include <stddef.h>
#include <stdint.h>

typedef struct ll_macho_header_64 {
    uint32_t magic;
    int32_t cputype, cpusubtype;
    uint32_t filetype, ncmds, sizeofcmds, flags, reserved;
} ll_macho_header_64;

typedef struct ll_macho_load_command {
    uint32_t cmd, cmdsize;
} ll_macho_load_command;

typedef struct ll_macho_segment_command_64 {
    uint32_t cmd, cmdsize;
    char segname[16];
    uint64_t vmaddr, vmsize, fileoff, filesize;
    int32_t maxprot, initprot;
    uint32_t nsects, flags;
} ll_macho_segment_command_64;

typedef struct ll_macho_section_64 {
    char sectname[16], segname[16];
    uint64_t addr, size;
    uint32_t offset, align, reloff, nreloc, flags;
    uint32_t reserved1, reserved2, reserved3;
} ll_macho_section_64;

typedef struct ll_macho_symtab_command {
    uint32_t cmd, cmdsize, symoff, nsyms, stroff, strsize;
} ll_macho_symtab_command;

typedef struct ll_macho_dysymtab_command {
    uint32_t cmd, cmdsize;
    uint32_t ilocalsym, nlocalsym, iextdefsym, nextdefsym;
    uint32_t iundefsym, nundefsym, tocoff, ntoc, modtaboff, nmodtab;
    uint32_t extrefsymoff, nextrefsyms, indirectsymoff, nindirectsyms;
    uint32_t extreloff, nextrel, locreloff, nlocrel;
} ll_macho_dysymtab_command;

typedef struct ll_macho_nlist_64 {
    union { uint32_t n_strx; } n_un;
    uint8_t n_type, n_sect;
    uint16_t n_desc;
    uint64_t n_value;
} ll_macho_nlist_64;

typedef struct ll_macho_relocation_info {
    int32_t r_address;
    uint32_t r_symbolnum:24, r_pcrel:1, r_length:2, r_extern:1, r_type:4;
} ll_macho_relocation_info;

/* Retained for private compatibility; the 64-bit backends do not consume
 * scattered records. Match the native SDK's endian-dependent declaration. */
typedef struct ll_macho_scattered_relocation_info {
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint32_t r_scattered:1, r_pcrel:1, r_length:2, r_type:4, r_address:24;
#else
    uint32_t r_address:24, r_type:4, r_length:2, r_pcrel:1, r_scattered:1;
#endif
    int32_t r_value;
} ll_macho_scattered_relocation_info;

_Static_assert(sizeof(ll_macho_header_64) == 32, "Mach-O header size");
_Static_assert(sizeof(ll_macho_load_command) == 8, "Mach-O command size");
_Static_assert(sizeof(ll_macho_segment_command_64) == 72, "Mach-O segment size");
_Static_assert(sizeof(ll_macho_section_64) == 80, "Mach-O section size");
_Static_assert(sizeof(ll_macho_symtab_command) == 24, "Mach-O symtab size");
_Static_assert(sizeof(ll_macho_dysymtab_command) == 80, "Mach-O dysymtab size");
_Static_assert(sizeof(ll_macho_nlist_64) == 16 &&
               offsetof(ll_macho_nlist_64, n_value) == 8, "Mach-O symbol layout");
_Static_assert(sizeof(ll_macho_relocation_info) == 8, "Mach-O relocation size");
_Static_assert(sizeof(ll_macho_scattered_relocation_info) == 8,
               "Mach-O scattered relocation size");

#define LL_MACHO_MH_MAGIC_64 0xfeedfacf
#define LL_MACHO_MH_OBJECT 0x1
#define LL_MACHO_LC_SEGMENT 0x1
#define LL_MACHO_LC_SYMTAB 0x2
#define LL_MACHO_LC_DYSYMTAB 0xb
#define LL_MACHO_LC_SEGMENT_64 0x19
#define LL_MACHO_SECTION_TYPE 0xff
#define LL_MACHO_S_ZEROFILL 0x1
#define LL_MACHO_S_SYMBOL_STUBS 0x8
#define LL_MACHO_S_MOD_INIT_FUNC_POINTERS 0x9
#define LL_MACHO_S_MOD_TERM_FUNC_POINTERS 0xa
#define LL_MACHO_S_GB_ZEROFILL 0xc
#define LL_MACHO_N_STAB 0xe0
#define LL_MACHO_N_TYPE 0x0e
#define LL_MACHO_N_EXT 0x01
#define LL_MACHO_N_UNDF 0x0
#define LL_MACHO_N_SECT 0x0e
#define LL_MACHO_N_WEAK_DEF 0x0080
#define LL_MACHO_NO_SECT 0
#define LL_MACHO_INDIRECT_SYMBOL_LOCAL 0x80000000
#define LL_MACHO_INDIRECT_SYMBOL_ABS 0x40000000
#define LL_MACHO_X86_64_RELOC_UNSIGNED 0
#define LL_MACHO_X86_64_RELOC_SIGNED 1
#define LL_MACHO_X86_64_RELOC_BRANCH 2
#define LL_MACHO_X86_64_RELOC_GOT_LOAD 3
#define LL_MACHO_X86_64_RELOC_GOT 4
#define LL_MACHO_X86_64_RELOC_SUBTRACTOR 5
#define LL_MACHO_X86_64_RELOC_SIGNED_1 6
#define LL_MACHO_X86_64_RELOC_SIGNED_2 7
#define LL_MACHO_X86_64_RELOC_SIGNED_4 8
#define LL_MACHO_X86_64_RELOC_TLV 9
#define LL_MACHO_ARM64_RELOC_UNSIGNED 0
#define LL_MACHO_ARM64_RELOC_SUBTRACTOR 1
#define LL_MACHO_ARM64_RELOC_BRANCH26 2
#define LL_MACHO_ARM64_RELOC_PAGE21 3
#define LL_MACHO_ARM64_RELOC_PAGEOFF12 4
#define LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGE21 5
#define LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGEOFF12 6
#define LL_MACHO_ARM64_RELOC_POINTER_TO_GOT 7
#define LL_MACHO_ARM64_RELOC_TLVP_LOAD_PAGE21 8
#define LL_MACHO_ARM64_RELOC_TLVP_LOAD_PAGEOFF12 9
#define LL_MACHO_ARM64_RELOC_ADDEND 10
#define LL_MACHO_ARM64_RELOC_AUTHENTICATED_POINTER 11
