/* Independent file offsets and optional real Apple SDK equivalence.
 * Also compiles freestanding for foreign compiler targets; no host shim.
 */
#include "liblink/macho.h"

#ifdef LL_MACHO_COMPARE_SDK
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <mach-o/arm64/reloc.h>
#include <mach-o/x86_64/reloc.h>
#define RECORD(name, sdk, bytes) \
    _Static_assert(sizeof(ll_macho_##name) == (bytes), "wire size"); \
    _Static_assert(sizeof(ll_macho_##name) == sizeof(struct sdk), "SDK size"); \
    _Static_assert(_Alignof(ll_macho_##name) == _Alignof(struct sdk), "SDK alignment")
#define FIELD(name, sdk, field, offset) \
    _Static_assert(offsetof(ll_macho_##name, field) == (offset), "wire offset"); \
    _Static_assert(offsetof(ll_macho_##name, field) == offsetof(struct sdk, field), "SDK offset")
#define CONSTANT(name) _Static_assert(LL_MACHO_##name == name, "SDK constant")
#else
#define RECORD(name, sdk, bytes) \
    _Static_assert(sizeof(ll_macho_##name) == (bytes), "wire size")
#define FIELD(name, sdk, field, offset) \
    _Static_assert(offsetof(ll_macho_##name, field) == (offset), "wire offset")
#endif

RECORD(header_64, mach_header_64, 32);
FIELD(header_64, mach_header_64, magic, 0);
FIELD(header_64, mach_header_64, cputype, 4);
FIELD(header_64, mach_header_64, cpusubtype, 8);
FIELD(header_64, mach_header_64, filetype, 12);
FIELD(header_64, mach_header_64, ncmds, 16);
FIELD(header_64, mach_header_64, sizeofcmds, 20);
FIELD(header_64, mach_header_64, flags, 24);
FIELD(header_64, mach_header_64, reserved, 28);
RECORD(load_command, load_command, 8);
FIELD(load_command, load_command, cmd, 0);
FIELD(load_command, load_command, cmdsize, 4);
RECORD(segment_command_64, segment_command_64, 72);
FIELD(segment_command_64, segment_command_64, cmd, 0);
FIELD(segment_command_64, segment_command_64, cmdsize, 4);
FIELD(segment_command_64, segment_command_64, segname, 8);
FIELD(segment_command_64, segment_command_64, vmaddr, 24);
FIELD(segment_command_64, segment_command_64, vmsize, 32);
FIELD(segment_command_64, segment_command_64, fileoff, 40);
FIELD(segment_command_64, segment_command_64, filesize, 48);
FIELD(segment_command_64, segment_command_64, maxprot, 56);
FIELD(segment_command_64, segment_command_64, initprot, 60);
FIELD(segment_command_64, segment_command_64, nsects, 64);
FIELD(segment_command_64, segment_command_64, flags, 68);
RECORD(section_64, section_64, 80);
FIELD(section_64, section_64, sectname, 0);
FIELD(section_64, section_64, segname, 16);
FIELD(section_64, section_64, addr, 32);
FIELD(section_64, section_64, size, 40);
FIELD(section_64, section_64, offset, 48);
FIELD(section_64, section_64, align, 52);
FIELD(section_64, section_64, reloff, 56);
FIELD(section_64, section_64, nreloc, 60);
FIELD(section_64, section_64, flags, 64);
FIELD(section_64, section_64, reserved1, 68);
FIELD(section_64, section_64, reserved2, 72);
FIELD(section_64, section_64, reserved3, 76);
RECORD(symtab_command, symtab_command, 24);
FIELD(symtab_command, symtab_command, cmd, 0);
FIELD(symtab_command, symtab_command, cmdsize, 4);
FIELD(symtab_command, symtab_command, symoff, 8);
FIELD(symtab_command, symtab_command, nsyms, 12);
FIELD(symtab_command, symtab_command, stroff, 16);
FIELD(symtab_command, symtab_command, strsize, 20);
RECORD(dysymtab_command, dysymtab_command, 80);
FIELD(dysymtab_command, dysymtab_command, cmd, 0);
FIELD(dysymtab_command, dysymtab_command, cmdsize, 4);
FIELD(dysymtab_command, dysymtab_command, ilocalsym, 8);
FIELD(dysymtab_command, dysymtab_command, nlocalsym, 12);
FIELD(dysymtab_command, dysymtab_command, iextdefsym, 16);
FIELD(dysymtab_command, dysymtab_command, nextdefsym, 20);
FIELD(dysymtab_command, dysymtab_command, iundefsym, 24);
FIELD(dysymtab_command, dysymtab_command, nundefsym, 28);
FIELD(dysymtab_command, dysymtab_command, tocoff, 32);
FIELD(dysymtab_command, dysymtab_command, ntoc, 36);
FIELD(dysymtab_command, dysymtab_command, modtaboff, 40);
FIELD(dysymtab_command, dysymtab_command, nmodtab, 44);
FIELD(dysymtab_command, dysymtab_command, extrefsymoff, 48);
FIELD(dysymtab_command, dysymtab_command, nextrefsyms, 52);
FIELD(dysymtab_command, dysymtab_command, indirectsymoff, 56);
FIELD(dysymtab_command, dysymtab_command, nindirectsyms, 60);
FIELD(dysymtab_command, dysymtab_command, extreloff, 64);
FIELD(dysymtab_command, dysymtab_command, nextrel, 68);
FIELD(dysymtab_command, dysymtab_command, locreloff, 72);
FIELD(dysymtab_command, dysymtab_command, nlocrel, 76);
RECORD(nlist_64, nlist_64, 16);
FIELD(nlist_64, nlist_64, n_un, 0);
FIELD(nlist_64, nlist_64, n_un.n_strx, 0);
FIELD(nlist_64, nlist_64, n_type, 4);
FIELD(nlist_64, nlist_64, n_sect, 5);
FIELD(nlist_64, nlist_64, n_desc, 6);
FIELD(nlist_64, nlist_64, n_value, 8);
RECORD(relocation_info, relocation_info, 8);
FIELD(relocation_info, relocation_info, r_address, 0);
RECORD(scattered_relocation_info, scattered_relocation_info, 8);
FIELD(scattered_relocation_info, scattered_relocation_info, r_value, 4);

#ifdef LL_MACHO_COMPARE_SDK
CONSTANT(MH_MAGIC_64);
CONSTANT(MH_OBJECT);
CONSTANT(LC_SEGMENT);
CONSTANT(LC_SYMTAB);
CONSTANT(LC_DYSYMTAB);
CONSTANT(LC_SEGMENT_64);
CONSTANT(SECTION_TYPE);
CONSTANT(S_ZEROFILL);
CONSTANT(S_SYMBOL_STUBS);
CONSTANT(S_MOD_INIT_FUNC_POINTERS);
CONSTANT(S_MOD_TERM_FUNC_POINTERS);
CONSTANT(S_GB_ZEROFILL);
CONSTANT(N_STAB);
CONSTANT(N_TYPE);
CONSTANT(N_EXT);
CONSTANT(N_UNDF);
CONSTANT(N_SECT);
CONSTANT(N_WEAK_DEF);
CONSTANT(NO_SECT);
CONSTANT(INDIRECT_SYMBOL_LOCAL);
CONSTANT(INDIRECT_SYMBOL_ABS);
CONSTANT(X86_64_RELOC_UNSIGNED);
CONSTANT(X86_64_RELOC_SIGNED);
CONSTANT(X86_64_RELOC_BRANCH);
CONSTANT(X86_64_RELOC_GOT_LOAD);
CONSTANT(X86_64_RELOC_GOT);
CONSTANT(X86_64_RELOC_SUBTRACTOR);
CONSTANT(X86_64_RELOC_SIGNED_1);
CONSTANT(X86_64_RELOC_SIGNED_2);
CONSTANT(X86_64_RELOC_SIGNED_4);
CONSTANT(X86_64_RELOC_TLV);
CONSTANT(ARM64_RELOC_UNSIGNED);
CONSTANT(ARM64_RELOC_SUBTRACTOR);
CONSTANT(ARM64_RELOC_BRANCH26);
CONSTANT(ARM64_RELOC_PAGE21);
CONSTANT(ARM64_RELOC_PAGEOFF12);
CONSTANT(ARM64_RELOC_GOT_LOAD_PAGE21);
CONSTANT(ARM64_RELOC_GOT_LOAD_PAGEOFF12);
CONSTANT(ARM64_RELOC_POINTER_TO_GOT);
CONSTANT(ARM64_RELOC_TLVP_LOAD_PAGE21);
CONSTANT(ARM64_RELOC_TLVP_LOAD_PAGEOFF12);
CONSTANT(ARM64_RELOC_ADDEND);
CONSTANT(ARM64_RELOC_AUTHENTICATED_POINTER);
#endif

/* Independent little-endian wire values. Each bit is tested separately so
 * two misplaced fields cannot conceal one another in an all-ones record. */
static int relocation_bits(void)
{
    for (unsigned bit = 0; bit < 32; ++bit) {
        uint32_t word = UINT32_C(1) << bit;
        ll_macho_relocation_info rel = {0};
        rel.r_address = -123;
        rel.r_symbolnum = word & UINT32_C(0xffffff);
        rel.r_pcrel = (word >> 24) & 1;
        rel.r_length = (word >> 25) & 3;
        rel.r_extern = (word >> 27) & 1;
        rel.r_type = word >> 28;
        const unsigned char *bytes = (const unsigned char *)&rel;
        const uint32_t address = (uint32_t)-123;
        for (unsigned i = 0; i < 4; ++i) {
            if (bytes[i] != ((address >> (i * 8)) & 255) ||
                bytes[i + 4] != ((word >> (i * 8)) & 255)) return 1;
        }
#ifdef LL_MACHO_COMPARE_SDK
        struct relocation_info sdk = {0};
        sdk.r_address = rel.r_address;
        sdk.r_symbolnum = rel.r_symbolnum;
        sdk.r_pcrel = rel.r_pcrel;
        sdk.r_length = rel.r_length;
        sdk.r_extern = rel.r_extern;
        sdk.r_type = rel.r_type;
        const unsigned char *sdk_bytes = (const unsigned char *)&sdk;
        for (unsigned i = 0; i < sizeof(rel); ++i)
            if (bytes[i] != sdk_bytes[i]) return 2;
#endif
        ll_macho_scattered_relocation_info scattered = {0};
        scattered.r_address = word & UINT32_C(0xffffff);
        scattered.r_type = (word >> 24) & 15;
        scattered.r_length = (word >> 28) & 3;
        scattered.r_pcrel = (word >> 30) & 1;
        scattered.r_scattered = word >> 31;
        scattered.r_value = -123;
        bytes = (const unsigned char *)&scattered;
        for (unsigned i = 0; i < 4; ++i) {
            if (bytes[i] != ((word >> (i * 8)) & 255) ||
                bytes[i + 4] != ((address >> (i * 8)) & 255)) return 3;
        }
#ifdef LL_MACHO_COMPARE_SDK
        struct scattered_relocation_info sdk_scattered = {0};
        sdk_scattered.r_address = scattered.r_address;
        sdk_scattered.r_type = scattered.r_type;
        sdk_scattered.r_length = scattered.r_length;
        sdk_scattered.r_pcrel = scattered.r_pcrel;
        sdk_scattered.r_scattered = scattered.r_scattered;
        sdk_scattered.r_value = scattered.r_value;
        sdk_bytes = (const unsigned char *)&sdk_scattered;
        for (unsigned i = 0; i < sizeof(scattered); ++i)
            if (bytes[i] != sdk_bytes[i]) return 4;
#endif
    }
    return 0;
}

int main(void)
{
    return relocation_bits();
}
