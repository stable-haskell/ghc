/* Independent COFF wire offsets, plus real Windows SDK comparisons when
 * selected. No Windows configuration or service substitutes are provided. */
#ifdef LL_COFF_COMPARE_SDK
#include <windows.h>
#endif

/* The installed header must establish its own ABI packing and restore the
 * caller's state. Also exercise the metadata header without any host SDK. */
#pragma pack(push, 1)
#include "liblink/coff.h"
struct coff_pack_restore_probe { uint8_t byte; uint32_t word; };
#pragma pack(pop)
_Static_assert(offsetof(struct coff_pack_restore_probe, word) == 1,
               "COFF header must restore caller packing");
#include "linker/PEi386Types.h"

#ifdef LL_COFF_COMPARE_SDK
#define RECORD(name, sdk, bytes, alignment) \
    _Static_assert(sizeof(ll_coff_##name) == (bytes), "wire size"); \
    _Static_assert(_Alignof(ll_coff_##name) == (alignment), "wire alignment"); \
    _Static_assert(sizeof(ll_coff_##name) == sizeof(sdk), "SDK size"); \
    _Static_assert(_Alignof(ll_coff_##name) == _Alignof(sdk), "SDK alignment")
#define FIELD(name, sdk, field, offset) \
    _Static_assert(offsetof(ll_coff_##name, field) == (offset), "wire offset"); \
    _Static_assert(offsetof(ll_coff_##name, field) == offsetof(sdk, field), "SDK offset")
#define CONSTANT(name) _Static_assert(LL_COFF_##name == name, "SDK constant")
#else
#define RECORD(name, sdk, bytes, alignment) \
    _Static_assert(sizeof(ll_coff_##name) == (bytes), "wire size"); \
    _Static_assert(_Alignof(ll_coff_##name) == (alignment), "wire alignment")
#define FIELD(name, sdk, field, offset) \
    _Static_assert(offsetof(ll_coff_##name, field) == (offset), "wire offset")
#endif

RECORD(file_header, IMAGE_FILE_HEADER, 20, 4);
FIELD(file_header, IMAGE_FILE_HEADER, Machine, 0);
FIELD(file_header, IMAGE_FILE_HEADER, NumberOfSections, 2);
FIELD(file_header, IMAGE_FILE_HEADER, TimeDateStamp, 4);
FIELD(file_header, IMAGE_FILE_HEADER, PointerToSymbolTable, 8);
FIELD(file_header, IMAGE_FILE_HEADER, NumberOfSymbols, 12);
FIELD(file_header, IMAGE_FILE_HEADER, SizeOfOptionalHeader, 16);
FIELD(file_header, IMAGE_FILE_HEADER, Characteristics, 18);
RECORD(class_id, CLSID, 16, 4);
FIELD(class_id, CLSID, Data1, 0);
FIELD(class_id, CLSID, Data2, 4);
FIELD(class_id, CLSID, Data3, 6);
FIELD(class_id, CLSID, Data4, 8);
RECORD(anon_header, ANON_OBJECT_HEADER, 32, 4);
FIELD(anon_header, ANON_OBJECT_HEADER, Sig1, 0);
FIELD(anon_header, ANON_OBJECT_HEADER, Sig2, 2);
FIELD(anon_header, ANON_OBJECT_HEADER, Version, 4);
FIELD(anon_header, ANON_OBJECT_HEADER, Machine, 6);
FIELD(anon_header, ANON_OBJECT_HEADER, TimeDateStamp, 8);
FIELD(anon_header, ANON_OBJECT_HEADER, ClassID, 12);
FIELD(anon_header, ANON_OBJECT_HEADER, SizeOfData, 28);
RECORD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, 56, 4);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, Sig1, 0);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, Sig2, 2);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, Version, 4);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, Machine, 6);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, TimeDateStamp, 8);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, ClassID, 12);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, SizeOfData, 28);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, Flags, 32);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, MetaDataSize, 36);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, MetaDataOffset, 40);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, NumberOfSections, 44);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, PointerToSymbolTable, 48);
FIELD(bigobj_header, ANON_OBJECT_HEADER_BIGOBJ, NumberOfSymbols, 52);
RECORD(section, IMAGE_SECTION_HEADER, 40, 4);
FIELD(section, IMAGE_SECTION_HEADER, Name, 0);
FIELD(section, IMAGE_SECTION_HEADER, Misc, 8);
FIELD(section, IMAGE_SECTION_HEADER, Misc.PhysicalAddress, 8);
FIELD(section, IMAGE_SECTION_HEADER, Misc.VirtualSize, 8);
FIELD(section, IMAGE_SECTION_HEADER, VirtualAddress, 12);
FIELD(section, IMAGE_SECTION_HEADER, SizeOfRawData, 16);
FIELD(section, IMAGE_SECTION_HEADER, PointerToRawData, 20);
FIELD(section, IMAGE_SECTION_HEADER, PointerToRelocations, 24);
FIELD(section, IMAGE_SECTION_HEADER, PointerToLinenumbers, 28);
FIELD(section, IMAGE_SECTION_HEADER, NumberOfRelocations, 32);
FIELD(section, IMAGE_SECTION_HEADER, NumberOfLinenumbers, 34);
FIELD(section, IMAGE_SECTION_HEADER, Characteristics, 36);
RECORD(symbol, IMAGE_SYMBOL, 18, 2);
FIELD(symbol, IMAGE_SYMBOL, N, 0);
FIELD(symbol, IMAGE_SYMBOL, N.ShortName, 0);
FIELD(symbol, IMAGE_SYMBOL, N.Name.Short, 0);
FIELD(symbol, IMAGE_SYMBOL, N.Name.Long, 4);
FIELD(symbol, IMAGE_SYMBOL, N.LongName, 0);
FIELD(symbol, IMAGE_SYMBOL, Value, 8);
FIELD(symbol, IMAGE_SYMBOL, SectionNumber, 12);
FIELD(symbol, IMAGE_SYMBOL, Type, 14);
FIELD(symbol, IMAGE_SYMBOL, StorageClass, 16);
FIELD(symbol, IMAGE_SYMBOL, NumberOfAuxSymbols, 17);
RECORD(symbol_ex, IMAGE_SYMBOL_EX, 20, 2);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, N, 0);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, N.ShortName, 0);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, N.Name.Short, 0);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, N.Name.Long, 4);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, N.LongName, 0);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, Value, 8);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, SectionNumber, 12);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, Type, 16);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, StorageClass, 18);
FIELD(symbol_ex, IMAGE_SYMBOL_EX, NumberOfAuxSymbols, 19);
RECORD(relocation, IMAGE_RELOCATION, 10, 2);
FIELD(relocation, IMAGE_RELOCATION, VirtualAddress, 0);
FIELD(relocation, IMAGE_RELOCATION, RelocCount, 0);
FIELD(relocation, IMAGE_RELOCATION, SymbolTableIndex, 4);
FIELD(relocation, IMAGE_RELOCATION, Type, 8);
RECORD(import_header, IMPORT_OBJECT_HEADER, 20, 4);
FIELD(import_header, IMPORT_OBJECT_HEADER, Sig1, 0);
FIELD(import_header, IMPORT_OBJECT_HEADER, Sig2, 2);
FIELD(import_header, IMPORT_OBJECT_HEADER, Version, 4);
FIELD(import_header, IMPORT_OBJECT_HEADER, Machine, 6);
FIELD(import_header, IMPORT_OBJECT_HEADER, TimeDateStamp, 8);
FIELD(import_header, IMPORT_OBJECT_HEADER, SizeOfData, 12);
FIELD(import_header, IMPORT_OBJECT_HEADER, Ordinal, 16);
FIELD(import_header, IMPORT_OBJECT_HEADER, Hint, 16);
_Static_assert(sizeof(COFF_symbol) == 20 && _Alignof(COFF_symbol) == 2,
               "copied symbol union retains its native stride and alignment");

#ifdef LL_COFF_COMPARE_SDK
CONSTANT(IMAGE_FILE_BYTES_REVERSED_HI);
CONSTANT(IMAGE_FILE_DLL);
CONSTANT(IMAGE_FILE_EXECUTABLE_IMAGE);
CONSTANT(IMAGE_FILE_MACHINE_AMD64);
CONSTANT(IMAGE_FILE_MACHINE_ARM64);
CONSTANT(IMAGE_FILE_MACHINE_UNKNOWN);
CONSTANT(IMAGE_FILE_SYSTEM);
#ifdef IMAGE_REL_ARM64_ABSOLUTE
CONSTANT(IMAGE_REL_ARM64_ABSOLUTE);
CONSTANT(IMAGE_REL_ARM64_ADDR32);
CONSTANT(IMAGE_REL_ARM64_ADDR32NB);
CONSTANT(IMAGE_REL_ARM64_BRANCH26);
CONSTANT(IMAGE_REL_ARM64_PAGEBASE_REL21);
CONSTANT(IMAGE_REL_ARM64_REL21);
CONSTANT(IMAGE_REL_ARM64_PAGEOFFSET_12A);
CONSTANT(IMAGE_REL_ARM64_PAGEOFFSET_12L);
CONSTANT(IMAGE_REL_ARM64_SECREL);
CONSTANT(IMAGE_REL_ARM64_SECREL_LOW12A);
CONSTANT(IMAGE_REL_ARM64_SECREL_HIGH12A);
CONSTANT(IMAGE_REL_ARM64_SECREL_LOW12L);
CONSTANT(IMAGE_REL_ARM64_TOKEN);
CONSTANT(IMAGE_REL_ARM64_SECTION);
CONSTANT(IMAGE_REL_ARM64_ADDR64);
CONSTANT(IMAGE_REL_ARM64_BRANCH19);
CONSTANT(IMAGE_REL_ARM64_BRANCH14);
CONSTANT(IMAGE_REL_ARM64_REL32);
#endif
CONSTANT(IMAGE_SCN_ALIGN_1024BYTES);
CONSTANT(IMAGE_SCN_ALIGN_128BYTES);
CONSTANT(IMAGE_SCN_ALIGN_16BYTES);
CONSTANT(IMAGE_SCN_ALIGN_1BYTES);
CONSTANT(IMAGE_SCN_ALIGN_2048BYTES);
CONSTANT(IMAGE_SCN_ALIGN_256BYTES);
CONSTANT(IMAGE_SCN_ALIGN_2BYTES);
CONSTANT(IMAGE_SCN_ALIGN_32BYTES);
CONSTANT(IMAGE_SCN_ALIGN_4096BYTES);
CONSTANT(IMAGE_SCN_ALIGN_4BYTES);
CONSTANT(IMAGE_SCN_ALIGN_512BYTES);
CONSTANT(IMAGE_SCN_ALIGN_64BYTES);
CONSTANT(IMAGE_SCN_ALIGN_8192BYTES);
CONSTANT(IMAGE_SCN_ALIGN_8BYTES);
CONSTANT(IMAGE_SCN_CNT_CODE);
CONSTANT(IMAGE_SCN_CNT_UNINITIALIZED_DATA);
CONSTANT(IMAGE_SCN_LNK_COMDAT);
CONSTANT(IMAGE_SCN_LNK_NRELOC_OVFL);
CONSTANT(IMAGE_SCN_LNK_REMOVE);
CONSTANT(IMAGE_SCN_MEM_DISCARDABLE);
CONSTANT(IMAGE_SCN_MEM_WRITE);
CONSTANT(IMAGE_SYM_ABSOLUTE);
CONSTANT(IMAGE_SYM_CLASS_EXTERNAL);
CONSTANT(IMAGE_SYM_CLASS_SECTION);
CONSTANT(IMAGE_SYM_CLASS_STATIC);
CONSTANT(IMAGE_SYM_CLASS_WEAK_EXTERNAL);
CONSTANT(IMAGE_SYM_DEBUG);
CONSTANT(IMAGE_SYM_UNDEFINED);
CONSTANT(IMPORT_OBJECT_CODE);
CONSTANT(IMPORT_OBJECT_HDR_SIG2);
#endif

/* Byte copies are intentional: arbitrary file offsets do not promise the
 * alignment of the C record type. No unaligned scalar pointer is formed. */
static void copy_bytes(void *destination, const void *source, size_t length)
{
    unsigned char *dst = destination;
    const unsigned char *src = source;
    for (size_t i = 0; i < length; ++i) dst[i] = src[i];
}

static int packed_arrays(void)
{
    ll_coff_symbol symbols[2] = {0};
    ll_coff_relocation relocations[2] = {0};
    symbols[1].Value = UINT32_C(0x12345678);
    symbols[1].SectionNumber = -2;
    relocations[1].SymbolTableIndex = UINT32_C(0x9abcdef0);
    relocations[1].Type = UINT16_C(0x1234);
    const unsigned char *bytes = (const unsigned char *)symbols;
    if (bytes[18 + 8] != 0x78 || bytes[18 + 11] != 0x12 ||
        bytes[18 + 12] != 0xfe || bytes[18 + 13] != 0xff) return 1;
    bytes = (const unsigned char *)relocations;
    if (bytes[10 + 4] != 0xf0 || bytes[10 + 7] != 0x9a ||
        bytes[10 + 8] != 0x34 || bytes[10 + 9] != 0x12) return 2;

    unsigned char odd_wire[1 + sizeof(symbols) + sizeof(relocations)] = {0};
    copy_bytes(odd_wire + 1, symbols, sizeof(symbols));
    copy_bytes(odd_wire + 1 + sizeof(symbols), relocations, sizeof(relocations));
    ll_coff_symbol decoded_symbol = {0};
    ll_coff_relocation decoded_relocation = {0};
    copy_bytes(&decoded_symbol, odd_wire + 1 + sizeof(ll_coff_symbol),
               sizeof(decoded_symbol));
    copy_bytes(&decoded_relocation,
               odd_wire + 1 + sizeof(symbols) + sizeof(ll_coff_relocation),
               sizeof(decoded_relocation));
    if (decoded_symbol.Value != symbols[1].Value ||
        decoded_symbol.SectionNumber != -2 ||
        decoded_relocation.SymbolTableIndex != relocations[1].SymbolTableIndex ||
        decoded_relocation.Type != relocations[1].Type) return 3;
    return 0;
}

static int import_bits(void)
{
    for (unsigned bit = 0; bit < 16; ++bit) {
        unsigned word = 1u << bit;
        ll_coff_import_header header = {0};
        header.Type = word & 3;
        header.NameType = (word >> 2) & 7;
        header.Reserved = word >> 5;
        const unsigned char *bytes = (const unsigned char *)&header;
        if (bytes[18] != (word & 255) || bytes[19] != (word >> 8)) return 4;
#ifdef LL_COFF_COMPARE_SDK
        IMPORT_OBJECT_HEADER sdk = {0};
        sdk.Type = header.Type;
        sdk.NameType = header.NameType;
        sdk.Reserved = header.Reserved;
        const unsigned char *sdk_bytes = (const unsigned char *)&sdk;
        for (size_t i = 0; i < sizeof(header); ++i)
            if (bytes[i] != sdk_bytes[i]) return 5;
#endif
    }
    return 0;
}

int main(void)
{
    int result = packed_arrays();
    return result != 0 ? result : import_bits();
}
