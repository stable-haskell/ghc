/* COFF object-file declarations, independent of Windows host services.
 * Records retain SDK packing. In particular symbol/relocation arrays have
 * two-byte alignment: access packed fields through these types, never through
 * a separately cast uint32_t pointer. Copy arbitrary byte-offset input into an
 * aligned record before accessing it. This is not a byte-swapping interface.
 */
#pragma once

#include <stddef.h>
#include <stdint.h>

#pragma pack(push, 4)
typedef struct ll_coff_file_header {
    uint16_t Machine, NumberOfSections;
    uint32_t TimeDateStamp, PointerToSymbolTable, NumberOfSymbols;
    uint16_t SizeOfOptionalHeader, Characteristics;
} ll_coff_file_header;

typedef struct ll_coff_class_id {
    uint32_t Data1;
    uint16_t Data2, Data3;
    uint8_t Data4[8];
} ll_coff_class_id;

typedef struct ll_coff_anon_header {
    uint16_t Sig1, Sig2, Version, Machine;
    uint32_t TimeDateStamp;
    ll_coff_class_id ClassID;
    uint32_t SizeOfData;
} ll_coff_anon_header;

typedef struct ll_coff_bigobj_header {
    uint16_t Sig1, Sig2, Version, Machine;
    uint32_t TimeDateStamp;
    ll_coff_class_id ClassID;
    uint32_t SizeOfData, Flags, MetaDataSize, MetaDataOffset;
    uint32_t NumberOfSections, PointerToSymbolTable, NumberOfSymbols;
} ll_coff_bigobj_header;

typedef struct ll_coff_section {
    uint8_t Name[8];
    union { uint32_t PhysicalAddress, VirtualSize; } Misc;
    uint32_t VirtualAddress, SizeOfRawData, PointerToRawData;
    uint32_t PointerToRelocations, PointerToLinenumbers;
    uint16_t NumberOfRelocations, NumberOfLinenumbers;
    uint32_t Characteristics;
} ll_coff_section;

/* The Windows COFF ABI uses pack(2), not the compiler's default struct
 * packing, for these records. Scope the pragma so it cannot affect clients. */
#pragma pack(push, 2)
typedef struct ll_coff_symbol {
    union {
        uint8_t ShortName[8];
        struct { uint32_t Short, Long; } Name;
        uint32_t LongName[2];
    } N;
    uint32_t Value;
    int16_t SectionNumber;
    uint16_t Type;
    uint8_t StorageClass, NumberOfAuxSymbols;
} ll_coff_symbol;

typedef struct ll_coff_symbol_ex {
    union {
        uint8_t ShortName[8];
        struct { uint32_t Short, Long; } Name;
        uint32_t LongName[2];
    } N;
    uint32_t Value;
    int32_t SectionNumber;
    uint16_t Type;
    uint8_t StorageClass, NumberOfAuxSymbols;
} ll_coff_symbol_ex;

typedef struct ll_coff_relocation {
    union { uint32_t VirtualAddress, RelocCount; };
    uint32_t SymbolTableIndex;
    uint16_t Type;
} ll_coff_relocation;
#pragma pack(pop)

typedef struct ll_coff_import_header {
    uint16_t Sig1, Sig2, Version, Machine;
    uint32_t TimeDateStamp, SizeOfData;
    union { uint16_t Ordinal, Hint; };
    uint16_t Type:2, NameType:3, Reserved:11;
} ll_coff_import_header;
#pragma pack(pop)

_Static_assert(sizeof(ll_coff_file_header) == 20, "COFF file header size");
_Static_assert(sizeof(ll_coff_class_id) == 16, "COFF class ID size");
_Static_assert(sizeof(ll_coff_anon_header) == 32, "COFF anonymous header size");
_Static_assert(sizeof(ll_coff_bigobj_header) == 56, "COFF bigobj header size");
_Static_assert(sizeof(ll_coff_section) == 40, "COFF section size");
_Static_assert(sizeof(ll_coff_symbol) == 18 && _Alignof(ll_coff_symbol) == 2,
               "COFF symbol packing");
_Static_assert(sizeof(ll_coff_symbol_ex) == 20 && _Alignof(ll_coff_symbol_ex) == 2,
               "COFF extended symbol packing");
_Static_assert(sizeof(ll_coff_relocation) == 10 && _Alignof(ll_coff_relocation) == 2,
               "COFF relocation packing");
_Static_assert(sizeof(ll_coff_import_header) == 20, "COFF import header size");

#define LL_COFF_IMAGE_FILE_BYTES_REVERSED_HI 0x8000
#define LL_COFF_IMAGE_FILE_DLL 0x2000
#define LL_COFF_IMAGE_FILE_EXECUTABLE_IMAGE 0x0002
#define LL_COFF_IMAGE_FILE_MACHINE_AMD64 0x8664
#define LL_COFF_IMAGE_FILE_MACHINE_ARM64 0xaa64
#define LL_COFF_IMAGE_FILE_MACHINE_UNKNOWN 0
#define LL_COFF_IMAGE_REL_ARM64_ABSOLUTE 0x0000
#define LL_COFF_IMAGE_REL_ARM64_ADDR32 0x0001
#define LL_COFF_IMAGE_REL_ARM64_ADDR32NB 0x0002
#define LL_COFF_IMAGE_REL_ARM64_BRANCH26 0x0003
#define LL_COFF_IMAGE_REL_ARM64_PAGEBASE_REL21 0x0004
#define LL_COFF_IMAGE_REL_ARM64_REL21 0x0005
#define LL_COFF_IMAGE_REL_ARM64_PAGEOFFSET_12A 0x0006
#define LL_COFF_IMAGE_REL_ARM64_PAGEOFFSET_12L 0x0007
#define LL_COFF_IMAGE_REL_ARM64_SECREL 0x0008
#define LL_COFF_IMAGE_REL_ARM64_SECREL_LOW12A 0x0009
#define LL_COFF_IMAGE_REL_ARM64_SECREL_HIGH12A 0x000a
#define LL_COFF_IMAGE_REL_ARM64_SECREL_LOW12L 0x000b
#define LL_COFF_IMAGE_REL_ARM64_TOKEN 0x000c
#define LL_COFF_IMAGE_REL_ARM64_SECTION 0x000d
#define LL_COFF_IMAGE_REL_ARM64_ADDR64 0x000e
#define LL_COFF_IMAGE_REL_ARM64_BRANCH19 0x000f
#define LL_COFF_IMAGE_REL_ARM64_BRANCH14 0x0010
#define LL_COFF_IMAGE_REL_ARM64_REL32 0x0011
#define LL_COFF_IMAGE_FILE_SYSTEM 0x1000
#define LL_COFF_IMAGE_SCN_ALIGN_1024BYTES 0x00B00000
#define LL_COFF_IMAGE_SCN_ALIGN_128BYTES 0x00800000
#define LL_COFF_IMAGE_SCN_ALIGN_16BYTES 0x00500000
#define LL_COFF_IMAGE_SCN_ALIGN_1BYTES 0x00100000
#define LL_COFF_IMAGE_SCN_ALIGN_2048BYTES 0x00C00000
#define LL_COFF_IMAGE_SCN_ALIGN_256BYTES 0x00900000
#define LL_COFF_IMAGE_SCN_ALIGN_2BYTES 0x00200000
#define LL_COFF_IMAGE_SCN_ALIGN_32BYTES 0x00600000
#define LL_COFF_IMAGE_SCN_ALIGN_4096BYTES 0x00D00000
#define LL_COFF_IMAGE_SCN_ALIGN_4BYTES 0x00300000
#define LL_COFF_IMAGE_SCN_ALIGN_512BYTES 0x00A00000
#define LL_COFF_IMAGE_SCN_ALIGN_64BYTES 0x00700000
#define LL_COFF_IMAGE_SCN_ALIGN_8192BYTES 0x00E00000
#define LL_COFF_IMAGE_SCN_ALIGN_8BYTES 0x00400000
#define LL_COFF_IMAGE_SCN_CNT_CODE 0x00000020
#define LL_COFF_IMAGE_SCN_CNT_UNINITIALIZED_DATA 0x00000080
#define LL_COFF_IMAGE_SCN_LNK_COMDAT 0x00001000
#define LL_COFF_IMAGE_SCN_LNK_NRELOC_OVFL 0x01000000
#define LL_COFF_IMAGE_SCN_LNK_REMOVE 0x00000800
#define LL_COFF_IMAGE_SCN_MEM_DISCARDABLE 0x02000000
#define LL_COFF_IMAGE_SCN_MEM_WRITE 0x80000000
#define LL_COFF_IMAGE_SYM_ABSOLUTE (int16_t)-1
#define LL_COFF_IMAGE_SYM_CLASS_EXTERNAL 0x0002
#define LL_COFF_IMAGE_SYM_CLASS_SECTION 0x0068
#define LL_COFF_IMAGE_SYM_CLASS_STATIC 0x0003
#define LL_COFF_IMAGE_SYM_CLASS_WEAK_EXTERNAL 0x0069
#define LL_COFF_IMAGE_SYM_DEBUG (int16_t)-2
#define LL_COFF_IMAGE_SYM_UNDEFINED (int16_t)0
#define LL_COFF_IMPORT_OBJECT_CODE 0
#define LL_COFF_IMPORT_OBJECT_HDR_SIG2 0xffff
