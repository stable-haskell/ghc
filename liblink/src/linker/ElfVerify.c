/* Bounds required before the native ELF backend follows image pointers. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"

#if LL_BUILD_ELF
#include "linker/elf_compat.h"
#endif

#include "LinkerInternals.h"

#if LL_BUILD_ELF
#include "linker/Elf.h"
#include "ll_error.h"

#include <limits.h>
#include <string.h>

static int invalidElf(ObjectCode *oc, ll_status status, const char *reason)
{
    ll_error_set(oc->L, status, "%" PATH_FMT ": %s",
                 OC_INFORMATIVE_FILENAME(oc), reason);
    LL_LOG_ERROR_FOR(oc->L, "%" PATH_FMT ": %s",
                     OC_INFORMATIVE_FILENAME(oc), reason);
    return 0;
}

static bool elfSpan(size_t size, uintmax_t offset, uintmax_t length)
{
    return offset <= size && length <= size - (size_t)offset;
}

static bool elfTable(size_t size, uintmax_t offset, uintmax_t count,
                     size_t entrySize, size_t alignment)
{
    return offset <= size && offset % alignment == 0 &&
           count <= (size - (size_t)offset) / entrySize;
}

static bool nativeElfMachine(ll_elf_target_half machine)
{
#if LL_TARGET_X86_64
#if LL_ELF_HAS_EM_X86_64
    return machine == LL_ELF_EM_X86_64;
#else
    return machine == LL_ELF_EM_AMD64;
#endif
#elif LL_TARGET_I386
    return machine == LL_ELF_EM_386;
#elif LL_TARGET_AARCH64
    return machine == LL_ELF_EM_AARCH64;
#elif LL_TARGET_ARM
    return machine == LL_ELF_EM_ARM;
#elif LL_TARGET_RISCV64
    return machine == LL_ELF_EM_RISCV;
#elif LL_TARGET_POWERPC
    return machine == LL_ELF_EM_PPC;
#elif LL_TARGET_SPARC
    return machine == LL_ELF_EM_SPARC
#if LL_ELF_HAS_EM_SPARC32PLUS
        || machine == LL_ELF_EM_SPARC32PLUS
#endif
        ;
#else
    (void)machine;
    return false;
#endif
}

static bool elfStrings(const char *image, const ll_elf_target_shdr *strings)
{
    /* The complete range was checked before this helper is called. Requiring
     * the ELF string-table sentinels also bounds every in-range name offset. */
    return strings->sh_type == LL_ELF_SHT_STRTAB && strings->sh_size != 0 &&
           image[strings->sh_offset] == '\0' &&
           image[strings->sh_offset + strings->sh_size - 1] == '\0';
}

static bool elfInitArrayName(const char *name)
{
    return strncmp(name, ".ctors", 6) == 0 ||
           strncmp(name, ".dtors", 6) == 0 ||
           strncmp(name, ".init_array", 11) == 0 ||
           strncmp(name, ".fini_array", 11) == 0;
}

#if LL_ELF_NEED_SYMBOL_EXTRAS
static bool elfExtrasFit(ObjectCode *oc, size_t imageSize, uintmax_t bssSize,
                         uintmax_t symbolCount)
{
    const uintmax_t limit = INT_MAX;
    /* The backend allocates extras for its last symbol table. Bounding all
     * tables also covers that selection without narrowing the symbol count. */
    const uintmax_t extraStride = LL_TARGET_SYMBOL_EXTRA_SIZE;
    if (extraStride != 0 && symbolCount > limit / extraStride) return false;
    uintmax_t extrasSize = symbolCount * extraStride;
    if (symbolCount == 0 && bssSize == 0) return true;
    if (!RTS_LINKER_USE_MMAP) {
        /* ocAllocateExtras first rounds the signed file size to four bytes. */
        if (imageSize > limit - 3) return false;
        uintmax_t aligned = ((uintmax_t)imageSize + 3) & ~UINTMAX_C(3);
        return extrasSize <= limit - aligned &&
               (uintmax_t)oc->misalignment <= limit - aligned - extrasSize;
    }
    if (USE_CONTIGUOUS_MMAP || oc->L->services.cfg.always_pic) {
        uintmax_t page = ll_page_size_for(oc->L);
        if (page == 0) return false;
        uintmax_t imagePadding = (page - imageSize % page) % page;
        uintmax_t bssPadding = (page - bssSize % page) % page;
        if (imagePadding > limit - imageSize || bssPadding > limit - bssSize)
            return false;
        uintmax_t roundedImage = imageSize + imagePadding;
        uintmax_t roundedBss = bssSize + bssPadding;
        /* Both the rounded BSS and the combined mapping become signed ints
         * in SymbolExtras.c. Reject before either narrowing conversion. */
        return roundedBss <= limit - roundedImage &&
               extrasSize <= limit - roundedImage - roundedBss;
    }
    return true;
}
#endif

static size_t elfRelocationWidth(uint32_t type)
{
#if LL_TARGET_X86_64
    switch (type) {
    case COMPAT_R_X86_64_NONE:
        return 0;
    case COMPAT_R_X86_64_64:
    case COMPAT_R_X86_64_PC64:
        return 8;
    case COMPAT_R_X86_64_16:
    case COMPAT_R_X86_64_PC16:
        return 2;
    case COMPAT_R_X86_64_8:
    case COMPAT_R_X86_64_PC8:
        return 1;
    default:
        return 4;
    }
#elif LL_TARGET_AARCH64
    switch (type) {
    case COMPAT_R_AARCH64_NONE:
        return 0;
    case COMPAT_R_AARCH64_ABS64:
    case COMPAT_R_AARCH64_PREL64:
        return 8;
    case COMPAT_R_AARCH64_ABS16:
    case COMPAT_R_AARCH64_PREL16:
        return 2;
    default:
        return 4;
    }
#elif LL_TARGET_RISCV64
    switch (type) {
    case LL_ELF_R_RISCV_NONE:
    case LL_ELF_R_RISCV_RELAX:
    case LL_ELF_R_RISCV_ALIGN:
        return 0;
    case LL_ELF_R_RISCV_CALL:
    case LL_ELF_R_RISCV_CALL_PLT:
    case LL_ELF_R_RISCV_64:
    case LL_ELF_R_RISCV_ADD64:
    case LL_ELF_R_RISCV_SUB64:
    case LL_ELF_R_RISCV_TLS_DTPMOD64:
    case LL_ELF_R_RISCV_TLS_DTPREL64:
    case LL_ELF_R_RISCV_TLS_TPREL64:
        return 8;
    case LL_ELF_R_RISCV_ADD16:
    case LL_ELF_R_RISCV_SUB16:
    case LL_ELF_R_RISCV_SET16:
    case LL_ELF_R_RISCV_RVC_BRANCH:
    case LL_ELF_R_RISCV_RVC_JUMP:
        return 2;
    case LL_ELF_R_RISCV_ADD8:
    case LL_ELF_R_RISCV_SUB8:
    case LL_ELF_R_RISCV_SET8:
    case LL_ELF_R_RISCV_SUB6:
    case LL_ELF_R_RISCV_SET6:
        return 1;
    default:
        return 4;
    }
#elif LL_TARGET_ARM
    switch (type) {
    case COMPAT_R_ARM_NONE:
    case COMPAT_R_ARM_V4BX:
        return 0;
    case COMPAT_R_ARM_THM_JUMP8:
    case COMPAT_R_ARM_THM_JUMP11:
        return 2;
    default:
        return 4;
    }
#elif LL_TARGET_I386
    switch (type) {
    case COMPAT_R_386_NONE:
        return 0;
    case COMPAT_R_386_16:
    case COMPAT_R_386_PC16:
        return 2;
    case COMPAT_R_386_8:
    case COMPAT_R_386_PC8:
        return 1;
    default:
        return 4;
    }
#else
    (void)type;
    return sizeof(ll_elf_target_word);
#endif
}

int ocValidateImage_ELF(ObjectCode *oc)
{
    if (oc->image == NULL || oc->fileSize < (int)sizeof(ll_elf_target_ehdr))
        return invalidElf(oc, LL_E_FORMAT, "truncated ELF header");

    const char *image = oc->image;
    size_t size = (size_t)oc->fileSize;
    ll_elf_target_ehdr header;
    memcpy(&header, image, sizeof(header));
    if (memcmp(header.e_ident, LL_ELF_ELFMAG, LL_ELF_SELFMAG) != 0)
        return invalidElf(oc, LL_E_FORMAT, "not an ELF object");
    if (header.e_ident[LL_ELF_EI_CLASS] != LL_ELF_CLASS)
        return invalidElf(oc, LL_E_UNSUPPORTED, "unsupported ELF class");
    const uint16_t endian = 1;
    unsigned nativeData = *(const unsigned char *)&endian == 1
                          ? LL_ELF_ELFDATA2LSB : LL_ELF_ELFDATA2MSB;
    if (header.e_ident[LL_ELF_EI_DATA] != LL_ELF_ELFDATA2LSB &&
        header.e_ident[LL_ELF_EI_DATA] != LL_ELF_ELFDATA2MSB)
        return invalidElf(oc, LL_E_FORMAT, "unknown ELF byte order");
    if (header.e_ident[LL_ELF_EI_DATA] != nativeData)
        return invalidElf(oc, LL_E_UNSUPPORTED, "non-native ELF byte order");
    if (header.e_ident[LL_ELF_EI_VERSION] != LL_ELF_EV_CURRENT || header.e_version != LL_ELF_EV_CURRENT)
        return invalidElf(oc, LL_E_UNSUPPORTED, "unsupported ELF version");
    if (header.e_type != LL_ELF_ET_REL)
        return invalidElf(oc, LL_E_FORMAT, "not a relocatable ELF object");
    if (!nativeElfMachine(header.e_machine))
        return invalidElf(oc, LL_E_UNSUPPORTED, "unsupported ELF architecture");
    if (header.e_ehsize != sizeof(ll_elf_target_ehdr) ||
        header.e_shentsize != sizeof(ll_elf_target_shdr) || header.e_shoff == 0 ||
        !elfTable(size, header.e_shoff, 1, sizeof(ll_elf_target_shdr), _Alignof(ll_elf_target_shdr)))
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF section header table");

    const ll_elf_target_shdr *sections = (const ll_elf_target_shdr *)(image + header.e_shoff);
    if (sections[0].sh_type != LL_ELF_SHT_NULL)
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF null section");
    uintmax_t count = header.e_shnum == LL_ELF_SHN_UNDEF
                      ? sections[0].sh_size : header.e_shnum;
    if (count > INT_MAX)
        return invalidElf(oc, LL_E_UNSUPPORTED, "too many ELF sections");
    if (count == 0 ||
        !elfTable(size, header.e_shoff, count, sizeof(ll_elf_target_shdr), _Alignof(ll_elf_target_shdr)))
        return invalidElf(oc, LL_E_FORMAT, "truncated ELF section header table");

    uintmax_t names = header.e_shstrndx;
#if LL_ELF_HAS_SHN_XINDEX
    if (names == LL_ELF_SHN_XINDEX)
        names = sections[0].sh_link;
#endif
    if (names == LL_ELF_SHN_UNDEF || names >= count)
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF section-name table index");

    uintmax_t programCount = header.e_phnum;
#if defined(PN_XNUM)
    if (programCount == PN_XNUM)
        programCount = sections[0].sh_info;
#endif
    if (header.e_phnum != 0 && programCount == 0)
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF extended program-header count");
    if (programCount != 0 &&
        (header.e_phentsize != sizeof(ll_elf_target_phdr) ||
         !elfTable(size, header.e_phoff, programCount,
                   sizeof(ll_elf_target_phdr), _Alignof(ll_elf_target_phdr))))
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF program header table");

    const ll_elf_target_word *extended = NULL;
    size_t extendedCount = 0;
    uintmax_t bssSize = 0;
    uintmax_t symbolCount = 0;
    size_t symtabCount = 0;
    for (size_t i = 0; i < count; i++) {
        const ll_elf_target_shdr *section = &sections[i];
        if (section->sh_type != LL_ELF_SHT_NOBITS && section->sh_type != LL_ELF_SHT_NULL &&
            !elfSpan(size, section->sh_offset, section->sh_size))
            return invalidElf(oc, LL_E_FORMAT, "ELF section extends past the image");
        uintmax_t align = section->sh_addralign;
        if (align != 0 && (align & (align - 1)) != 0)
            return invalidElf(oc, LL_E_FORMAT, "invalid ELF section alignment");
        if (section->sh_size != 0 && align > INT_MAX)
            return invalidElf(oc, LL_E_UNSUPPORTED, "ELF section alignment is too large");
        if (section->sh_type == LL_ELF_SHT_NOBITS && section->sh_size > INT_MAX)
            return invalidElf(oc, LL_E_UNSUPPORTED, "ELF zero-fill section is too large");
        if (section->sh_type == LL_ELF_SHT_NOBITS &&
            (section->sh_flags & LL_ELF_SHF_ALLOC) && (section->sh_flags & LL_ELF_SHF_WRITE)) {
            uintmax_t alignment = align == 0 ? 1 : align;
            uintmax_t padding = (alignment - section->sh_size % alignment) % alignment;
            if (section->sh_size > (uintmax_t)INT_MAX - bssSize ||
                padding > (uintmax_t)INT_MAX - bssSize - section->sh_size)
                return invalidElf(oc, LL_E_UNSUPPORTED, "ELF zero-fill allocation is too large");
            bssSize += section->sh_size + padding;
        }
        if (section->sh_type == LL_ELF_SHT_SYMTAB) {
            if (symtabCount > 0)
                return invalidElf(oc, LL_E_UNSUPPORTED, "multiple ELF symbol tables are not supported");
            symtabCount++;
            uintmax_t symbols = section->sh_size / sizeof(ll_elf_target_sym);
            if (symbols > (uintmax_t)INT_MAX - symbolCount)
                return invalidElf(oc, LL_E_UNSUPPORTED, "too many ELF symbols");
            symbolCount += symbols;
        }
#if LL_ELF_HAS_SHT_SYMTAB_SHNDX
        if (section->sh_type == LL_ELF_SHT_SYMTAB_SHNDX) {
            if (section->sh_entsize != sizeof(ll_elf_target_word) ||
                section->sh_size % sizeof(ll_elf_target_word) != 0 ||
                !elfTable(size, section->sh_offset,
                          section->sh_size / sizeof(ll_elf_target_word),
                          sizeof(ll_elf_target_word), _Alignof(ll_elf_target_word)) ||
                section->sh_link == LL_ELF_SHN_UNDEF || section->sh_link >= count ||
                sections[section->sh_link].sh_type != LL_ELF_SHT_SYMTAB)
                return invalidElf(oc, LL_E_FORMAT, "invalid ELF extended symbol indexes");
            /* Match get_shndx_table's first-table selection. */
            if (extended == NULL) {
                extended = (const ll_elf_target_word *)(image + section->sh_offset);
                extendedCount = section->sh_size / sizeof(ll_elf_target_word);
            }
        }
#endif
    }

    const ll_elf_target_shdr *nameTable = &sections[names];
    if (!elfStrings(image, nameTable))
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF section-name string table");

    for (size_t i = 0; i < count; i++) {
        const ll_elf_target_shdr *section = &sections[i];
        if (section->sh_name >= nameTable->sh_size)
            return invalidElf(oc, LL_E_FORMAT, "invalid ELF section-name offset");
        const char *name = image + nameTable->sh_offset + section->sh_name;
        if ((section->sh_type == LL_ELF_SHT_NULL || section->sh_type == LL_ELF_SHT_NOBITS) &&
            elfInitArrayName(name) && section->sh_size != 0)
            return invalidElf(oc, LL_E_FORMAT, "ELF initializer section has no file contents");
        if (section->sh_type == LL_ELF_SHT_SYMTAB) {
            size_t symbols = section->sh_size / sizeof(ll_elf_target_sym);
            if (section->sh_entsize != sizeof(ll_elf_target_sym) ||
                section->sh_size % sizeof(ll_elf_target_sym) != 0 ||
                !elfTable(size, section->sh_offset, symbols,
                          sizeof(ll_elf_target_sym), _Alignof(ll_elf_target_sym)) ||
                section->sh_link == LL_ELF_SHN_UNDEF || section->sh_link >= count ||
                !elfStrings(image, &sections[section->sh_link]) ||
                section->sh_info > symbols)
                return invalidElf(oc, LL_E_FORMAT, "invalid ELF symbol table");
            const ll_elf_target_sym *table = (const ll_elf_target_sym *)(image + section->sh_offset);
            for (size_t j = 0; j < symbols; j++) {
                if (table[j].st_name >= sections[section->sh_link].sh_size)
                    return invalidElf(oc, LL_E_FORMAT, "invalid ELF symbol-name offset");
                uintmax_t index = table[j].st_shndx;
#if LL_ELF_HAS_SHN_XINDEX
                if (index == LL_ELF_SHN_XINDEX) {
                    if (extended == NULL || j >= extendedCount)
                        return invalidElf(oc, LL_E_FORMAT, "missing ELF extended symbol index");
                    index = extended[j];
                    if (index == LL_ELF_SHN_UNDEF || index >= count)
                        return invalidElf(oc, LL_E_FORMAT, "invalid ELF extended section index");
                } else
#endif
                if (index < LL_ELF_SHN_LORESERVE && index >= count)
                    return invalidElf(oc, LL_E_FORMAT, "invalid ELF symbol section index");
                if (LL_ELF_ST_TYPE(table[j].st_info) == LL_ELF_STT_SECTION &&
                    (index == LL_ELF_SHN_UNDEF || index >= count ||
                     table[j].st_shndx == LL_ELF_SHN_ABS || table[j].st_shndx == LL_ELF_SHN_COMMON))
                    return invalidElf(oc, LL_E_FORMAT, "invalid ELF section symbol section index");
                uintmax_t value = table[j].st_value;
#if LL_TARGET_ARM
                /* The low bit encodes Thumb mode, not a byte in the section. */
                if (LL_ELF_ST_TYPE(table[j].st_info) == LL_ELF_STT_FUNC)
                    value &= ~UINTMAX_C(1);
#endif
                if (index > LL_ELF_SHN_UNDEF && index < count &&
                    table[j].st_shndx != LL_ELF_SHN_COMMON && table[j].st_shndx != LL_ELF_SHN_ABS &&
                    (value > sections[index].sh_size ||
                     table[j].st_size > sections[index].sh_size - value))
                    return invalidElf(oc, LL_E_FORMAT, "ELF symbol extends past its section");
            }
        } else if (section->sh_type == LL_ELF_SHT_REL || section->sh_type == LL_ELF_SHT_RELA) {
            bool rela = section->sh_type == LL_ELF_SHT_RELA;
            size_t entrySize = rela ? sizeof(ll_elf_target_rela) : sizeof(ll_elf_target_rel);
            size_t alignment = rela ? _Alignof(ll_elf_target_rela) : _Alignof(ll_elf_target_rel);
            if (section->sh_entsize != entrySize || section->sh_size % entrySize != 0 ||
                !elfTable(size, section->sh_offset, section->sh_size / entrySize,
                          entrySize, alignment) ||
                section->sh_link == LL_ELF_SHN_UNDEF || section->sh_link >= count ||
                sections[section->sh_link].sh_type != LL_ELF_SHT_SYMTAB ||
                section->sh_info == LL_ELF_SHN_UNDEF || section->sh_info >= count)
                return invalidElf(oc, LL_E_FORMAT, "invalid ELF relocation table");
            size_t symbols = sections[section->sh_link].sh_size / sizeof(ll_elf_target_sym);
            for (size_t j = 0; j < section->sh_size / entrySize; j++) {
                ll_elf_target_rela relocation = {0};
                if (rela) {
                    memcpy(&relocation, image + section->sh_offset + j * entrySize,
                           sizeof(ll_elf_target_rela));
                } else {
                    ll_elf_target_rel rel;
                    memcpy(&rel, image + section->sh_offset + j * entrySize, sizeof(rel));
                    relocation.r_info = rel.r_info;
                    relocation.r_offset = rel.r_offset;
                }
                uint32_t reloc_type = LL_ELF_R_TYPE(relocation.r_info);
                size_t width = elfRelocationWidth(reloc_type);
                if (LL_ELF_R_SYM(relocation.r_info) >= symbols ||
                    (reloc_type != 0 &&
                     (relocation.r_offset > sections[section->sh_info].sh_size ||
                      width > sections[section->sh_info].sh_size - (size_t)relocation.r_offset)))
                    return invalidElf(oc, LL_E_FORMAT, "invalid ELF relocation reference");
            }
        }
    }
#if LL_ELF_NEED_SYMBOL_EXTRAS
    if (!elfExtrasFit(oc, size, bssSize, symbolCount))
        return invalidElf(oc, LL_E_UNSUPPORTED, "ELF combined allocation is too large");
#endif
    return 1;
}
#endif
