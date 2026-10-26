/* Bounds required before the native ELF backend follows image pointers. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)
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

static bool nativeElfMachine(Elf_Half machine)
{
#if LL_TARGET_X86_64
#if defined(EM_X86_64)
    return machine == EM_X86_64;
#else
    return machine == EM_AMD64;
#endif
#elif LL_TARGET_I386
    return machine == EM_386;
#elif LL_TARGET_AARCH64
    return machine == EM_AARCH64;
#elif LL_TARGET_ARM
    return machine == EM_ARM;
#elif LL_TARGET_RISCV64
    return machine == EM_RISCV;
#elif LL_TARGET_POWERPC
    return machine == EM_PPC;
#elif LL_TARGET_SPARC
    return machine == EM_SPARC
#if defined(EM_SPARC32PLUS)
        || machine == EM_SPARC32PLUS
#endif
        ;
#else
    (void)machine;
    return false;
#endif
}

static bool elfStrings(const char *image, const Elf_Shdr *strings)
{
    /* The complete range was checked before this helper is called. Requiring
     * the ELF string-table sentinels also bounds every in-range name offset. */
    return strings->sh_type == SHT_STRTAB && strings->sh_size != 0 &&
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
    if (symbolCount > limit / sizeof(SymbolExtra)) return false;
    uintmax_t extrasSize = symbolCount * sizeof(SymbolExtra);
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

int ocValidateImage_ELF(ObjectCode *oc)
{
    if (oc->image == NULL || oc->fileSize < (int)sizeof(Elf_Ehdr))
        return invalidElf(oc, LL_E_FORMAT, "truncated ELF header");

    const char *image = oc->image;
    size_t size = (size_t)oc->fileSize;
    Elf_Ehdr header;
    memcpy(&header, image, sizeof(header));
    if (memcmp(header.e_ident, ELFMAG, SELFMAG) != 0)
        return invalidElf(oc, LL_E_FORMAT, "not an ELF object");
    if (header.e_ident[EI_CLASS] != ELFCLASS)
        return invalidElf(oc, LL_E_UNSUPPORTED, "unsupported ELF class");
    const uint16_t endian = 1;
    unsigned nativeData = *(const unsigned char *)&endian == 1
                          ? ELFDATA2LSB : ELFDATA2MSB;
    if (header.e_ident[EI_DATA] != ELFDATA2LSB &&
        header.e_ident[EI_DATA] != ELFDATA2MSB)
        return invalidElf(oc, LL_E_FORMAT, "unknown ELF byte order");
    if (header.e_ident[EI_DATA] != nativeData)
        return invalidElf(oc, LL_E_UNSUPPORTED, "non-native ELF byte order");
    if (header.e_ident[EI_VERSION] != EV_CURRENT || header.e_version != EV_CURRENT)
        return invalidElf(oc, LL_E_UNSUPPORTED, "unsupported ELF version");
    if (header.e_type != ET_REL)
        return invalidElf(oc, LL_E_FORMAT, "not a relocatable ELF object");
    if (!nativeElfMachine(header.e_machine))
        return invalidElf(oc, LL_E_UNSUPPORTED, "unsupported ELF architecture");
    if (header.e_ehsize != sizeof(Elf_Ehdr) ||
        header.e_shentsize != sizeof(Elf_Shdr) || header.e_shoff == 0 ||
        !elfTable(size, header.e_shoff, 1, sizeof(Elf_Shdr), _Alignof(Elf_Shdr)))
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF section header table");

    const Elf_Shdr *sections = (const Elf_Shdr *)(image + header.e_shoff);
    if (sections[0].sh_type != SHT_NULL)
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF null section");
    uintmax_t count = header.e_shnum == SHN_UNDEF
                      ? sections[0].sh_size : header.e_shnum;
    if (count > INT_MAX)
        return invalidElf(oc, LL_E_UNSUPPORTED, "too many ELF sections");
    if (count == 0 ||
        !elfTable(size, header.e_shoff, count, sizeof(Elf_Shdr), _Alignof(Elf_Shdr)))
        return invalidElf(oc, LL_E_FORMAT, "truncated ELF section header table");

    uintmax_t names = header.e_shstrndx;
#if defined(SHN_XINDEX)
    if (names == SHN_XINDEX)
        names = sections[0].sh_link;
#endif
    if (names == SHN_UNDEF || names >= count)
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF section-name table index");

    uintmax_t programCount = header.e_phnum;
#if defined(PN_XNUM)
    if (programCount == PN_XNUM)
        programCount = sections[0].sh_info;
#endif
    if (header.e_phnum != 0 && programCount == 0)
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF extended program-header count");
    if (programCount != 0 &&
        (header.e_phentsize != sizeof(Elf_Phdr) ||
         !elfTable(size, header.e_phoff, programCount,
                   sizeof(Elf_Phdr), _Alignof(Elf_Phdr))))
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF program header table");

    const Elf_Word *extended = NULL;
    size_t extendedCount = 0;
    uintmax_t bssSize = 0;
    uintmax_t symbolCount = 0;
    for (size_t i = 0; i < count; i++) {
        const Elf_Shdr *section = &sections[i];
        if (section->sh_type != SHT_NOBITS && section->sh_type != SHT_NULL &&
            !elfSpan(size, section->sh_offset, section->sh_size))
            return invalidElf(oc, LL_E_FORMAT, "ELF section extends past the image");
        uintmax_t align = section->sh_addralign;
        if (align != 0 && (align & (align - 1)) != 0)
            return invalidElf(oc, LL_E_FORMAT, "invalid ELF section alignment");
        if (section->sh_size != 0 && align > INT_MAX)
            return invalidElf(oc, LL_E_UNSUPPORTED, "ELF section alignment is too large");
        if (section->sh_type == SHT_NOBITS && section->sh_size > INT_MAX)
            return invalidElf(oc, LL_E_UNSUPPORTED, "ELF zero-fill section is too large");
        if (section->sh_type == SHT_NOBITS &&
            (section->sh_flags & SHF_ALLOC) && (section->sh_flags & SHF_WRITE)) {
            uintmax_t alignment = align == 0 ? 1 : align;
            uintmax_t padding = (alignment - section->sh_size % alignment) % alignment;
            if (section->sh_size > (uintmax_t)INT_MAX - bssSize ||
                padding > (uintmax_t)INT_MAX - bssSize - section->sh_size)
                return invalidElf(oc, LL_E_UNSUPPORTED, "ELF zero-fill allocation is too large");
            bssSize += section->sh_size + padding;
        }
        if (section->sh_type == SHT_SYMTAB) {
            uintmax_t symbols = section->sh_size / sizeof(Elf_Sym);
            if (symbols > (uintmax_t)INT_MAX - symbolCount)
                return invalidElf(oc, LL_E_UNSUPPORTED, "too many ELF symbols");
            symbolCount += symbols;
        }
#if defined(SHT_SYMTAB_SHNDX)
        if (section->sh_type == SHT_SYMTAB_SHNDX) {
            if (section->sh_entsize != sizeof(Elf_Word) ||
                section->sh_size % sizeof(Elf_Word) != 0 ||
                !elfTable(size, section->sh_offset,
                          section->sh_size / sizeof(Elf_Word),
                          sizeof(Elf_Word), _Alignof(Elf_Word)) ||
                section->sh_link == SHN_UNDEF || section->sh_link >= count ||
                sections[section->sh_link].sh_type != SHT_SYMTAB)
                return invalidElf(oc, LL_E_FORMAT, "invalid ELF extended symbol indexes");
            /* Match get_shndx_table's first-table selection. */
            if (extended == NULL) {
                extended = (const Elf_Word *)(image + section->sh_offset);
                extendedCount = section->sh_size / sizeof(Elf_Word);
            }
        }
#endif
    }

    const Elf_Shdr *nameTable = &sections[names];
    if (!elfStrings(image, nameTable))
        return invalidElf(oc, LL_E_FORMAT, "invalid ELF section-name string table");

    for (size_t i = 0; i < count; i++) {
        const Elf_Shdr *section = &sections[i];
        if (section->sh_name >= nameTable->sh_size)
            return invalidElf(oc, LL_E_FORMAT, "invalid ELF section-name offset");
        const char *name = image + nameTable->sh_offset + section->sh_name;
        if ((section->sh_type == SHT_NULL || section->sh_type == SHT_NOBITS) &&
            elfInitArrayName(name) && section->sh_size != 0)
            return invalidElf(oc, LL_E_FORMAT, "ELF initializer section has no file contents");
        if (section->sh_type == SHT_SYMTAB) {
            size_t symbols = section->sh_size / sizeof(Elf_Sym);
            if (section->sh_entsize != sizeof(Elf_Sym) ||
                section->sh_size % sizeof(Elf_Sym) != 0 ||
                !elfTable(size, section->sh_offset, symbols,
                          sizeof(Elf_Sym), _Alignof(Elf_Sym)) ||
                section->sh_link == SHN_UNDEF || section->sh_link >= count ||
                !elfStrings(image, &sections[section->sh_link]) ||
                section->sh_info > symbols)
                return invalidElf(oc, LL_E_FORMAT, "invalid ELF symbol table");
            const Elf_Sym *table = (const Elf_Sym *)(image + section->sh_offset);
            for (size_t j = 0; j < symbols; j++) {
                if (table[j].st_name >= sections[section->sh_link].sh_size)
                    return invalidElf(oc, LL_E_FORMAT, "invalid ELF symbol-name offset");
                uintmax_t index = table[j].st_shndx;
#if defined(SHN_XINDEX)
                if (index == SHN_XINDEX) {
                    if (extended == NULL || j >= extendedCount)
                        return invalidElf(oc, LL_E_FORMAT, "missing ELF extended symbol index");
                    index = extended[j];
                    if (index == SHN_UNDEF || index >= count)
                        return invalidElf(oc, LL_E_FORMAT, "invalid ELF extended section index");
                } else
#endif
                if (index < SHN_LORESERVE && index >= count)
                    return invalidElf(oc, LL_E_FORMAT, "invalid ELF symbol section index");
                uintmax_t value = table[j].st_value;
#if LL_TARGET_ARM
                /* The low bit encodes Thumb mode, not a byte in the section. */
                if (ELF_ST_TYPE(table[j].st_info) == STT_FUNC)
                    value &= ~UINTMAX_C(1);
#endif
                if (index > SHN_UNDEF && index < count &&
                    table[j].st_shndx != SHN_COMMON && table[j].st_shndx != SHN_ABS &&
                    (value > sections[index].sh_size ||
                     table[j].st_size > sections[index].sh_size - value))
                    return invalidElf(oc, LL_E_FORMAT, "ELF symbol extends past its section");
            }
        } else if (section->sh_type == SHT_REL || section->sh_type == SHT_RELA) {
            bool rela = section->sh_type == SHT_RELA;
            size_t entrySize = rela ? sizeof(Elf_Rela) : sizeof(Elf_Rel);
            size_t alignment = rela ? _Alignof(Elf_Rela) : _Alignof(Elf_Rel);
            if (section->sh_entsize != entrySize || section->sh_size % entrySize != 0 ||
                !elfTable(size, section->sh_offset, section->sh_size / entrySize,
                          entrySize, alignment) ||
                section->sh_link == SHN_UNDEF || section->sh_link >= count ||
                sections[section->sh_link].sh_type != SHT_SYMTAB ||
                section->sh_info == SHN_UNDEF || section->sh_info >= count)
                return invalidElf(oc, LL_E_FORMAT, "invalid ELF relocation table");
            size_t symbols = sections[section->sh_link].sh_size / sizeof(Elf_Sym);
            for (size_t j = 0; j < section->sh_size / entrySize; j++) {
                Elf_Rela relocation = {0};
                if (rela) {
                    memcpy(&relocation, image + section->sh_offset + j * entrySize,
                           sizeof(Elf_Rela));
                } else {
                    Elf_Rel rel;
                    memcpy(&rel, image + section->sh_offset + j * entrySize, sizeof(rel));
                    relocation.r_info = rel.r_info;
                    relocation.r_offset = rel.r_offset;
                }
                if (ELF_R_SYM(relocation.r_info) >= symbols ||
                    (ELF_R_TYPE(relocation.r_info) != 0 &&
                     relocation.r_offset >= sections[section->sh_info].sh_size))
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
