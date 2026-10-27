/* Establish image bounds before the native Mach-O backend follows pointers. */
#define LL_INTERNAL_SOURCE 1
#include "ll_compat.h"
#include "ll_target.h"
#include "LinkerInternals.h"

#if LL_BUILD_MACHO
#include "linker/MachO.h"
#include "linker/MMap.h"
#include "ll_error.h"

#include <limits.h>
#include <string.h>

static int invalidMachO(ObjectCode *oc, const char *reason)
{
    ll_error_set(oc->L, LL_E_FORMAT, "%" PATH_FMT ": %s",
                 OC_INFORMATIVE_FILENAME(oc), reason);
    LL_LOG_ERROR_FOR(oc->L, "%" PATH_FMT ": %s",
                     OC_INFORMATIVE_FILENAME(oc), reason);
    return 0;
}

static bool machoSpan(size_t size, uint64_t offset, uint64_t length)
{
    return offset <= size && length <= size - (size_t)offset;
}

static bool machoTable(const char *image, size_t size, uint32_t offset,
                        uint32_t count, size_t width, size_t alignment)
{
    return offset <= size && count <= (size - offset) / width &&
           (count == 0 || (uintptr_t)(image + offset) % alignment == 0);
}

static bool machoAccumulate(size_t *total, uint64_t size, size_t alignment)
{
    /* Bound the intermediate sum used by ll_round_up_to_align, too. */
    if (*total > SIZE_MAX - (alignment - 1)) return false;
    size_t rounded = ll_round_up_to_align(*total, alignment);
    if (size > PTRDIFF_MAX || rounded > (size_t)PTRDIFF_MAX - size)
        return false;
    *total = rounded + (size_t)size;
    return true;
}

static bool machoAllocationFits(uintmax_t count, size_t width, size_t limit)
{
    /* The existing arm64 SymbolExtra is empty. Its allocation has size zero
     * even though the generic extras path still takes a symbol count. */
    return width == 0 || count <= limit / width;
}

static bool machoExtrasFit(ObjectCode *oc, uint32_t count)
{
#if LL_MACHO_NEED_SYMBOL_EXTRAS
    if (count > INT_MAX ||
        !machoAllocationFits(count, sizeof(SymbolExtra), INT_MAX))
        return false;
    size_t extras = count * sizeof(SymbolExtra);
    size_t image = (size_t)oc->fileSize;
    if (count == 0) return true;
    if (!RTS_LINKER_USE_MMAP) {
        if (image > INT_MAX - 3 || oc->misalignment < 0) return false;
        size_t aligned = (image + 3) & ~(size_t)3;
        return (size_t)oc->misalignment <= INT_MAX - aligned &&
               extras <= INT_MAX - aligned - (size_t)oc->misalignment;
    }
    if (USE_CONTIGUOUS_MMAP || oc->L->services.cfg.always_pic) {
        size_t page = ll_page_size_for(oc->L);
        if (page == 0 || page > INT_MAX ||
            image > (size_t)INT_MAX - (page - 1)) return false;
        size_t rounded = ll_round_up_to_page_for(oc->L, image);
        return rounded <= INT_MAX && extras <= INT_MAX - rounded;
    }
#else
    (void)oc;
    (void)count;
#endif
    return true;
}

static bool machoRelocSymbol(const MachORelocationInfo *reloc)
{
#if LL_TARGET_AARCH64
    /* ADDEND carries an immediate in r_symbolnum, not a symbol index. */
    switch (reloc->r_type) {
    case LL_MACHO_ARM64_RELOC_UNSIGNED:
    case LL_MACHO_ARM64_RELOC_SUBTRACTOR:
    case LL_MACHO_ARM64_RELOC_BRANCH26:
    case LL_MACHO_ARM64_RELOC_PAGE21:
    case LL_MACHO_ARM64_RELOC_PAGEOFF12:
    case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGE21:
    case LL_MACHO_ARM64_RELOC_GOT_LOAD_PAGEOFF12:
        return true;
    default:
        return false;
    }
#elif LL_TARGET_X86_64
    return reloc->r_extern || reloc->r_type == LL_MACHO_X86_64_RELOC_GOT ||
           reloc->r_type == LL_MACHO_X86_64_RELOC_GOT_LOAD;
#else
    (void)reloc;
    return false;
#endif
}

int ocVerifyImage_MachO(ObjectCode *oc)
{
    if (oc->image == NULL || oc->fileSize < (int)sizeof(MachOHeader))
        return invalidMachO(oc, "truncated Mach-O header");
    const char *image = oc->image;
    size_t size = (size_t)oc->fileSize;
    MachOHeader header;
    memcpy(&header, image, sizeof(header));
    if (header.magic != LL_MACHO_MH_MAGIC_64)
        return invalidMachO(oc, "invalid Mach-O magic");
    /* The loader may bias the image for section alignment. Require only the
     * native structure alignment, never an unconditional 16-byte image base. */
    if ((uintptr_t)image % _Alignof(MachOHeader) != 0 ||
        !machoSpan(size, sizeof(header), header.sizeofcmds))
        return invalidMachO(oc, "invalid Mach-O load-command range");

    size_t commandOffset = sizeof(header);
    size_t commandEnd = commandOffset + header.sizeofcmds;
    const MachOSegmentCommand *segment = NULL;
    const MachOSymtabCommand *symbols = NULL;
    const MachODsymtabCommand *dynamic = NULL;
    for (uint32_t i = 0; i < header.ncmds; ++i) {
        if (!machoSpan(commandEnd, commandOffset, sizeof(MachOLoadCommand)) ||
            (uintptr_t)(image + commandOffset) % _Alignof(MachOSegmentCommand) != 0)
            return invalidMachO(oc, "truncated or unaligned Mach-O load command");
        MachOLoadCommand command;
        memcpy(&command, image + commandOffset, sizeof(command));
        if (command.cmdsize < sizeof(command) ||
            !machoSpan(commandEnd, commandOffset, command.cmdsize))
            return invalidMachO(oc, "invalid Mach-O load-command size");
        /* Preserve the initializer's last-command-wins selection and its
         * existing treatment of LC_SEGMENT using the native segment shape. */
        if (command.cmd == LL_MACHO_LC_SEGMENT || command.cmd == LL_MACHO_LC_SEGMENT_64) {
            if (command.cmdsize < sizeof(*segment))
                return invalidMachO(oc, "truncated Mach-O segment command");
            segment = (const MachOSegmentCommand *)(image + commandOffset);
            if (segment->nsects > (command.cmdsize - sizeof(*segment)) /
                                     sizeof(MachOSection))
                return invalidMachO(oc, "truncated Mach-O section records");
        } else if (command.cmd == LL_MACHO_LC_SYMTAB) {
            if (command.cmdsize < sizeof(*symbols))
                return invalidMachO(oc, "truncated Mach-O symbol command");
            symbols = (const MachOSymtabCommand *)(image + commandOffset);
        } else if (command.cmd == LL_MACHO_LC_DYSYMTAB) {
            if (command.cmdsize < sizeof(*dynamic))
                return invalidMachO(oc, "truncated Mach-O dynamic symbol command");
            dynamic = (const MachODsymtabCommand *)(image + commandOffset);
        }
        commandOffset += command.cmdsize;
    }
    if (segment == NULL || segment->nsects > INT_MAX ||
        !machoAllocationFits(segment->nsects, sizeof(Section), SIZE_MAX))
        return invalidMachO(oc, "invalid Mach-O section count");
    const MachOSection *sections = (const MachOSection *)(segment + 1);
    uint32_t symbolCount = symbols == NULL ? 0 : symbols->nsyms;
    if (symbols != NULL &&
        (!machoTable(image, size, symbols->symoff, symbolCount,
                      sizeof(MachONList), 1) ||
         !machoSpan(size, symbols->stroff, symbols->strsize) ||
         symbolCount > INT_MAX || !machoAllocationFits(symbolCount, sizeof(MachOSymbol), SIZE_MAX) ||
         !machoAllocationFits(symbolCount, sizeof(Symbol_t), SIZE_MAX) ||
         !machoExtrasFit(oc, symbolCount)))
        return invalidMachO(oc, "invalid Mach-O symbol or string table");
    if (dynamic != NULL &&
        !machoTable(image, size, dynamic->indirectsymoff, dynamic->nindirectsyms,
                      sizeof(uint32_t), _Alignof(uint32_t)))
        return invalidMachO(oc, "invalid Mach-O indirect symbol table");

    size_t totals[3] = {0, 0, 0};
    size_t max_alignment[3] = {1, 1, 1};
    for (uint32_t i = 0; i < segment->nsects; ++i) {
        const MachOSection *section = &sections[i];
        unsigned type = section->flags & LL_MACHO_SECTION_TYPE;
        if (type != LL_MACHO_S_ZEROFILL && type != LL_MACHO_S_GB_ZEROFILL &&
            !machoSpan(size, section->offset, section->size))
            return invalidMachO(oc, "Mach-O section extends past the image");
        if (!machoTable(image, size, section->reloff, section->nreloc,
                         sizeof(MachORelocationInfo), 1))
            return invalidMachO(oc, "invalid Mach-O relocation table");
        if ((type == LL_MACHO_S_MOD_INIT_FUNC_POINTERS || type == LL_MACHO_S_MOD_TERM_FUNC_POINTERS) &&
            section->size % sizeof(init_t) != 0)
            return invalidMachO(oc, "invalid Mach-O initializer or finalizer array");
        /* The consumer uses an int shift. Empty sections never shift. */
        if (section->size != 0) {
            if (section->align >= sizeof(int) * CHAR_BIT - 1)
                return invalidMachO(oc, "invalid Mach-O section alignment");
            size_t alignment = (size_t)1 << section->align;
            unsigned kind = type == LL_MACHO_S_GB_ZEROFILL ? 2 :
                type != LL_MACHO_S_MOD_INIT_FUNC_POINTERS && type != LL_MACHO_S_MOD_TERM_FUNC_POINTERS &&
                strncmp(section->segname, "__TEXT", sizeof(section->segname)) == 0
                    ? 0 : 1;
            if (alignment > max_alignment[kind]) max_alignment[kind] = alignment;
            if (!machoAccumulate(&totals[kind], section->size, alignment))
                return invalidMachO(oc, "Mach-O segment size overflow");
        }
        const char *relocs = image + section->reloff;
        for (uint32_t j = 0; j < section->nreloc; ++j) {
            MachORelocationInfo record;
            memcpy(&record, relocs + (size_t)j * sizeof(record), sizeof(record));
            const MachORelocationInfo *reloc = &record;
#if LL_TARGET_AARCH64
            if (reloc->r_type == LL_MACHO_ARM64_RELOC_ADDEND) continue;
#endif
            if (reloc->r_address < 0 ||
                !machoSpan((size_t)section->size, (uint32_t)reloc->r_address,
                            (size_t)1 << reloc->r_length))
                return invalidMachO(oc, "Mach-O relocation operand is outside its section");
#if LL_TARGET_AARCH64
            /* Instruction relocations read uint32_t irrespective of r_length. */
            if (reloc->r_type != LL_MACHO_ARM64_RELOC_UNSIGNED &&
                reloc->r_type != LL_MACHO_ARM64_RELOC_SUBTRACTOR &&
                !machoSpan((size_t)section->size, (uint32_t)reloc->r_address, 4))
                return invalidMachO(oc, "truncated Mach-O instruction relocation");
#elif LL_TARGET_X86_64
            if (!reloc->r_extern &&
                (reloc->r_type == LL_MACHO_X86_64_RELOC_GOT ||
                 reloc->r_type == LL_MACHO_X86_64_RELOC_GOT_LOAD))
                return invalidMachO(oc, "Mach-O GOT relocation requires r_extern");
#endif
            if (machoRelocSymbol(reloc)) {
                if (reloc->r_symbolnum >= symbolCount)
                    return invalidMachO(oc, "invalid Mach-O relocation symbol index");
#if LL_TARGET_X86_64
            } else if (reloc->r_symbolnum == 0 ||
                       reloc->r_symbolnum > segment->nsects ||
                       sections[reloc->r_symbolnum - 1].size == 0) {
                return invalidMachO(oc, "invalid Mach-O relocation section index");
#endif
            }
        }
    }
    size_t compound = 0;
    size_t page = ll_page_size_for(oc->L);
    if (page == 0 || (page & (page - 1)) != 0)
        return invalidMachO(oc, "invalid Mach-O allocation page size");
    for (size_t i = 0; i < 3; ++i) {
        size_t padding = max_alignment[i] > page ? max_alignment[i] - page : 0;
        if (!machoAccumulate(&totals[i], padding, 1))
            return invalidMachO(oc, "Mach-O segment alignment padding overflow");
        if (totals[i] > SIZE_MAX - (page - 1))
            return invalidMachO(oc, "Mach-O segment rounding overflow");
        size_t rounded = ll_round_up_to_page_for(oc->L, totals[i]);
        if (rounded > (size_t)PTRDIFF_MAX - compound)
            return invalidMachO(oc, "Mach-O compound allocation overflow");
        compound += rounded;
    }
    size_t common = 0;
    const char *strings = symbols == NULL ? NULL : image + symbols->stroff;
    size_t terminatedSize = symbols == NULL ? 0 : symbols->strsize;
    /* A name has a terminator exactly when it starts before or at the last
     * NUL. Find that bound once; unused trailing bytes need not be zero. */
    while (terminatedSize != 0 && strings[terminatedSize - 1] != '\0')
        --terminatedSize;
    for (uint32_t i = 0; i < symbolCount; ++i) {
        MachONList record;
        memcpy(&record, image + symbols->symoff + (size_t)i * sizeof(record),
               sizeof(record));
        const MachONList *symbol = &record;
        uint32_t name = symbol->n_un.n_strx;
        if (name >= terminatedSize)
            return invalidMachO(oc, "invalid Mach-O symbol name");
        if ((symbol->n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_SECT &&
            (symbol->n_sect == LL_MACHO_NO_SECT || symbol->n_sect > segment->nsects))
            return invalidMachO(oc, "invalid Mach-O symbol section index");
        if (!(symbol->n_type & LL_MACHO_N_STAB) && (symbol->n_type & LL_MACHO_N_EXT) &&
            (symbol->n_type & LL_MACHO_N_TYPE) == LL_MACHO_N_UNDF && symbol->n_value != 0 &&
            (oc->L->symhash == NULL ||
             !lookupStrHashTable(oc->L->symhash, image + symbols->stroff + name))) {
            if (symbol->n_value > (size_t)PTRDIFF_MAX - common)
                return invalidMachO(oc, "Mach-O COMMON allocation overflow");
            common += (size_t)symbol->n_value;
        }
    }
    return 1;
}
#endif /* OBJFORMAT_MACHO */
