// Exercise the real RTS predicate, including unrelated local symbols that
// fillGot cannot resolve. The companion elf_local_got test loads an object.
#include <elf.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// needGotSlot is an RTS-private function. Use its native ELF symbol type
// without depending on the rest of the RTS's internal object-code structures.
#if defined(__LP64__) || defined(_LP64)
typedef Elf64_Sym TestElfSym;
#else
typedef Elf32_Sym TestElfSym;
#endif
extern bool needGotSlot(TestElfSym *symbol);

static void check_symbol(unsigned binding, unsigned type, unsigned short section,
                         unsigned name, bool expected)
{
    TestElfSym symbol = {0};
    symbol.st_info = (binding << 4) | type;
    symbol.st_shndx = section;
    symbol.st_name = name;
    if (needGotSlot(&symbol) != expected) {
        fprintf(stderr, "unexpected GOT selection: bind=%u type=%u section=%u name=%u\n",
                binding, type, (unsigned)section, name);
        exit(1);
    }
}

int main(void)
{
    check_symbol(STB_LOCAL, STT_OBJECT, 1, 1, true);
    check_symbol(STB_LOCAL, STT_FUNC, 1, 1, true);
    check_symbol(STB_LOCAL, STT_NOTYPE, 1, 1, true);
    check_symbol(STB_LOCAL, STT_TLS, 1, 1, false);
    check_symbol(STB_LOCAL, STT_FILE, SHN_ABS, 1, false);
    check_symbol(STB_LOCAL, STT_OBJECT, SHN_ABS, 1, false);
    check_symbol(STB_LOCAL, STT_OBJECT, SHN_UNDEF, 1, false);
    check_symbol(STB_LOCAL, STT_OBJECT, 1, 0, false);
    check_symbol(STB_LOCAL, STT_SECTION, 1, 0, true);
    check_symbol(STB_GLOBAL, STT_NOTYPE, SHN_UNDEF, 1, true);
    check_symbol(STB_WEAK, STT_FUNC, 1, 1, true);
#if defined(SHN_XINDEX)
    check_symbol(STB_LOCAL, STT_OBJECT, SHN_XINDEX, 1, true);
#endif
    return 0;
}
