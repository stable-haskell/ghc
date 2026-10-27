#define _GNU_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#define LL_INTERNAL_SOURCE 1
#include "ll_linker.h"
#include "linker/ElfTypes.h"
#include "linker/elf_reloc_riscv64.c"

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#if !defined(riscv64_HOST_ARCH) || !defined(OBJFORMAT_ELF)
#error "riscv64_reloc_errors requires the generated riscv64 ELF target"
#endif

#define CHECK(condition, message) do { \
  if (!(condition)) { \
    (void)fprintf(stderr, "riscv64_reloc_errors:%d: %s\n", __LINE__, message); \
    ++failures; \
  } \
} while (0)

static unsigned failures;
static unsigned test_log_calls;
static unsigned default_log_calls;
static unsigned default_fatal_calls;
static ll_linker linker;

static void count_test_log(void *ctx, int kind, const char *fmt, va_list ap)
{
  (void)ctx;
  (void)kind;
  (void)fmt;
  (void)ap;
  ++test_log_calls;
}

static void count_default_log(void *ctx, int kind, const char *fmt, va_list ap)
{
  (void)ctx;
  (void)kind;
  (void)fmt;
  (void)ap;
  ++default_log_calls;
}

static void fail_on_default_fatal(void *ctx, const char *fmt, va_list ap)
{
  (void)ctx;
  (void)fmt;
  (void)ap;
  ++default_fatal_calls;
  (void)fprintf(stderr, "riscv64_reloc_errors: unexpected default fatal callback\n");
  _Exit(98);
}

static void fail_on_fatal(void *ctx, const char *fmt, va_list ap)
{
  (void)ctx;
  (void)fmt;
  (void)ap;
  (void)fprintf(stderr, "riscv64_reloc_errors: unexpected fatal callback\n");
  _Exit(99);
}

static void reset_error(void)
{
  ll_error_clear(&linker);
}

static bool has_error(ll_status status)
{
  return linker.error_status == status && linker.error_message[0] != '\0';
}

static void test_instruction_boundaries(void)
{
  uint32_t word = UINT32_C(0x00000063);
  CHECK(setBType(&linker, &word, 4094) && word == UINT32_C(0x7e000fe3),
        "B-type positive boundary differs from LLVM assembler oracle");
  word = UINT32_C(0x00000063);
  CHECK(setBType(&linker, &word, -4096) && word == UINT32_C(0x80000063),
        "B-type negative boundary differs from LLVM assembler oracle");

  word = UINT32_C(0x0000006f);
  CHECK(setJType(&linker, &word, 1048574)
            && word == UINT32_C(0x7ffff06f),
        "J-type positive boundary differs from LLVM assembler oracle");
  word = UINT32_C(0x0000006f);
  CHECK(setJType(&linker, &word, -1048576)
            && word == UINT32_C(0x8000006f),
        "J-type negative boundary differs from LLVM assembler oracle");

  uint16_t half = UINT16_C(0xc001);
  CHECK(setCBType(&linker, &half, 254) && half == UINT16_C(0xcc7d),
        "CB-type positive boundary differs from LLVM assembler oracle");
  half = UINT16_C(0xc001);
  CHECK(setCBType(&linker, &half, -256) && half == UINT16_C(0xd001),
        "CB-type negative boundary differs from LLVM assembler oracle");
  half = UINT16_C(0xa001);
  CHECK(setCJType(&linker, &half, 2046) && half == UINT16_C(0xaffd),
        "CJ-type positive boundary differs from LLVM assembler oracle");
  half = UINT16_C(0xa001);
  CHECK(setCJType(&linker, &half, -2048) && half == UINT16_C(0xb001),
        "CJ-type negative boundary differs from LLVM assembler oracle");
}

static void test_range_errors(void)
{
  uint32_t word = UINT32_C(0x12345678);
  reset_error();
  CHECK(!setBType(&linker, &word, 4096) && word == UINT32_C(0x12345678),
        "B-type overflow changed instruction bytes");
  CHECK(has_error(LL_E_RELOC_RANGE), "B-type overflow status or diagnostic missing");

  word = UINT32_C(0x12345678);
  reset_error();
  CHECK(!setJType(&linker, &word, 1048576) && word == UINT32_C(0x12345678),
        "J-type overflow changed instruction bytes");
  CHECK(has_error(LL_E_RELOC_RANGE), "J-type overflow status or diagnostic missing");

  uint16_t half = UINT16_C(0x1234);
  reset_error();
  CHECK(!setCBType(&linker, &half, 256) && half == UINT16_C(0x1234),
        "CB-type overflow changed instruction bytes");
  CHECK(has_error(LL_E_RELOC_RANGE), "CB-type overflow status or diagnostic missing");

  half = UINT16_C(0x1234);
  reset_error();
  CHECK(!setCJType(&linker, &half, 2048) && half == UINT16_C(0x1234),
        "CJ-type overflow changed instruction bytes");
  CHECK(has_error(LL_E_RELOC_RANGE), "CJ-type overflow status or diagnostic missing");
}

static void test_compressed_read_boundary(void)
{
  long page_result = sysconf(_SC_PAGESIZE);
  CHECK(page_result > 0, "cannot obtain page size for compressed-read guard");
  if (page_result <= 0) return;
  size_t page = (size_t)page_result;
  uint8_t *mapping = mmap(NULL, page * 2, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  CHECK(mapping != MAP_FAILED, "cannot allocate compressed-read guard pages");
  if (mapping == MAP_FAILED) return;
  if (mprotect(mapping + page, page, PROT_NONE) != 0) {
    CHECK(false, "cannot protect compressed-read guard page");
    (void)munmap(mapping, page * 2);
    return;
  }
  uint16_t *last_half = (uint16_t *)(void *)(mapping + page - sizeof(uint16_t));
  *last_half = UINT16_C(0x1234);
  uint16_t before = *last_half;
  reset_error();
  CHECK(!setCBType(&linker, last_half, 256) && *last_half == before,
        "2-byte compressed range failure read beyond its instruction");
  CHECK(has_error(LL_E_RELOC_RANGE),
        "guarded compressed range failure status or diagnostic missing");
  CHECK(munmap(mapping, page * 2) == 0, "guard-page cleanup failed");
}

static void setup_object(ObjectCode *oc, struct ObjectCodeFormatInfo *info,
                         Section *section, ElfSymbolTable *symtab,
                         ElfSymbol *symbols, uint32_t *bytes, size_t byte_count)
{
  memset(oc, 0, sizeof(*oc));
  memset(info, 0, sizeof(*info));
  memset(section, 0, sizeof(*section));
  oc->L = &linker;
  oc->info = info;
  oc->sections = section;
  oc->n_sections = 1;
  section->start = bytes;
  section->exec_start = (uintptr_t)bytes;
  section->size = (ll_word)byte_count;
  section->kind = SECTIONKIND_CODE_OR_RODATA;
  info->symbolTables = symtab;
  symtab->index = 1;
  symtab->symbols = symbols;
  symtab->n_symbols = 3;
}

static void test_full_loop_failures(void)
{
  uint32_t bytes[4] = {UINT32_C(0x00000063), UINT32_C(0x00000013), 0, 0};
  ObjectCode oc;
  struct ObjectCodeFormatInfo info;
  Section section;
  ElfSymbolTable symtab = {0};
  ElfSymbol symbols[3] = {0};
  ll_elf_target_sym elf_symbol = {0};
  char symbol_name[] = "target";
  void *symbol_addr = (void *)(uintptr_t)UINT32_C(0x1000);
  symbols[1].name = symbol_name;
  symbols[1].addr = symbol_addr;
  symbols[1].elf_sym = &elf_symbol;
  setup_object(&oc, &info, &section, &symtab, symbols, bytes, sizeof(bytes));
  ll_elf_target_shdr rel_shdr = {0};
  rel_shdr.sh_link = 1;

  /* The same relocation succeeds at +4094, then propagates both out-of-range
   * endpoints from encodeAddend through the real relocation loop. */
  ll_elf_target_rela branch = {0};
  branch.r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_BRANCH);
  ElfRelocationATable branch_table = {0};
  branch_table.sectionHeader = &rel_shdr;
  branch_table.targetSectionIndex = 0;
  branch_table.relocations = &branch;
  branch_table.n_relocations = 1;
  info.relaTable = &branch_table;
  symbols[1].addr = (void *)((uintptr_t)section.start + (uintptr_t)4094);
  bytes[0] = UINT32_C(0x00000063);
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_SUCCESS
            && bytes[0] == UINT32_C(0x7e000fe3),
        "valid R_RISCV_BRANCH did not encode its positive boundary");

  bytes[0] = UINT32_C(0x00000063);
  symbols[1].addr = (void *)((uintptr_t)section.start + (uintptr_t)4096);
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_FAILURE
            && bytes[0] == UINT32_C(0x00000063),
        "positive branch range failure did not propagate or changed bytes");
  CHECK(has_error(LL_E_RELOC_RANGE),
        "positive branch range failure status or diagnostic missing");

  symbols[1].addr = (void *)((uintptr_t)section.start - (uintptr_t)4098);
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_FAILURE
            && bytes[0] == UINT32_C(0x00000063),
        "negative branch range failure did not propagate or changed bytes");
  CHECK(has_error(LL_E_RELOC_RANGE),
        "negative branch range failure status or diagnostic missing");

  ll_elf_target_rela unknown = {0};
  unknown.r_info = LL_ELF64_R_INFO(1, UINT64_C(0x7fff));
  ElfRelocationATable unknown_table = {0};
  rel_shdr.sh_link = 1;
  unknown_table.sectionHeader = &rel_shdr;
  unknown_table.targetSectionIndex = 0;
  unknown_table.relocations = &unknown;
  unknown_table.n_relocations = 1;
  info.relaTable = &unknown_table;
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_FAILURE,
        "unknown relocation did not fail the relocation loop");
  CHECK(has_error(LL_E_UNSUPPORTED),
        "unknown relocation lost unsupported status or diagnostic");

  ll_elf_target_rel implicit = {0};
  implicit.r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_32);
  ElfRelocationTable rel_table = {0};
  rel_table.sectionHeader = &rel_shdr;
  rel_table.targetSectionIndex = 0;
  rel_table.relocations = &implicit;
  rel_table.n_relocations = 1;
  info.relaTable = NULL;
  info.relTable = &rel_table;
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_FAILURE,
        "SHT_REL relocation did not fail the relocation loop");
  CHECK(has_error(LL_E_UNSUPPORTED),
        "SHT_REL lost unsupported status or diagnostic");

  ll_elf_target_rela missing_lo = {0};
  missing_lo.r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_PCREL_LO12_I);
  symbols[1].addr = &bytes[1];
  ElfRelocationATable missing_table = {0};
  missing_table.sectionHeader = &rel_shdr;
  missing_table.targetSectionIndex = 0;
  missing_table.relocations = &missing_lo;
  missing_table.n_relocations = 1;
  info.relTable = NULL;
  info.relaTable = &missing_table;
  reset_error();
  unsigned logs_before_missing_hi = test_log_calls;
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_FAILURE,
        "PCREL_LO12 without a matching HI20 did not fail the loop");
  CHECK(has_error(LL_E_FORMAT),
        "missing PCREL_LO12 HI20 pair lost format status or diagnostic");
  CHECK(test_log_calls == logs_before_missing_hi + 1,
        "missing-HI diagnostic did not use the test-linker log callback");
}

static void test_paired_u_type_and_riscv64_width(void)
{
  uint32_t bytes[4] = {UINT32_C(0x00000297), UINT32_C(0x00028313), 0, 0};
  ObjectCode oc;
  struct ObjectCodeFormatInfo info;
  Section section;
  ElfSymbolTable symtab = {0};
  ElfSymbol symbols[3] = {0};
  ll_elf_target_sym elf_symbols[3] = {{0}};
  char *names[3] = {"", "target", "hi_anchor"};
  void *addresses[3] = {0};
  symbols[1].name = names[1];
  symbols[1].elf_sym = &elf_symbols[1];
  symbols[2].name = names[2];
  symbols[2].elf_sym = &elf_symbols[2];
  setup_object(&oc, &info, &section, &symtab, symbols, bytes, sizeof(bytes));
  addresses[1] = (void *)((uintptr_t)section.start + (uintptr_t)0x12345);
  addresses[2] = section.start;
  symbols[1].addr = addresses[1];
  symbols[2].addr = addresses[2];

  ll_elf_target_shdr rel_shdr = {0};
  rel_shdr.sh_link = 1;
  ll_elf_target_rela pair[2] = {{0}};
  pair[0].r_offset = 0;
  pair[0].r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_PCREL_HI20);
  pair[0].r_addend = 0;
  pair[1].r_offset = 4;
  pair[1].r_info = LL_ELF64_R_INFO(2, LL_ELF_R_RISCV_PCREL_LO12_I);
  pair[1].r_addend = 0;
  ElfRelocationATable pair_table = {0};
  pair_table.sectionHeader = &rel_shdr;
  pair_table.targetSectionIndex = 0;
  pair_table.relocations = pair;
  pair_table.n_relocations = 2;
  info.relaTable = &pair_table;
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_SUCCESS,
        "paired PCREL_HI20/LO12_I failed after corrected U-type rounding");
  CHECK(linker.error_status == LL_OK,
        "successful paired U-type relocation left a diagnostic");
  CHECK(bytes[0] == UINT32_C(0x00012297)
            && bytes[1] == UINT32_C(0x34528313),
        "paired U-type words differ from the LLVM assembler oracle");

  uint64_t data[2] = {UINT64_MAX, UINT64_C(0xfeedfacecafebeef)};
  section.start = data;
  section.exec_start = (uintptr_t)data;
  section.size = sizeof(data);
  ll_elf_target_rela rel64 = {0};
  rel64.r_info = LL_ELF64_R_INFO(1, LL_ELF_R_RISCV_64);
  ElfRelocationATable width_table = {0};
  width_table.sectionHeader = &rel_shdr;
  width_table.targetSectionIndex = 0;
  width_table.relocations = &rel64;
  width_table.n_relocations = 1;
  info.relaTable = &width_table;
  addresses[1] = (void *)(uintptr_t)UINT32_C(0x12345678);
  symbols[1].addr = addresses[1];
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_SUCCESS,
        "R_RISCV_64 positive relocation failed");
  CHECK(data[0] == UINT64_C(0x12345678)
            && data[1] == UINT64_C(0xfeedfacecafebeef),
        "R_RISCV_64 did not write eight bytes or overwrote its neighbor");

  data[0] = UINT64_MAX;
  addresses[1] = (void *)(uintptr_t)UINT64_C(0x1234567801234567);
  symbols[1].addr = addresses[1];
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_SUCCESS
            && linker.error_status == LL_OK,
        "R_RISCV_64 high-bit positive narrowing control failed");
  CHECK(data[0] == UINT64_C(0x0000000001234567)
            && data[1] == UINT64_C(0xfeedfacecafebeef),
        "R_RISCV_64 positive 32-bit narrowing changed unexpectedly");

  data[0] = UINT64_MAX;
  addresses[1] = (void *)(uintptr_t)UINT64_C(0x12345678abcdef01);
  symbols[1].addr = addresses[1];
  reset_error();
  CHECK(relocateObjectCodeRISCV64(&oc) == EXIT_SUCCESS
            && linker.error_status == LL_OK,
        "R_RISCV_64 high-bit negative narrowing control failed");
  CHECK(data[0] == UINT64_C(0xffffffffabcdef01)
            && data[1] == UINT64_C(0xfeedfacecafebeef),
        "R_RISCV_64 negative 32-bit narrowing changed unexpectedly");
}

int main(void)
{
  linker.services.host.log = count_test_log;
  linker.services.host.fatal = fail_on_fatal;
  ll_default_linker.services.host.log = count_default_log;
  ll_default_linker.services.host.fatal = fail_on_default_fatal;
  ll_default_linker.error_status = LL_E_NOT_FOUND;
  memcpy(ll_default_linker.error_message, "default-linker-sentinel",
         sizeof("default-linker-sentinel"));
  test_instruction_boundaries();
  test_range_errors();
  test_compressed_read_boundary();
  test_full_loop_failures();
  test_paired_u_type_and_riscv64_width();
  CHECK(ll_default_linker.error_status == LL_E_NOT_FOUND
            && strcmp(ll_default_linker.error_message,
                      "default-linker-sentinel") == 0,
        "operation changed the default-linker error sentinel");
  CHECK(default_log_calls == 0 && default_fatal_calls == 0,
        "operation called a default-linker log or fatal callback");
  if (failures != 0) return 1;
  return 0;
}
