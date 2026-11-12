/* Exact-width controls for the real AArch64 instruction classifiers. */
#define LL_INTERNAL_SOURCE 1
#include "linker/elf_compat.h"
#include "linker/elf_reloc_aarch64.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "linker/elf_reloc_aarch64.c"
#include "linker/elf_plt.c"
#include "linker/elf_plt_aarch64.c"
#include "linker/elf_util.c"

static unsigned failures;
#define CHECK(test) do { if (!(test)) { \
    (void)fprintf(stderr, "classifier:%d: %s\n", __LINE__, #test); ++failures; \
} } while (0)

int main(void)
{
    uint32_t *word = malloc(sizeof(*word));
    if (word == NULL) return 1;
    *word = UINT32_C(0x14000000);
    CHECK(isBranch((uintptr_t)word));
    CHECK(!isBranchLink((uintptr_t)word));
    *word = UINT32_C(0x94000000);
    CHECK(isBranchLink((uintptr_t)word));
    CHECK(!isBranch((uintptr_t)word));
    *word = UINT32_C(0x90000000);
    CHECK(isAdrp((uintptr_t)word));
    CHECK(!isLoadStore((uintptr_t)word));
    *word = UINT32_C(0xf9000022);
    CHECK(isLoadStore((uintptr_t)word));
    *word = UINT32_C(0x11000000);
    CHECK(isAddSub((uintptr_t)word));
    *word = UINT32_C(0x04800000);
    CHECK(isVectorOp((uintptr_t)word));
    *word = 0;
    CHECK(!isAdrp((uintptr_t)word) && !isAddSub((uintptr_t)word) &&
          !isLoadStore((uintptr_t)word) && !isVectorOp((uintptr_t)word));
    free(word);
    if (failures != 0) return 1;
    (void)puts("elf_aarch64_classifier: PASS");
    return 0;
}
