#pragma once

#include "LinkerInternals.h"
#include "ll_backend.h"
#include "linker/ElfTypes.h"

#if LL_BUILD_ELF

extern const struct ll_reloc_ops ll_elf_aarch64_reloc_ops;

bool
relocateObjectCodeAarch64(ObjectCode * oc);

/* Zero means success, one means failure. The addend is already computed;
 * only site.work is accessed. Callers retain the existing storage alignment
 * and readable-width requirements, including the instruction classifiers. */
bool encodeAddendAarch64Site(ll_linker *L, ll_site site, ll_elf_target_rel *rel,
                            int64_t addend);
bool encodeAddendAarch64(ll_linker *L, Section *section, ll_elf_target_rel *rel,
                        int64_t addend);

void flushInstructionCacheAarch64(ObjectCode *oc);
#endif /* OBJETFORMAT_ELF */
