#pragma once

#include <LinkerInternals.h>

#include "elf_plt_arm.h"
#include "elf_plt_aarch64.h"
#include "elf_plt_riscv64.h"

#if LL_TARGET_ARM || LL_TARGET_AARCH64 || LL_TARGET_RISCV64

#if defined(OBJFORMAT_ELF)

#if LL_TARGET_AARCH64
#define __suffix__ Aarch64
#elif LL_TARGET_ARM
#define __suffix__ Arm
#elif LL_TARGET_RISCV64
#define __suffix__ RISCV64
#else
#error "unknown ELF PLT target"
#endif

#define PASTE(x,y) x ## y
#define EVAL(x,y) PASTE(x,y)
#define ADD_SUFFIX(x) EVAL(PASTE(x,),__suffix__)

unsigned  numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex);

#define STUB_SIZE          ADD_SUFFIX(stubSize)

bool findStub(Section * section, void* * addr, uint8_t flags);
bool makeStub(ll_linker *L, Section * section, void* * addr, void* got_addr, uint8_t flags);

void freeStubs(ll_linker *L, Section * section);

#endif // OBJECTFORMAT_ELF

#endif // arm/aarch64_HOST_ARCH/riscv64_HOST_ARCH
