/* Compile-time target controls; this file emits no target instructions. */
#include "ll_target.h"

#if defined(LL_EXPECT_TARGET)
_Static_assert(LL_TARGET_ARCH == LL_EXPECT_TARGET, "explicit target was ignored");
_Static_assert(LL_TARGET_WORD_SIZE == LL_EXPECT_WORD_SIZE,
               "object word size followed the compiler instead of the target");
_Static_assert(LL_ELF_NEED_PLT == LL_EXPECT_ELF_PLT, "wrong ELF PLT capability");
_Static_assert(LL_ELF_NEED_GOT == LL_EXPECT_ELF_PLT, "wrong ELF GOT capability");
_Static_assert(LL_ELF_NEED_SYMBOL_EXTRAS == LL_EXPECT_ELF_EXTRAS,
               "wrong ELF symbol-extra capability");
_Static_assert(LL_MACHO_NEED_PLT == LL_EXPECT_MACHO_PLT,
               "wrong Mach-O PLT capability");
_Static_assert(LL_TARGET_SYMBOL_EXTRA_SIZE == LL_EXPECT_EXTRA_SIZE,
               "symbol payload layout followed the host instead of the target");
#else
_Static_assert(LL_TARGET_WORD_SIZE == sizeof(void *),
               "native target word size differs from the configured compiler");
#endif

int ll_target_config_control(void) { return LL_TARGET_ARCH; }
