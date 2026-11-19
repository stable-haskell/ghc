/* Header for MicroHs stage1 RTS stubs (see mhs_rts_stubs.c). */
#ifndef MHS_RTS_STUBS_H
#define MHS_RTS_STUBS_H

#include <stdint.h>

extern uint64_t ghc_unique_counter64;
extern intptr_t ghc_unique_inc;

intptr_t rts_isProfiled(void);
intptr_t rts_isDynamic(void);
intptr_t rts_isThreaded(void);
intptr_t rts_isDebugged(void);
intptr_t rts_isTracing(void);

void *getOrSetLibHSghcGlobalHasPprDebug(void *p);
void *getOrSetLibHSghcGlobalHasNoDebugOutput(void *p);
void *getOrSetLibHSghcGlobalHasNoStateHack(void *p);

/* IEEE-754 predicates (GHC.Internal.Float FFI).
 * Haskell `Float`/`Double` map to C `float`/`double` and `Int` to
 * `intptr_t` in MicroHs (see MicroHs HsFFI.h: HsInt/HsFloat/HsDouble). */
intptr_t isFloatNaN(float f);
intptr_t isFloatInfinite(float f);
intptr_t isFloatDenormalized(float f);
intptr_t isFloatNegativeZero(float f);
intptr_t isFloatFinite(float f);

intptr_t isDoubleNaN(double d);
intptr_t isDoubleInfinite(double d);
intptr_t isDoubleDenormalized(double d);
intptr_t isDoubleNegativeZero(double d);
intptr_t isDoubleFinite(double d);

/* Round to nearest, ties to even (GHC.Internal.Float.RealFracMethods FFI). */
double rintDouble(double d);
float rintFloat(float f);

/* Small-Integer to Double conversion (GHC.Internal.Bignum.Primitives FFI:
 * intEncodeDouble# :: Int# -> Int# -> Double#,
 * wordEncodeDouble# :: Word# -> Int# -> Double#).
 * On 64-bit MicroHs targets Int#/Word# are word-sized, so int64_t/uint64_t
 * match intptr_t/uintptr_t exactly. */
double __int_encodeDouble(int64_t j, intptr_t e);
double __word_encodeDouble(uint64_t j, intptr_t e);

#endif /* MHS_RTS_STUBS_H */
