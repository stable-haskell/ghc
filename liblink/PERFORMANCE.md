# Native linker optimisation in debug GHC builds

The static GHCi startup/load benchmark measured on 2026-10-22 fell from
0.55 s before extraction to 0.46 s afterward: 16.36% less elapsed time.
The benefit came from the native C compilation settings. The original
linker inherited the debug RTS's explicit `-O0`; the separately compiled
liblink inherited effective `-O2` from the Nix compiler wrapper in that
build environment. Assertions remained enabled.

A controlled experiment rebuilt the same 29 native C files and relinked
against the same Haskell executable objects. Adding only `-optc-O0`
restored the original runtime and retired-instruction count. A rebuild
using the original native flags reproduced the shipped candidate archive
byte for byte.

These diagnostic medians are separate from the original benchmark:

| Configuration | Elapsed time | Instructions retired |
| --- | ---: | ---: |
| Original compiler | 0.54 s | 5.70 billion |
| Extracted candidate | 0.45 s | 4.69 billion |
| Extracted candidate with explicit C `-O0` | 0.54 s | 5.71 billion |

The diagnostic experiment used 64 measured calls and 16 warmups. CPU time
outside garbage collection fell from 239 ms to 152 ms; GC stayed around
273 ms. All four executable variants evaluated all 200 expected exported
values correctly, giving 800 checked values.

An otherwise debug or unoptimised GHC can therefore benefit from compiling
liblink independently with optimisation when the workload spends enough
time in the runtime linker. Haskell optimisation, native C optimisation
and assertions are separate controls. The effective C flags depend on the
build environment and should be checked explicitly.

The experiment did not test LTO or more aggressive linker flags. It does
not establish the same benefit for general batch compilation or an
already optimised release build. Sampling profiles implicate symbol
hashing and lookup, but all 29 native files were varied, so hashing's exact
share of the saving remains unmeasured.
