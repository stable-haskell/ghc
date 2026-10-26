Owner-scoped host services
==========================

`owner_services.c` exercises the private host-service interface directly. It
checks that owner-scoped calls use each linker's service table, while legacy
macros continue to use the default linker. It covers allocator provenance,
separate page sizes and debug masks, logging and `errno`, callback routing,
and single evaluation of macro inputs. Build once with assertions enabled
and once without them; the disabled build also checks that `LL_ASSERT_FOR`
does not evaluate its owner or predicate.

Set `LL_CONFIG_DIR` to the generated liblink configuration headers. Compile
`ll_host.c` directly so the test exercises the same private source interface
without depending on a packaged linker library:

    : "${LL_CONFIG_DIR:?set the generated liblink include directory}"
    CC=${CC:-cc}
    "$CC" -std=c11 -Wall -Wextra -Werror=vla -pedantic -DLL_ASSERTS \
      -isystem "$LL_CONFIG_DIR" -Iliblink/include -Iliblink/src \
      liblink/tests/unit/owner_services.c \
      liblink/src/ll_host.c -o owner-services
    ./owner-services

Repeat without `-DLL_ASSERTS` to check the disabled assertion path. The
test exits nonzero with a concise diagnostic on failure and is silent on
success. It exercises service routing only; it does not load object files.
The optional `fatal`, `check`, `checkm`, and `assert` modes call the fatal
callback with the second owner's services. Expected statuses are 102 for
`fatal`, `check`, and `checkm` in both builds; `assert` returns 102 with
assertions enabled and 0 without them.

Hash table owner destruction
============================

`hash_destroy_owner.c` links the actual hash and host implementations to check
that owned values and table storage use the table owner's allocator, including
the empty-table path. It also checks that `freeHashTable` retains its
caller-provided value callback and callback order. Set `LIBLINK_BUILD_INCLUDE`
to the configured liblink build `include` directory, then run from the
repository root:

    : "${LIBLINK_BUILD_INCLUDE:?set the configured liblink include directory}"
    CC=${CC:-cc}
    "$CC" -std=c11 -Wall -Wextra -Werror=vla -pedantic -DLL_INTERNAL_SOURCE=1 \
      -isystem "$LIBLINK_BUILD_INCLUDE" -Iliblink/include -Iliblink/src \
      liblink/tests/unit/hash_destroy_owner.c liblink/src/ll_hash.c \
      liblink/src/ll_host.c -o hash-destroy-owner
    ./hash-destroy-owner

Repeat with `-DLL_ASSERTS` to run the assertion build. The test is silent on
success and exits nonzero with a diagnostic on failure.

Private linker error state
==========================

`error_state.c` checks status/diagnostic storage, owner isolation, formatting
boundaries, errno preservation, and that recording an error does not call host
allocation or diagnostic hooks. Run from the repository root and set
`LL_CONFIG_DIR` to the generated include directory from a static liblink build.

    : "${LL_CONFIG_DIR:?set the generated static liblink include directory}"
    CC=${CC:-cc}
    "$CC" -std=c11 -Wall -Wextra -Werror=vla -pedantic -DLL_ASSERTS \
      -isystem "$LL_CONFIG_DIR" -Iliblink/include -Iliblink/src \
      liblink/tests/unit/error_state.c liblink/src/ll_error.c \
      liblink/src/ll_host.c -o error-state-asserts
    ./error-state-asserts
    "$CC" -std=c11 -Wall -Wextra -Werror=vla -pedantic \
      -isystem "$LL_CONFIG_DIR" -Iliblink/include -Iliblink/src \
      liblink/tests/unit/error_state.c liblink/src/ll_error.c \
      liblink/src/ll_host.c -o error-state-noasserts
    ./error-state-noasserts
    "$CC" -std=c11 -Wall -Wextra -Werror=vla -pedantic -DLL_ASSERTS \
      -fsanitize=address,undefined -fno-omit-frame-pointer \
      -isystem "$LL_CONFIG_DIR" -Iliblink/include -Iliblink/src \
      liblink/tests/unit/error_state.c liblink/src/ll_error.c \
      liblink/src/ll_host.c -o error-state-sanitized
    ./error-state-sanitized

Core linker error statuses and mmap failure
===========================================

`core_error_status.c` includes the current private `Linker.c` and links its
dependencies from a freshly rebuilt static `liblink.a`. It covers the core
error statuses and checks that a NULL mapping result closes the input file
descriptor before returning. Set `LL_CONFIG_DIR` and `RTS_FS_INCLUDE` to the
generated liblink and `rts-fs` include directories, and `LIBLINK_BUILD_LIB` to
the directory containing the fresh archive. The link-map option below uses
Darwin linker syntax.

    : "${LL_CONFIG_DIR:?set the generated liblink include directory}"
    : "${RTS_FS_INCLUDE:?set the generated rts-fs include directory}"
    : "${LIBLINK_BUILD_LIB:?set the directory containing fresh liblink.a}"
    CC=${CC:-cc}
    "$CC" -std=c11 -Wall -Wextra -Werror=vla -pedantic -DLL_ASSERTS \
      -isystem "$LL_CONFIG_DIR" -isystem "$RTS_FS_INCLUDE" \
      -Iliblink/include -Iliblink/src \
      liblink/tests/unit/core_error_status.c -L"$LIBLINK_BUILD_LIB" -llink \
      -pthread \
      -Wl,-map,core-error-status.map -o core-error-status
    ./core-error-status

Stage2 native C unit driver
===========================

`symbol_extras_image.c` uses a valid compiler-produced native object to check
heap-to-mapping image replacement. It includes the real shared allocator with
only mapping calls intercepted. Positive controls cover heap images at offsets
0 and 8; the mapped-image regression requires one old mapping release. Three
injected mapping failures retain the original image bytes, allocation owner,
and diagnostic until normal disposal. Every case balances owner allocations.
The original `format_archive` ownership assertion remains unchanged.

`symbol_extras_storage.c` tests the shared allocation primitive with target
strides 0, 8, and 16 independently of native payload selection. It checks
mapping/BSS lengths, zeroing, unchanged input bytes, refresh timing and failure
propagation, and protection lengths. Zero counts, nonzero counts with zero
stride, optional refresh, and separate M32 storage retain their existing paths.
The fixture contains storage bytes only; it does not parse or execute them.
M32 requests use a bounded allocation spy to check owner, RX allocator,
alignment, byte length, and untouched trailing bytes independently of OS
placement. Existing M32 tests retain real mapping and ownership coverage.

After a successful top-level `make stage2`, run the focused units from the
repository root with:

    python3 liblink/tests/unit/run_native_tests.py

The default run also checks target configuration without executing target
instructions. `test_target_config.py` compiles the native default and seven
explicit architectures, checks target word classes, symbol payload sizes and
format capabilities,
compares host macro definitions before and after each override, and requires
an invalid target selector to fail. It can run independently with
`--cc <compiler command> --config-dir <generated include directory>`.

The driver locates the generated `liblink` configuration headers, installed
`rts-fs` headers, and stage2 `native-linker/liblink.a`. It compiles the private
error-state, core status/negative-input, and archive status tests with
assertions. The core and archive fixtures include their implementation source
and link dependencies from the real archive using `-llink`; linker maps check
that the corresponding archive object is not also selected. Before any C
compilation, the driver runs `test_link_trace_checks.py` to check the MinGW LLD
verbose-selection/COFF-map format and both Darwin `-why_load` output forms. On
Darwin, it also runs the Mach-O preflight, indirect-import, and segment-alignment
units. The driver also runs the AArch64 Mach-O stub lifecycle unit on
Darwin/AArch64. It
accepts `--build-root` and `--store-root` when the stage2 build uses
non-default directories, and `--cc` to select a C compiler command.

On Linux and Windows, standalone units also link the installed
`rts-fs-1.0.0.0/libHSrts-fs-1.0.0.0.a` immediately after `-llink`, before
platform libraries. This provides the filesystem symbols referenced by the
ELF and PE backends. The driver fails before compiling native C units when that
archive is missing. Darwin units keep their existing link arguments because
the Darwin backend does not reference these `rts-fs` C symbols. The
`--foundation-only` mode remains independent of the package archive.

Sanitized native C units
========================

`run_sanitized_tests.py` builds a fresh static archive from the current
`native-sources.list` with AddressSanitizer and UndefinedBehaviorSanitizer, then
runs the default native C unit driver against that archive. It supports native
Linux and Darwin hosts, compiles one source at a time, and writes a JSON report
with source/configuration hashes, commands, and the resulting archive hash.
Choose a new or empty output directory. On Linux, enable LeakSanitizer in the
caller environment when checking malformed-input cleanup:

    ASAN_OPTIONS=detect_leaks=1 UBSAN_OPTIONS=halt_on_error=1 \
      python3 liblink/tests/unit/run_sanitized_tests.py \
        --build-root _build/stage2/build/host \
        --store-root _build/stage2/store/host \
        --output-dir /tmp/liblink-sanitized-native

The default overall limit is 900 seconds and each compiler, archiver, or unit
driver command is limited to 90 seconds. Override with `--timeout` and
`--command-timeout` when the host needs different bounds. The runner inherits
the caller's sanitizer environment and does not disable leak detection.
The Linux static CI job enables leak detection and retains the JSON report and
logs in the `liblink-linux-sanitizer-results` artifact.

The core and archive fixtures use POSIX file descriptors and mmap injection.
On Windows, `Linker.c` follows its stdio path (`RTS_LINKER_USE_MMAP=0`); run
only the platform-neutral foundation test with `--foundation-only` until
Windows fixtures cover those paths. Use `--protection-only` to compile and run
only the Darwin Mach-O protection cases while iterating.

Archive input status
====================

`archive_error_status.c` exercises the private archive reader with deterministic
archive-byte fixtures. It checks per-owner status, cleanup, registration, and
default-linker isolation for archive framing, GNU/BSD names, thin-member IO, and
pending-image mapping failures. On AArch64 Darwin it also checks a two-slice FAT archive
whose slices are empty standard archives. The fixture includes `LoadArchive.c`
to interpose only its image-mapping call; its other linker dependencies come
from the built static archive. Set `LL_CONFIG_DIR`, `RTS_FS_INCLUDE`, and
`LIBLINK_BUILD_LIB` to the generated liblink include, generated `rts-fs`
include, and directory containing the freshly built `liblink.a`.

The manual link-map flag below uses Darwin syntax. On Linux, use
`-Wl,-Map,archive-error-status.map` and add `-ldl` to the link flags.

    : "${LL_CONFIG_DIR:?set the generated liblink include directory}"
    : "${RTS_FS_INCLUDE:?set the generated rts-fs include directory}"
    : "${LIBLINK_BUILD_LIB:?set the directory containing fresh liblink.a}"
    CC=${CC:-cc}
    "$CC" -std=c11 -Wall -Wextra -Werror=vla -pedantic -DLL_ASSERTS \
      -isystem "$LL_CONFIG_DIR" -isystem "$RTS_FS_INCLUDE" \
      -Iliblink/include -Iliblink/src \
      liblink/tests/unit/archive_error_status.c -L"$LIBLINK_BUILD_LIB" -llink -pthread \
      -Wl,-map,archive-error-status.map -o archive-error-status
    ./archive-error-status

Legacy ELF x86_64 RelA errors
=============================

`elf_relocation_errors.c` includes the actual ELF implementation to call its
static legacy RelA relocation helper. It checks valid 64-bit and 32-bit writes,
range errors for PC32/32/32S/GOTPCREL-family/PLT32 (including a preallocated
symbol-extra fallback that remains out of range), unsupported relocation and
TLSGD errors, intact failure payloads/canaries, per-owner status, and
default-owner isolation. The driver runs it only when the host and generated
configuration are Linux x86_64 ELF. It links other dependencies from the
generated static archive and checks the linker map for the selected archive
without selecting `Elf.o` beside the included implementation; `ElfVerify.o`
remains allowed.

ELF data-only resource fixtures
===============================

`elf_resource_fixtures.py` writes eight deterministic ELF64 x86_64 object
files for resource tests. The fixtures cover COMMON symbols with
matching, smaller, conflicting, zero, and overflow sizes, plus an ordinary
`.data` symbol and identical provider/alias bytes. Generate them into a private
directory with:

    python3 liblink/tests/unit/elf_resource_fixtures.py /tmp/elf-resource-fixtures

The files contain no code, relocations, or initializers. LLVM 21 independently
verified their ELF metadata, symbol sizes and section identities, and exact
data bytes. This generator does not run the liblink backend or qualify native
runtime behavior.

`elf_section_errors.c` tests `ocGetNames_ELF` from the current native archive.
Its ordinary storage fixtures have file-backed data and zero-fill BSS, with
both section orders. Linker wrappers inject reopen, file mapping, BSS mapping,
and M32 allocation failures. The checks cover IO/NOMEM status, preservation of
a specific M32 error, owner isolation, saved allocation errno, descriptor
closure, and normal `ll_discard_object` cleanup after earlier sections have
already acquired storage. Successful controls check every data/BSS byte and
run again after each failure. No relocated code is executed. COMMON/GOT,
archive-member placement, public load/resolve, and RTS GC are outside this unit.
The driver requires genuine Linux x86_64 generated configuration and links
with literal `-llink`, checking that the map selects Elf, Linker, MMap and
M32Alloc from the expected archive. It does not include replacement backend
source. GNU-compatible `--wrap` support is required.

The section unit has not been compiled or run on native Linux during Darwin
authoring. Its source and Python driver checks do not qualify ELF runtime
behavior. Run the normal driver after a Linux x86_64 static stage2 build.

Darwin and other targets skip this native Linux x86_64 unit. Source presence
does not qualify its runtime behavior; native Linux x86_64 execution is
required. After a static stage2 build on Linux x86_64, run the driver from the
repository root with:

    python3 liblink/tests/unit/run_native_tests.py

For a manual link, set `LL_CONFIG_DIR`, `RTS_FS_INCLUDE`, and
`LIBLINK_BUILD_LIB` to the generated liblink include, generated `rts-fs`
include, and directory containing the fresh `liblink.a`:

    : "${LL_CONFIG_DIR:?set the generated liblink include directory}"
    : "${RTS_FS_INCLUDE:?set the generated rts-fs include directory}"
    : "${LIBLINK_BUILD_LIB:?set the directory containing fresh liblink.a}"
    CC=${CC:-cc}
    "$CC" -std=c11 -Wall -Wextra -Werror=vla -pedantic -DLL_ASSERTS \
      -isystem "$LL_CONFIG_DIR" -isystem "$RTS_FS_INCLUDE" \
      -Iliblink/include -Iliblink/src \
      liblink/tests/unit/elf_relocation_errors.c -L"$LIBLINK_BUILD_LIB" \
      -llink -pthread -ldl -Wl,-Map,elf-relocation-errors.map \
      -o elf-relocation-errors
    ./elf-relocation-errors

RISC-V U-type arithmetic
========================

`riscv_u_type.c` is a portable helper unit generated from the current
`elf_reloc_riscv64.c` by `riscv_u_type_source.py`. The extractor accepts only
one definition of each expected helper and checks its signature and balanced
function body before compiling it with the native `LinkerInternals.h`,
`linker/util.h`, generated host headers, and support from the stage2
`liblink.a`. The unit uses twelve explicit instruction words to check signed
minimum/maximum values, rounding boundaries around +/-2048, ordinary positive
and negative addends, and preservation of several low-12-bit instruction
patterns. The native test driver runs it after the archive unit. To enable
ASan+UBSan for this unit, pass sanitizer options through the driver's existing
`--cc` argument, for example:

    python3 liblink/tests/unit/run_native_tests.py \
      --cc 'clang -fsanitize=address,undefined -fno-sanitize-recover=undefined -fno-omit-frame-pointer'

The regression covers two arithmetic defects in U-type encoding. First,
rounding a signed `int32_t` with `val + 0x800` overflows at `INT32_MAX`; casting
to `uint32_t` first makes the addition defined modulo 2^32. Second,
`signExtend32` takes `(bits, value)`, so passing the operands in that order
checks the rounded high field as a signed 32-bit value before shifting and
checking its 20-bit range. The test exercises the actual extracted definitions,
not a maintained copy. It does not build or execute the RISC-V relocation
backend on this host. The current CI matrix has no RISC-V job, so backend
execution still requires a genuine RISC-V or QEMU qualification.

RISC-V ELF relocation error propagation
=======================================

When run on a native Linux RISC-V ELF build, the unit driver compiles
`riscv64_reloc_errors.c`, which includes the current relocation backend and
links the remaining dependencies from the stage2 `liblink.a`. The link map
checks that the archive does not also select `elf_reloc_riscv64.o`. The unit
checks instruction encodings, range failures and unchanged output bytes,
relocation-loop status propagation, unsupported relocation forms, malformed
PC-relative pairs, and 64-bit relocation writes including the existing
32-bit narrowing behavior. Literal instruction encodings use the LLVM
assembler oracle recorded by the unit; this checkout did not run a RISC-V
assembler or backend. The portable U-type helper unit above still runs on
other hosts. The current CI matrix has no RISC-V job, so this native backend
unit awaits qualification on genuine RISC-V hardware or QEMU; no target
execution is claimed on other architectures.

AArch64 ELF relocation errors
=============================

When run on a native Linux AArch64 ELF build, the unit driver compiles
`aarch64_reloc_errors.c`, which includes the current relocation backend and
links its other dependencies from the stage2 `liblink.a`. Its map check
excludes a duplicate archived `elf_reloc_aarch64.o`. The unit covers 16-,
32-, and 64-bit writes, signed-width and branch/page reach boundaries,
unsupported REL forms, zero-address RelA state errors, `R_AARCH64_NONE`,
scaled-address range failures, instruction-scale format failures, and cached
branch-stub fallback outcomes. It checks status and unchanged target bytes on
errors and does not execute relocated code.

Darwin and other targets skip this Linux AArch64 unit. This checkout has no
native Linux AArch64 runner, and the current AArch64 Linux CI job is disabled;
the source test is uncompiled here and requires native or emulated AArch64
execution for qualification.

Mach-O input preflight
======================

On Darwin, the native unit driver compiles ordinary, `-fcommon`, and type-only
zero-size Mach-O object fixtures, links `macho_input.c` against the stage2 `liblink.a`,
checks the link map for `MachOVerify.o`, and runs malformed-header/command,
section, symbol, relocation, initializer, and COMMON-bound cases. Valid
fixtures pass through `ll_init_object`; valid symbol-bearing objects also pass
through `loadOc`, after which the test discards them and verifies owner cleanup.
The ordinary fixture includes a 32 KiB-aligned `S_ZEROFILL` symbol; the unit
checks that `loadOc` registers it in the owner's symbol hash at the requested
alignment and leaves its value zero.
The unit validates preflight/loading only: it does not resolve symbols or run
initializers. It is native Darwin evidence and does not qualify another host or
architecture. The separate unit below covers legacy indirect-import errors.
Pre-file misalignment error conversion remains outside this preflight slice.

Mach-O packed table alignment
============================

On Darwin, `macho_table_alignment.c` reuses the owner-scoped harness from
`macho_input.c` and loads a data-only fixture with a defined integer, a pointer
relocation, and a `-fcommon` zero-initialized symbol. The test repacks the
symbol and relocation tables at deliberately unaligned file offsets, then
checks aligned input, symbol-only, relocation-only, and combined cases through
`ll_init_object`, `loadOc`, and lookup-triggered relocation resolution. It
checks whether each table is borrowed or held in an owned aligned copy, verifies
the resolved pointer and zeroed COMMON storage, and checks owner teardown.
Invalid symbol and relocation spans and indexes still must return
`LL_E_FORMAT`, including malformed indexes repacked into unaligned tables.
Zero-relocation and zero-symbol-plus-zero-relocation controls verify that empty
tables do not acquire owned copies. A load-time re-verification failure after
`ll_init_object` has copied the unaligned symbol table checks partial cleanup
without violating the host allocator's non-NULL contract. Archive-member state
is left intact through initialization; lookup then checks the real transition
to `OBJECT_RESOLVED`. After discard, all fixture symbols must be absent from
the owner hash. No mapped function is called and no initializer is run.

The fixture contains only data, keeping its relocation target naturally
aligned under both ARM64 and x86_64. The test uses the generated native Mach-O
configuration and is intended to run from the existing Darwin branch of
`run_native_tests.py`; it does not qualify other hosts or architectures.
Run only this unit with `python3 liblink/tests/unit/run_native_tests.py
--macho-table-alignment-only` on a configured Darwin build.
The data-only fixture isolates table storage; it does not qualify the separate
typed relocation-operand alignment behavior observed with a function-bearing
fixture under x86_64 UBSan.

Mach-O indirect-import status
=============================

`macho_import_status.c` is a Darwin-only function-level fixture. It includes
the current `MachO.c` and calls its static indirect-import resolver with
explicit Mach-O metadata, while linking the remaining linker dependencies
from the selected stage2 `liblink.a`. The positive case checks two 64-bit
pointer entries selected by a nonzero `reserved1`, writes to an intentionally
unaligned loaded section, verifies the original image bytes remain untouched,
and checks surrounding canaries. Negative cases cover invalid table spans,
an invalid second symbol index before any writes, unresolved symbols,
LOCAL/ABS entries, and the approved `__jump_table` policy: return
`LL_E_UNSUPPORTED` because its slot format is not established by the legacy
importer. This is synthetic metadata coverage; it does not establish that a
current compiler or linker producer emits these exact import sections.

The native test driver runs this unit only on Darwin. Its link-map check
requires the expected stage2 archive and rejects an archived `MachO.c` object
beside the included implementation.

Mach-O segment alignment headroom
=================================

On Darwin, `macho_segment_padding.c` includes the actual `MachO.c` and
`Linker.c` implementations and interposes only their anonymous segment mapper
and segment unmapper. The mapper uses real `mmap` storage but chooses a base
that is page aligned and exactly one page off a two-page boundary, so the
regression does not depend on ASLR. Tests cover page-aligned sections,
over-page-aligned RX/RW/GB sections, mixed regular and zero-fill sections, and
padding-add overflow in each segment class. They check every computed section
span against its segment before any section payload write, and verify mapping
teardown through the matching unmap wrapper.

The loader originally sized each class from offset zero, although the mapper
promised only page alignment. Rounding the returned base up to a larger section
alignment could therefore place a section beyond its segment allocation. The
runtime now reserves the worst-case alignment headroom per class and rejects
padding-add overflow with `LL_E_FORMAT`. This unit exercises segment
construction only; it does not copy or zero section payloads, resolve symbols,
apply relocations, or run initializers. It is native Darwin evidence only.

Mach-O pre-file alignment errors
================================

On Darwin, `macho_prefile_error.c` includes the current `MachO.c` and exercises
`machoGetMisalignment` against real `FILE *` streams. Positive cases cover all
four 4-byte-aligned command-size residues modulo 16 and verify the prior
nonzero stream position is restored. Negative cases cover bad magic, empty
and short headers, a real read error, and rewind failure on a nonseekable pipe.
They check the `LL_E_FORMAT`
or `LL_E_IO` status on the owning linker, preserve a default-linker sentinel,
and reject unexpected fatal callbacks. The driver requires the selected
stage2 archive in both linker map and trace, requires its `MachOVerify.o`
dependency, and rejects an archived `MachO.o` beside the included source.

The native Darwin configuration enables mmap, so the non-mmap caller branches
in `Linker.c` and `LoadArchive.c` are compiled out here. This unit verifies the
error-conversion helper only; those caller cleanup paths need a supported
non-mmap configuration to execute.

Mach-O AArch64 relocation status
================================

On native Darwin AArch64, `macho_error_status.c` includes the current `MachO.c`
and tests its private addend decoder/encoder, symbol lookup, and section
relocation loop. It checks valid little-endian writes and branch encoding, then
checks supported SUBTRACTOR/UNSIGNED and ADDEND/PAGEOFF12 pairs, malformed pair
shapes, conflicting implicit and explicit addends, unresolved symbols, and
positive/negative range errors for UNSIGNED, SUBTRACTOR, BRANCH26, PAGE21,
GOT_LOAD_PAGE21, PAGEOFF12, and GOT_LOAD_PAGEOFF12. Failure cases require the
expected owner-local status, a diagnostic, and an unchanged write target and
canaries. The unit also checks that owner teardown releases its allocations and
that errors do not alter the default linker state.

The native driver runs this unit only when the generated platform header selects
AArch64 on Darwin. Its link-map check requires the selected stage2 archive and
rejects an archived `MachO.o` beside the included implementation. This covers
function-level relocation behavior, not a complete object-file load or a
non-Darwin architecture.

Mach-O x86_64 relocation status
===============================

On Darwin/x86_64, `macho_x86_error_status.c` includes the current `MachO.c` and
calls its actual section relocation loop. Positive controls check literal
UNSIGNED, SIGNED, SIGNED_1/2/4, BRANCH, and SUBTRACTOR writes, GOT and GOT_LOAD
through real `SymbolExtra` storage, and valid internal UNSIGNED/SIGNED section
targets. A real far target checks nearby branch-island encoding. A second
branch case uses a separately allocated real `SymbolExtra`; it checks the
actual target and island distances before exercising `LL_E_RELOC_RANGE` and
prints a skip if those placements do not cross the signed 32-bit boundary.
Negative controls check unsupported relocation types, missing external and
GOT symbols, unresolved section symbols, invalid local GOT symbol types,
invalid PC-relative flags, internal displacements before the target section, a
zero internal section index, computed targets outside the proddable blocks,
and writes outside the object span. Failures must return zero with the expected
owner-local status and diagnostic, leave the relocation target and canaries
intact, and preserve the default linker's error sentinel. Owner teardown must
release all tracked allocations and no recoverable case may use the fatal
callback.

The native driver runs this unit only when the generated platform header selects
x86_64 on Darwin. Its link-map check requires the selected stage2 archive and
`MachOVerify.o`, and rejects an archived `MachO.o` beside the included source.
This covers function-level relocation status behavior under x86_64; it does not
load a complete object. The branch-range case writes to its allocated island
before it can reject an out-of-range island; the test checks target bytes and
canaries, not transaction rollback of the auxiliary island storage.

Mach-O load failure cleanup
===========================

`macho_load_errors.c` includes the actual `MachO.c`, `SymbolExtras.c`,
`Linker.c`, `M32Alloc.c`, and `MMap.c` implementations; its helper cases are in
`macho_protection_cases.h`. It injects tagged host
allocation failures in Mach-O initialization and load metadata, and mapper
failures for segment and NEED_PLT section storage. Each failed load checks
`LL_E_NOMEM`, a diagnostic, owner isolation, and teardown of host allocations
and native mappings. Empty section/symbol and ordinary section loads provide
success controls. The unit uses generated stage2 headers and links remaining
dependencies from the stage2 `liblink.a`; its link-map check rejects archived
copies of all five included implementations.

On Darwin, `--protection-only` exercises a real successful `mprotect`,
injected `EACCES` with owner-local `LL_E_IO` and errno preservation, failed
Mach-O segment and mapped-section protection, and the M32 RX/RW flush and
release paths. It checks that `ocTryLoad` leaves the object unresolved on late
protection failure, that RX pages stay on exactly one owned list after partial
failure, and that a page whose RW transition fails is unmapped before reuse.
The x86_64 case also covers nonzero symbol-extra protection; the AArch64 target
has zero-sized `SymbolExtra` and skips that case. The normal driver runs these
cases after the existing load-cleanup cases.

`--protection-gap-probes` checks quarantine behavior after late protection
failure. The first operation retains its `LL_E_IO` diagnostic; later cached
lookups (including optimistic lookup) and global resolve report `LL_E_STATE`
without another protection attempt. A private dependency graph checks
transitive dependents, cycles, a previously completed dependent, an independent
completed sibling, unregistered active frames, a dependency edge added after
failure, NULL-valued hash-set membership, and exact restoration of the primary
diagnostic across nested frame unwinds. Independent healthy lookup still runs
two initializer hooks in traversal order while skipping quarantined objects.
A pre-registration Mach-O weak-definition lookup retains and quarantines both
the dependent and its lazy provider, including the escaped dependency key.
Normal unload removes the old quarantine and a fresh object loaded at the same
path resolves successfully; an ordinary missing-symbol lookup also succeeds
after its provider is loaded. The M32 retry case rejects a second flush and a
new allocation without another protection syscall, and an RW cleanup failure
preserves the earlier `LL_E_DUPSYM` diagnostic. These cases inspect addresses
and state but never call a mapped symbol. The fixture writes Mach-O objects to
temporary files for the real `loadObj_` path and copies file mappings into the
harness's tracked mappings. The focused driver verifies its link map excludes
every implementation included directly by the test.

`--resolution-failure-only` drives the actual `ocTryLoadImpl` backend-failure
branch through a test-local Mach-O resolver wrapper. With injection disabled,
the wrapper delegates to the real resolver. Its controlled failures cover all
explicit statuses other than `LL_OK` and `LL_E_UNRESOLVED`; they verify the
first status and complete diagnostic buffer, then check that repeat resolution,
cached/optimistic lookup, global resolution, and initialization cannot reuse
the object or re-enter the backend. The mode also applies terminal errors to
the existing dependency-closure, pre-registration weak-lookup, and
unload/reload fixtures. A nested early duplicate is also propagated through a
parent resolver without marking either object; both resolve after removing the
conflicting provider. A false resolver return with no status remains
`LL_E_UNRESOLVED` and succeeds after an ordinary retry. An early duplicate
symbol rejection remains outside the terminal backend-failure boundary,
including when a parent resolver propagates it. A subsequent top-level explicit
backend error still quarantines its object after the stale provenance clears.
The same-operation case then has one resolver call an object that produces
early `LL_E_DUPSYM`, followed by `ll_error_set` with a new `LL_E_FORMAT`
diagnostic; it verifies that only the parent's new error is terminal and that
the complete diagnostic survives scope cleanup. These terminal-resolution
cases run in the driver's default Mach-O unit path as well as in the focused
mode.
`LL_E_NOMEM` is treated as terminal for the affected object: recovery requires
unloading and loading a fresh object. The controlled wrapper tests linker
policy, not each backend's production generation of those statuses.

The weak-definition fixture models an archive member by setting the already
loaded provider to `OBJECT_LOADED`, so symbol lookup resolves it on demand. It
does not execute Windows PE import code; a native Windows quarantine fixture
remains unqualified.

Mach-O branch stub lifecycle
============================

On Darwin/AArch64, `macho_stub_errors.c` includes the current `MachO.c`,
`macho/plt.c`, and `macho/plt_aarch64.c`. It checks successful stub encoding
against fixed instruction words, lookup and repeated-target reuse, two-node
list teardown, allocation failure, backend rejection cleanup, and
allocation-error propagation through the branch relocation caller. The failure
cases verify that the owner status and log are preserved, no fatal callback is
used, and rejected operations leave caller output and stub ownership unchanged.
The backend rejection hook affects only the cleanup case; successful encoding
uses the real backend. The unit links dependencies from the selected stage2
archive and rejects archived `MachO.o`, `plt.o`, and `plt_aarch64.o` beside the
included sources.

The relocation-failure fixture initializes a proddable-block set, adds the
instruction span through `addProddableBlock`, verifies it with `containsSpan`,
and releases its storage with `freeProddableBlocks`. This prepares a valid write
target before allocation failure is injected at `makeStub`. The unit provides
native Darwin/AArch64 function-level coverage; it does not load a full Mach-O
object or qualify other architectures.

M32 mapping failure status
==========================

On Darwin, `m32_map_status.c` includes the actual `M32Alloc.c` and interposes
its anonymous mapping and unmapping calls with real OS mappings plus an
owner-local mapping ledger. It checks small-batch and large-allocation mapping
failure and out-of-range results, including `LL_E_NOMEM`, release of rejected
mappings, and no writes into a rejected small-page pool. Positive cases cover
small-pool reuse and large allocations. The fixture drains any retained small
page pool using the configured system page size before checking that every
mapped page was released. It also checks allocation ownership, logging and
memory-failure callbacks, fatal-callback exclusion, and default-linker error
isolation.

The native driver runs this unit on Darwin and requires the selected stage2
archive while rejecting an archived `M32Alloc.o` beside the included source.
The check is native to the configured Darwin architecture; it does not qualify
other hosts or the full object-loading path.

Windows M32 reservation ownership
=================================

`m32_windows_reservations.c` is gated to a generated native Windows
configuration with `NEED_M32`. It includes the actual M32 reservation,
mapping-protection, and allocator implementations. Real `VirtualAlloc`,
`VirtualQuery`, and successful `VirtualFree` calls provide OS ownership
evidence; wrappers inject only bounded `MEM_DECOMMIT`, `MEM_RELEASE`, and
read/write protection failures.

The cases check base and interior page decommit while preserving sibling
commitments, reject duplicate and out-of-range discards before calling
`VirtualFree`, cover a pooled prefix with a decommitted large-allocation tail,
and retain records after failed decommit or base release for one successful
retry. They also check shutdown behavior with live allocator pages. The fixture
snapshots an executable page header before injected RW-protection and
decommit failures and checks that the rejected page was not linked into the
free pool or modified. The failed-protection path is composed with the
quarantine handling here; native Windows execution remains required.
Additional injected query and mapper failures exercise both small and large
allocation callers: query failures retain `LL_E_IO`, mapping failures replace
stale status with `LL_E_NOMEM`, and captured Win32 error/`errno` values survive
the host allocator's bookkeeping free. The large query case also checks that a
failed cleanup release preserves the primary diagnostic and retries once at
shutdown.
Use `python3 liblink/tests/unit/run_native_tests.py --windows-m32-only` on a
native Windows build to run this unit and `pei386_common_ownership.c`. The PE
fixture writes a small AMD64 COFF object with a writable data section, an
`ADDR64` relocation, and a COMMON symbol, then loads, resolves, unloads, and
disposes it through the real PE linker and `freeObjectCode` paths. It checks
zero-filled COMMON storage, the relocation target, smaller-COMMON reuse, and
that disposing reuse and empty objects leaves the provider's bytes intact.
Its host allocation ledger detects a COMMON M32 address sent to `host_free`
without calling `free` on that address. The driver also compiles a temporary
one-line `SECTION_MALLOC` mutation and requires the ledger diagnostic, so the
stale classification is a discriminating failure while remaining safe to run.
After normal disposal the fixture reports and checks the known host-ledger
metadata residue: three copied COFF string tables, two section names, and two
relocation copies. It releases those allocations only after recording the
observation as fixture-owned harness cleanup. This is not a claim about
CRT-allocated duplicated symbol names or whole-process leak freedom.

The Windows-only archive checks normalize slash direction and case and add
`cygpath -m` spelling when available, while requiring the selected archive in
both link map and trace and rejecting duplicate included implementation members.
Native Windows CI passed the reservation and COMMON ownership units in both
qualification runs recorded below. The known normal-disposal metadata residue
remains explicitly counted and released by the harness; these runs do not
establish general leak freedom.

The Windows CI job runs the focused GHC testsuite after stage2 with
`bash .github/scripts/run-windows-focused-tests.sh`. It makes nine selections:
the two T1 roots, the three T2 roots in normal and external-interpreter ways,
and the five registered T3 unload test names with all registered available
ways. Logs, JUnit, summaries, and metrics are uploaded under the
`windows-focused-testsuite-results` artifact from `_build/logs/windows-focused/`.
Both focused and top-level CI testsuite invocations use `VERBOSE=3` so run logs
include named test/way starts and fragile outcome records. JUnit omits expected
failures and fragile passes, so the raw log, JUnit, and summary together allow
complete capture validation for baseline comparison.
The reviewed Windows x86_64 qualification runs are:

- [Path ownership, run 40077036858](https://github.com/stable-haskell/ghc/actions/runs/40077036858),
  commit `fe5f0785fbae6c6a7955d6b86bfd5a85fdf6ae96`: stage1, stage2,
  path ownership, M32 reservations, COMMON ownership, and focused T1–T3 passed.
- [PE resolution and unwind, run 40077134443](https://github.com/stable-haskell/ghc/actions/runs/40077134443),
  commit `656b4fa6a038a0fa6b2034bb54794ab20ea9ad23`: the same checks plus
  `pei386_resolution_status` and `pei386_unwind_retention` passed.

Complete named captures contain 1,273 selection executions per run, with
identical test/way outcomes between these two commits. Selections overlap, so
this count is not a count of unique tests. No frozen Windows baseline is
available, and neither run used Windows sanitizers. These results therefore
establish focused native qualification, without a baseline-parity or general
leak-freedom claim. Later source changes need their own affected checks.

PE post-symbol resolution
=========================

`pei386_resolution_status.c` uses the genuine Windows x86_64 host fixture
and actual included `PEi386.c`. It checks all three symbol-extra encodings
and exact addresses with real M32 allocators, mapping-failure status and
output preservation, owner/default isolation, and DLL fallback termination.
Its valid COFF relocation fixture checks post-trampoline range failures for
ADDR32, ADDR32NB, 32S, and REL32, including an unchanged relocation slot and
one selected trampoline allocation per case. A within-range REL32 control
checks the normal write path without a trampoline. These checks inspect data
and never call generated jump bytes.

The focused `--pe-resolution-only` mode runs this unit, the unwind lifetime
unit below, the terminal classification/quarantine units, and the COMMON
size-conflict controls. Native Windows
compilation and execution of the DLL
indirection allocation failure, error preservation, and post-trampoline range
cases passed in run 40077134443, as recorded above.

PE terminal classification controls
==================================

`pei386_resolver_classification.c` retains the direct backend checks for
reserved static section indices and the existing upper boundary, SECREL
success at `UINT32_MAX` and failure at `UINT32_MAX + 1`, unsupported
relocations, failed exception registration, unchanged failed slots, and valid
REL32.
`pei386_resolver_quarantine.c` drives the actual core `ocTryLoad` wrapper
with unsupported-relocation and exception-registration failures. It requires
the first specific status and diagnostic, then `LL_E_STATE` on a later call,
exactly one backend dispatch, owner isolation, and normal `freeObjectCode`
disposal returning the failure cases' host allocation ledger to its baseline.
A resolved REL32 object remains usable without another backend dispatch. The
healthy control explicitly releases its fixture-owned relocation array after
those assertions: the existing successful PE disposal path leaves copied
relocation arrays behind. Its ledger still must return to baseline after
fixture cleanup and normal disposal; this does not claim general successful
PE metadata leak freedom.

The retry control counts an erroneous backend dispatch without entering
already released metadata. An uncorrected baseline that remains retryable
fails its quarantine assertion without attempting that retry. These
native Windows x86_64 controls compiled and passed at `9eeb993c` in CI run
`40178749130`. Their failure behavior on the uncorrected baseline remains
unverified. They do not qualify the ARM64 unsupported-relocation branch or
the held pre-symbol table-validation work.

PE COMMON size conflicts
=======================

`pei386_common_conflict.c` uses ordinary valid AMD64 COFF objects and the
real Windows loader, resolver, dependency tracking, and object disposal.
For short, exactly eight-character inline, and string-table symbol names,
equal and smaller declarations reuse a resolved 64-byte COMMON provider.
Six larger declarations in each allocation mode must return `LL_E_DUPSYM`
with the existing precise diagnostic, without calling the fatal callback. The provider's address, size,
bytes, registration, and usable state must remain unchanged. Half of those
failures follow publication of an independent prefix COMMON symbol; cleanup
must remove that symbol while retaining the provider. Each rejected load
must restore its host allocation ledger and leave no object/list entry.
Replacing each rejected file with a compatible declaration must then succeed
at the same path, resolve its ADDR64 relocation to the original storage, and
retain the provider dependency. The default linker's error remains isolated.

The repair preserves the existing size-acceptance rule. Caller-owned name
buffers cover the two temporary COMMON-name lookups, including eight-byte
inline names; persistent symbol names are still copied. Other name callers
retain their existing ownership behavior. Successful control disposal
separately verifies and releases only the fixture's three known PE metadata
residues. Failed-load assertions receive no harness cleanup. The ledger does
not track CRT-allocated duplicated symbol names, so this is not a general
leak-freedom claim. Run `python3 liblink/tests/unit/run_native_tests.py
--pe-resolution-only --cc clang` on configured native Windows x86_64.
Windows x86_64 runs `40191109539` at `939cfd1` and `40195430466` at
`07ab2de` compiled and executed this unit. Both passed the four preceding
PE units. The COMMON unit completed with 128 initial-zero byte assertion
failures in each run; no conflict, reuse, cleanup, retry, ownership, or
default-linker-isolation assertion failed. Both sets of nine focused GHC
captures passed, with the same 1,274 named selection outcomes as the previous
successful `9eeb993` Windows run.

Source review confirms that M32 can return uncleared storage and the original
GHC PE and M32 paths have the same omission. The earlier failure logs did
not record the exact origin of each nonzero byte.

The separate zero-initialization repair clears only newly allocated global
COMMON storage after a successful M32 allocation and before publication.
M32 recycles pages without clearing their payload. Existing provider symbols
are excluded from this allocation, so their sentinel bytes must survive.
The unit retains every original assertion and runs both the native allocator
and a deterministic nonzero-storage control. The latter calls the real M32
allocator, patterns only the fixture's new COMMON requests, and checks the
intercepted count, size, and provider address. Entire new providers and prefix
COMMON ranges must be zero, while compatible aliases must allocate no
replacement COMMON storage. The six providers cover all three name forms in
each mode; twelve conflicts and twelve same-path retries retain the rollback
and provider-preservation controls. Native diagnostics record pre-clear bytes
and whether a provider address was reused; the original failure log did not
record those details.

At `59e9b809`, Windows x86_64
[CI run 40238497625](https://github.com/stable-haskell/ghc/actions/runs/40238497625)
passed both compiler builds and all six native C-test steps, including all
five PE resolution units. This unit passed both allocation modes, six
providers, twelve conflicts, and twelve same-path retries. In native mode,
the second and third providers reused the previous provider address with
64 nonzero bytes before clearing; the first byte was `0xa6`. All subsequent
provider/prefix zero-fill and existing-provider preservation assertions
passed. This records reuse in the corrected run; the older failure logs did
not record byte provenance. All nine focused GHC captures also completed,
with no unexpected results and the same 1,274 named selection outcomes as
previous Windows CI runs at `07ab2de` and `9eeb993`. Together with the native
C controls, this qualifies the repaired COMMON path on Windows x86_64.
An uncorrected size-conflict baseline run has not been performed; these
comparisons use previous Windows CI results.

PE load-resource failure cleanup
=================================

`pei386_load_resources.c` uses valid AMD64 COFF objects and the real Windows
x86_64 linker fixture. It injects only private M32 allocation failures: the
first section allocation, and global COMMON BSS after a section allocation
succeeds. It checks `LL_E_NOMEM` and preservation of a specific owner error,
no fatal callback, failed-object/symbol/list/host-ledger cleanup, default
linker isolation, and a successful retry of the same failed fixture file and
symbol while a distinct resolved COMMON provider remains live. A normal
load/resolve/dispose path remains the control. Each failed `loadObj_` must
return the host allocation ledger to its
pre-load baseline. After the two successful control objects are disposed, the
fixture separately counts the existing normal-disposal residue of two copied
COFF string tables, two section names, and two relocation copies, then releases
those known allocations as harness cleanup. This is a narrow ownership check,
not general native leak qualification. Run `python3
liblink/tests/unit/run_native_tests.py
--pe-load-resources-only --cc clang` on native Windows x86_64. Compilation and
native execution passed at `17f5961a` in
[CI run 40175526890](https://github.com/stable-haskell/ghc/actions/runs/40175526890).
This fixture does not inject NULL host allocator callbacks or establish general
Windows sanitizer/leak qualification.

PE unwind record lifetime
=========================

`pei386_unwind_retention.c` uses the genuine Windows x86_64 host fixture
and actual included PE cleanup functions. Resolution cleanup discards parse
metadata while retaining the `pdata` ownership record until physical disposal
unregisters the same table pointer. Positive, negative, and regression cases
check repeated cleanup, the callback's live ownership record, exact unregister
count, and eager cleanup for objects without a table. The owner ledger must be
empty after disposal.

Run `python3 liblink/tests/unit/run_native_tests.py --pe-resolution-only --cc clang`
on configured native Windows x86_64. Native compilation and execution passed
in run 40077134443. This repair preserves the existing single-table bookkeeping
and unregister-failure policy; it adds no registration-success tracking.

Approved ELF resource error coverage
====================================

`elf_resource_errors.c` covers the approved ELF COMMON/GOT resource behavior.
It reuses the section unit's owner and mapping ledgers, includes current
`Linker.c`, and links the actual ELF backend from the native archive. Data-only
fixtures cover COMMON/GOT successes and exact allocation failures, same or
smaller COMMON aliases, larger duplicate rollback, zero-sized COMMON
anchoring, COMMON sum/page overflow, primary-error preservation under a
format-metadata cleanup callback, and descriptor closure on quarantined COMMON
alias rejection. Existing quarantine retention is asserted, followed by
explicit logical unload and modeled disposal after all aliases unload. It does
not exercise actual RTS GC marking or scheduling.

New COMMON/GOT mappings remain owned until actual object disposal, and aliases
retain their provider's lifetime. Allocation failure returns `LL_E_NOMEM`, a
larger COMMON conflict returns `LL_E_DUPSYM`, and representational overflow
returns `LL_E_UNSUPPORTED`.

The fixture generator writes ordinary ELF64 x86_64 data and symbol containers
without code, relocations, or initialization arrays. The unit uses contiguous
images, so M32 page caching is outside its COMMON/GOT mapping ledger; normal
M32 pooling is unchanged. Native Linux x86_64 and genuine generated config are
required. The driver supports `--elf-resources-only` and the default native
run, literal `-llink`, GNU wrappers, and archive member selection.

The GOT byte-count overflow guard has no runtime coverage in the 64-bit unit.
Backend execution and sanitizer behavior require running this unit on native
Linux x86_64 with the approved implementation.

Approved x86_64 Mach-O operand-width coverage
============================================

`macho_x86_operand_widths.c` reuses the current x86 status-unit host helpers
and includes the actual Mach-O backend. Its 56 cases cover byte offsets 0–7,
all four operand widths, literal high-bit, carry, and narrowing results,
16/32-bit zero-extension controls using internal target spans, and first or
only invalid operand spans with unchanged source and target sentinels. The
compiler-fixture unit reuses `macho_input.c`, supplies the fixture's external
data definition, resolves its entry, and verifies relocated data pointers
without calling loaded functions or initializers. It uses Darwin's usual PIC
mapping mode, matching the RTS default and existing native fixtures. Both
units require generated x86_64 Darwin configuration. The driver links literal `-llink`, checks archive
selection, and supports `--macho-x86-operands-only` plus the default native
run. The repair uses `memcpy` for 16, 32, and 64-bit operands, allowing
unaligned locations while preserving zero extension, narrowing, arithmetic,
and byte order.

Run `python3 liblink/tests/unit/run_native_tests.py
--macho-x86-operands-only` on a Darwin x86_64 build with the approved backend
and generated configuration. A Rosetta run provides evidence for that
execution environment; native Intel macOS CI provides separate evidence.
Neither is implied by adding these tests.

POSIX mmap address bounds
=========================

`mmap_address_bounds.c` includes the actual `MMap.c` and checks `nearImage`
using integer address comparisons only. A zero lower bound produces the upper
bound `0x80000000`. These are virtual addresses, with no mapping to dereference.
Computing the bound with `uintptr_t` avoids pointer arithmetic from `NULL`.
The unit also covers ordinary and maximum representable bounds, cached-region
behavior, and isolation from the default linker owner. It performs no mmap syscall and does
not parse or load an object. Run
`python3 liblink/tests/unit/run_native_tests.py --mmap-address-bounds-only` on
a POSIX build with generated `RTS_LINKER_USE_MMAP=1` configuration. The default
driver run also includes it on eligible Linux and Darwin builds. A Rosetta run
qualifies that execution environment; native Intel macOS CI is separate
evidence.

Unpacked sums timing diagnosis
==============================

For a diagnostic sample, run `.github/scripts/measure-unpack-sums.py` with an
explicit compiler and a new output directory, for example:

    python3 .github/scripts/measure-unpack-sums.py \
      --ghc _build/dist/bin/ghc \
      --output-dir _build/unpack-sums-timing-sample

The POSIX collector compiles `unpack_sums_6.hs` with `-O`, `-rtsopts`,
`-no-user-package-db`, and `-fforce-recomp`. It keeps compiler outputs in the
result directory, runs one warmup and five recorded samples, and keeps raw
stdout/stderr plus exact arguments, compiler/source hashes, and `--info` output
in the result directory. It enables `GHC_UNPACK_SUMS_TIMINGS=1` only for direct
program runs. Exit 1 from the unchanged timing predicate is recorded as data;
the collector succeeds only when every run has expected stdout and two parsed
durations. It does not compare performance or establish benchmark acceptance.
Flushed phase-start/phase-end markers identify the last active command; if
cleanup leaves pipes open, the result records incomplete cleanup and retains
the partial command output. Cleanup allows two seconds to drain pipes and one
second to reap the direct child after a command times out.

Darwin CI runs this collector after the ordinary testsuite with each stage2
compiler and uploads a separate timing artifact, including incomplete results.
The ordinary test keeps its original expected stdout and timing predicate.

Focused native qualification
=============================

The manual workflow accepts `qualification=full` (the default),
`qualification=linux-native`, or `qualification=windows-native`. For example,
run only the static Linux build and native linker qualification with:

    gh workflow run ci.yml --repo stable-haskell/ghc --ref <branch> -f qualification=linux-native

To run the existing Windows build and native linker qualification without
queuing the Unix matrix, use:

    gh workflow run ci.yml --repo stable-haskell/ghc --ref <branch> -f qualification=windows-native

This focused native qualification does not replace full T1–T4 or platform
acceptance. The default manual, push, and pull-request workflow retains the
full existing matrix and jobs.

Private path duplication ownership
==================================

`pathdup_ownership.c` links the installed native archive and checks that empty,
ASCII, and non-ASCII path copies are allocated and released by their linker
owner, with an independent default-host sentinel. It runs with the full native
unit suite. To isolate it on a native Windows build without changing the
foundation-only mode, run `python3 liblink/tests/unit/run_native_tests.py
--path-ownership-only`. A Darwin or Linux control run checks the existing
POSIX implementation; it does not qualify the Windows path.

On Windows, the copy uses the selected linker owner's allocator and includes
the wide path's terminator. The legacy `wcsdup` allocation bypassed that
allocator while disposal used the owner's `free_` callback. This regression
requires acquisition and release to use the same owner.

Initializer error status and quarantine
======================================

`init_array_status.c` includes the shared `InitFini.c`. Its six cases cover a
null first, middle, or last `.init_array` entry; successful owned-environment
and absent-argument-hook controls; and initializer priority order. Failure
returns `LL_E_INIT`, releases the program environment exactly once, and leaves
the pending list owned for normal disposal. Callback counters verify the
executed prefix, and owner ledgers check list/environment cleanup and default
owner isolation. Successful initializer and finalizer behavior is unchanged.

`init_array_policy.c` includes `Linker.c`, `InitFini.c`, and `ll_failure.c`.
A controlled native initializer backend calls the actual shared helper. The
unit checks that the lookup triggering initialization returns NULL on failure,
preserves the first error and hook result, and quarantines the affected object
and its transitive dependents. It checks rejection of repeated initialization,
cached lookup and global resolve, and continued use of an independent object.
It exercises logical unload through `unloadObj_` and initialization of a fresh
same-name modeled object. Stack objects model already-resolved objects; this
part does not parse or reload a file. On ELF and PE, a separate heap-owned object
control calls normal `freeObjectCode` after a partial initializer failure and
requires all retained init/fini metadata to be freed without calling finalizers.
Successful initialization and never-started controls verify the finalizer gate
and cleanup. The archive supplies the real native format teardown. These synthetic
objects do not exercise GC reachability or file parsing; ordinary file loading
and unload behavior also require native GHC T1–T3 qualification.
Earlier constructor side effects cannot be rolled back. The failure policy
prevents replay until unload/reload and retains `initializersRan == false`.
Existing finalizer handling remains unchanged.

Run `python3 liblink/tests/unit/run_native_tests.py --initializer-errors-only`
on a configured native Linux, Darwin, or Windows build. Default native Linux
and Darwin unit runs also include these cases. The driver requires the selected
native archive and rejects included implementation members pulled from it.
Windows uses the real installed rts-fs archive before its system libraries.
Native Linux and Windows execution are required for their disposal controls. Windows
execution and integrated GHC T1–T3 qualification remain required.

The fixture installs and checks a real OS page-size service. Windows M32
shutdown queries that service even when its free-page pool is empty.
The Windows fixture explicitly frees its process-DLL bookkeeping after
`ll_exit_linker`, matching the existing PE ownership fixture. Its final
allocation assertion remains zero. This auxiliary cleanup does not unregister
DLLs or change library teardown.

Set `LIBLINK_INIT_POLICY_TRACE=1` for flushed stderr phase markers around
initialization, physical disposal, lookup, unload, and fixture teardown.
Windows CI enables these markers and compiles the initializer fixtures with
debug information. If an initializer binary fails and GDB is available, the
driver reruns that binary under GDB for a stack trace before temporary files
are removed. The first failure remains the result even if the diagnostic run
succeeds or the debugger fails. Independent Windows checks continue after a
unit failure, and every failed step still fails the job.

POSIX native-load failure cleanup
================================

`native_load_errors.c` links the selected native archive and uses real
`dlopen`, `mkOc`, and `freeObjectCode`. Owner allocation ledgers cover two
successful owners, duplicate reuse, static-object collisions, and two repeated
missing-library failures on one owner. Error-copy contents must survive a host
cleanup callback that changes loader-error storage. A separate callback model
attaches an opaque fixture-owned cookie and verifies its one-time release
before object metadata. This does not model RTS foreign-export registrations
backed by a library that an OS loader has already unmapped.

The missing-library case also runs twice with the optional `on_unload` hook
unset. The library must still free its own metadata with no cleanup callback
or extra hook event; allocation cleanup does not depend on a host hook.

The final no-handle failure callback sees an unregistered dynamic object whose
internal status remains `OBJECT_READY`. Both native-load hooks have completed,
and the diagnostic has been copied, before `on_unload` releases attached host
state and the library frees metadata. The caller still owns the copied error.
This unit adds no public `ll_status` classification; public native API wiring
is a separate work package.

A second fixture contains an unused reference to a missing function. No fixture
function is called. With native `HAVE_DLINFO`, eager loading must fail and lazy
loading succeed without failure cleanup, preserving both hook pairs. Other
POSIX builds exercise their existing single lazy attempt. Generated platform
macros are used unchanged; Darwin execution does not qualify Linux retry paths.

The Linux lazy fixture explicitly uses `-fplt` and `-Wl,-z,lazy` so it has
the binding behavior required by this control. These options apply only to
that fixture. An additional library retains the original compiler defaults;
the driver records ELF dynamic tags and relocations when `readelf` is present.
Two direct-loader controls report the ambient library's NOW/LAZY results and
require the controlled library to reject NOW and accept LAZY before liblink
is exercised. The original liblink lazy-retry assertions remain unchanged.
The original Linux CI failure is not attributed to compiler hardening without
this native evidence. [GCC documents why lazy binding requires PLT calls](https://gcc.gnu.org/onlinedocs/gcc-14.3.0/gcc/Code-Gen-Options.html),
and [GNU ld documents lazy binding](https://sourceware.org/binutils/docs/ld/Options.html).

Run `python3 liblink/tests/unit/run_native_tests.py --native-load-errors-only`
on a configured native Darwin or Linux build. The default POSIX unit suite
also runs these cases. Native Linux execution remains required for the
`HAVE_DLINFO` controls; synthetic host state is not a rollback guarantee.

Successful native loads without mapping metadata
------------------------------------------------

On Linux with native `HAVE_DLINFO`, GNU linker wrappers inject a `dlinfo`
failure and, separately, successful iteration that supplies no ranges. Each
case runs with linker diagnostics enabled and disabled. The selected archive
must return a successful handle with no error allocation, retain `OBJECT_READY`
and null ranges, and mark the object non-unloadable. Tests require one native
load hook pair, registration and batch completion, no failure callback, no
close, accessible exported data, and duplicate reuse without another attempt.
Enabled diagnostics must identify the injected failure. Uninjected eager-load
and lazy-retry controls keep their existing range and unloadability assertions.

The unit deliberately frees the object after checking retention and requires
exactly one close at that explicit teardown. This checks fixture disposal, not
GC retention or RTS profiling/foreign-export lifetimes. Native Linux execution
is required for these four controls; Darwin uses its genuine configuration
and runs the six existing POSIX controls without Linux wrappers.

Runtime format dispatch
-----------------------

`run_native_tests.py --format-dispatch-only` checks the private format selector
and core lifecycle dispatch with the current native archive. Signature-only
controls cover ELF, all four Mach-O byte orders/widths, ordinary/anonymous COFF,
and Wasm, including short, unaligned, unavailable, and unknown inputs. They do
not enter backend parsers. Two distinct operation tables check standalone and
modeled lazy-member dispatch, extras-before-names ordering, image release during
name acquisition, initializer hook ordering, finalizer gating, partial-init
cleanup, and the NULL-image native-library path. Actual archive parsing remains
covered by the existing archive and T1 tests.

The native table must be the backend-owned table, with every frozen callback
bound. Source-included backend-interposition fixtures copy that table and replace
only their intended test callbacks. Synthetic objects that bypass input
initialization explicitly install their operations; production objects acquire
them through `ll_init_object`.

On POSIX hosts, `format_archive.c` also loads an ordinary compiler-produced
object standalone and as an archive member. It requires the same backend table
and lazy member state. Index and non-object members remain skipped. A later
short object member reports its format error while retaining the first member,
its table, and registration. The primary diagnostic is checked before the next
operation; a fresh unused-member query clears it and retains lazy state.
Explicit disposal and linker exit balance the owner allocation
ledger. These checks run with the default native suite and do not execute the
member's code or constructors.

Mach-O ARM64 PAGE21 encoding
---------------------------

The default native unit suite extracts the current shared PAGE21 and
GOT_LOAD_PAGE21 field writer into a portable integer test. It checks fixed
positive, negative, and endpoint encodings; every signed 21-bit page delta;
preservation of surrounding opcode/register bits; and ignored low-byte bits.
Two deliberately wrong outputs check that the independent decoder rejects
changed immediate or register bits. The unit executes no generated instructions.

The writer converts the signed byte displacement to uint64_t before extracting
instruction fields. This avoids the undefined signed left shift exposed by a
backward reference in the ordinary PAGE21 golden fixture. It also avoids
implementation-defined signed right shift in this field extraction. Existing
range guards, error classification, addend calculation, and numeric goldens
remain unchanged. Backend range rejection and native relocation behavior still
require the existing native units and golden captures; the portable unit alone
does not qualify them.
