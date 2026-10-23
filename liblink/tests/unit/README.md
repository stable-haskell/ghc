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

After a successful top-level `make stage2`, run the focused units from the
repository root with:

    python3 liblink/tests/unit/run_native_tests.py

The driver locates the generated `liblink` configuration headers, installed
`rts-fs` headers, and stage2 `native-linker/liblink.a`. It compiles the private
error-state, core status/negative-input, and archive status tests with
assertions. The core and archive fixtures include their implementation source
and link dependencies from the real archive using `-llink`; linker maps check
that the corresponding archive object is not also selected. On Darwin, it also
runs the Mach-O preflight, indirect-import, and segment-alignment units. The
driver also runs the AArch64 Mach-O stub lifecycle unit on Darwin/AArch64. It
accepts `--build-root` and `--store-root` when the stage2 build uses
non-default directories, and `--cc` to select a C compiler command.

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
backend on this host; full backend execution remains for RISC-V CI.

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

This checkout has no Windows compiler or runner, so the Windows behavior
remains unqualified until that native command runs. The Windows-only archive
checks normalize slash direction and case and add `cygpath -m` spelling when
available, while requiring the selected archive in both link map and trace and
rejecting duplicate included implementation members.
