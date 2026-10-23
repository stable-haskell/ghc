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
that the corresponding archive object is not also selected. It accepts
`--build-root` and `--store-root` when the stage2 build uses non-default
directories, and `--cc` to select a C compiler command.

The core and archive fixtures use POSIX file descriptors and mmap injection.
On Windows, `Linker.c` follows its stdio path (`RTS_LINKER_USE_MMAP=0`); run
only the platform-neutral foundation test with `--foundation-only` until
Windows fixtures cover those paths.

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

The native test driver runs this unit only on Darwin. Its linker map verifies
that the included `MachO.c` is not also selected from the archive. Run the
driver after a static stage2 build as described above.

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
padding addition overflow with `LL_E_FORMAT`. This unit exercises segment
construction only; it does not copy or zero section payloads, resolve symbols,
apply relocations, or run initializers. It is native Darwin evidence only.
