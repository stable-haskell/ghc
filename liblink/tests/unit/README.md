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
