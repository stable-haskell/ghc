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
