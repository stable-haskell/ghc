/* Native arm64 Darwin fixture for compat_exec_smoke.c. */
.text
.p2align 2
.globl _fixture_entry
_fixture_entry:
    stp x29, x30, [sp, #-16]!
    bl _external_callback
    ldp x29, x30, [sp], #16
    ret
