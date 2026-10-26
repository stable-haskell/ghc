# Equal-valued symbols require distinct GOT slots. Never executed.
.text
.p2align 3
.globl fixture_aarch64_got_alias
fixture_aarch64_got_alias:
ldr x0, [x1, :got_lo12:external_function]
.p2align 3
ldr x2, [x3, :got_lo12:external_function_alias]
.section .note.GNU-stack,"",@progbits
