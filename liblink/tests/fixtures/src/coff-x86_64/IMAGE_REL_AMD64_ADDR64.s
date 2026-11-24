# The relocation target is external so assembly cannot resolve it.
.data
.p2align 3
.globl ll_fixture_addr64
ll_fixture_addr64:
    .quad ll_fixture_target
