# The relocation target is external so assembly cannot resolve it.
.data
.p2align 2
.globl ll_fixture_secrel
ll_fixture_secrel:
    .secrel32 ll_fixture_target
