# The relocation target is external so assembly cannot resolve it.
.data
.p2align 3
.globl _ll_fixture_unsigned
_ll_fixture_unsigned:
    .quad _ll_fixture_target
