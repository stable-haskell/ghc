# The relocation target is external so assembly cannot resolve it.
.data
.p2align 2
.globl _ll_fixture_pointer_to_got
_ll_fixture_pointer_to_got:
    .long _ll_fixture_target@GOT - .
