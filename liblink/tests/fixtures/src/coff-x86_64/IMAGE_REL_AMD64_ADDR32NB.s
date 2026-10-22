# The relocation target is external so assembly cannot resolve it.
.data
.p2align 2
.globl ll_fixture_addr32nb
ll_fixture_addr32nb:
    .rva ll_fixture_target
