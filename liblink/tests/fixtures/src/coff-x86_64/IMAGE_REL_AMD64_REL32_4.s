# LLVM emits REL32; the catalog transform selects REL32_4.
.text
.p2align 2
.globl ll_fixture_rel32_4
ll_fixture_rel32_4:
    cmpl $0x12345678, ll_fixture_target(%rip)
    ret
