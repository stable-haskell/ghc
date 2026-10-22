# LLVM emits REL32; the catalog transform selects REL32_1.
.text
.p2align 2
.globl ll_fixture_rel32_1
ll_fixture_rel32_1:
    cmpb $1, ll_fixture_target(%rip)
    ret
