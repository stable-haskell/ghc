# LLVM emits REL32; the catalog transform selects REL32_2.
.text
.p2align 2
.globl ll_fixture_rel32_2
ll_fixture_rel32_2:
    cmpw $0x1234, ll_fixture_target(%rip)
    ret
