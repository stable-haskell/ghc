# LLVM emits REL32; the catalog transform selects REL32_5.
# This data record stores a displacement from the end of its trailing bytes.
.data
.p2align 2
.globl ll_fixture_rel32_5
ll_fixture_rel32_5:
    .long ll_fixture_target - . - 9
    .zero 5
