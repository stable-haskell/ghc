# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl _ll_fixture_arm64_reloc_branch26
_ll_fixture_arm64_reloc_branch26:
    b _ll_fixture_target
    ret
