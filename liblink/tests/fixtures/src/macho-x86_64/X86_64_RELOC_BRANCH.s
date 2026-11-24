# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl _ll_fixture_x86_64_reloc_branch
_ll_fixture_x86_64_reloc_branch:
    jmp _ll_fixture_target
    ret
