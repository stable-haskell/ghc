# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl ll_fixture_image_rel_amd64_rel32
ll_fixture_image_rel_amd64_rel32:
    leaq ll_fixture_target(%rip), %rax
    ret
