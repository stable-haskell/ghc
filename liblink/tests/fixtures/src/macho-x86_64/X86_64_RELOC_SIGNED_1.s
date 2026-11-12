# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl _ll_fixture_x86_64_reloc_signed_1
_ll_fixture_x86_64_reloc_signed_1:
    cmpb $1, _ll_fixture_target(%rip)
    ret
