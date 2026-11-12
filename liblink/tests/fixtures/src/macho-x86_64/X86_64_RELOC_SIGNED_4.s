# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl _ll_fixture_x86_64_reloc_signed_4
_ll_fixture_x86_64_reloc_signed_4:
    cmpl $0x12345678, _ll_fixture_target(%rip)
    ret
