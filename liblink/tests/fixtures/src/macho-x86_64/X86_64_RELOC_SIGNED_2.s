# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl _ll_fixture_x86_64_reloc_signed_2
_ll_fixture_x86_64_reloc_signed_2:
    cmpw $0x1234, _ll_fixture_target(%rip)
    ret
