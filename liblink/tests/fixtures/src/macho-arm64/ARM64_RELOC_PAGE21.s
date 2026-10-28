# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl _ll_fixture_arm64_reloc_page21
_ll_fixture_arm64_reloc_page21:
    adrp x0, _ll_fixture_target@PAGE
    ret
