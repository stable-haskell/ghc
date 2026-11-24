# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl _ll_fixture_arm64_reloc_pageoff12
_ll_fixture_arm64_reloc_pageoff12:
    add x0, x0, _ll_fixture_target@PAGEOFF
    ret
