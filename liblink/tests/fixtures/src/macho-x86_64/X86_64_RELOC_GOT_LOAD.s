# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl _ll_fixture_x86_64_reloc_got_load
_ll_fixture_x86_64_reloc_got_load:
    movq _ll_fixture_target@GOTPCREL(%rip), %rax
    ret
