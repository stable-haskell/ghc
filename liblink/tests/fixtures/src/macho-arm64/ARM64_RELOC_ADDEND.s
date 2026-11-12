# Each ADDEND record must immediately precede its PAGEOFF12 or PAGE21 record.
.text
.p2align 2
.globl _ll_fixture_arm64_reloc_addend
_ll_fixture_arm64_reloc_addend:
    adrp x0, _ll_fixture_target@PAGE+8
    add x0, x0, _ll_fixture_target@PAGEOFF+8
    ret
