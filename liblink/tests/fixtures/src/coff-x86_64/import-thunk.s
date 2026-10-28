# The relocation target is external so assembly cannot resolve it.
.text
.p2align 2
.globl ll_fixture_import_thunk
ll_fixture_import_thunk:
    jmp *__imp_ll_fixture_import(%rip)
    ret
