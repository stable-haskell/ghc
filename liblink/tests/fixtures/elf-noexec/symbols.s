# Valid ELF64 data-only relocation fixture; these bytes are never executed.
.text
.globl fixture_high_symbol
.space 4096
fixture_high_symbol:
.quad 0
.local fixture_local_symbol
.type fixture_local_symbol,@object
fixture_local_symbol:
.quad 0
.space 4096
.data
.globl fixture_high_reference
fixture_high_reference:
.quad fixture_high_symbol
.quad fixture_local_symbol
.quad fixture_noexec_bss
.quad fixture_noexec_common
.bss
.globl fixture_noexec_bss
fixture_noexec_bss:
.zero 16
.comm fixture_noexec_common,16,8
.section .note.GNU-stack,"",@progbits
