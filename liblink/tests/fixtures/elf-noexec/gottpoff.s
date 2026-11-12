# Valid TLS relocation input; the NO_EXEC tests never execute these bytes.
.text
.globl fixture_noexec_gottpoff
fixture_noexec_gottpoff:
movq external_tls@GOTTPOFF(%rip), %rax
.section .note.GNU-stack,"",@progbits
