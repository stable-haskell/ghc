.text
.globl fixture_x86_64_tlsgd
fixture_x86_64_tlsgd:
  leaq external_tls@TLSGD(%rip), %rdi
  callq __tls_get_addr@PLT
