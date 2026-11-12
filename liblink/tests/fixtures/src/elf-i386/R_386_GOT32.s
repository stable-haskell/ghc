.text
.globl fixture_i386_got32
fixture_i386_got32:
  addl external_data@GOT(%ebx), %eax
