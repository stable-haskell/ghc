.text
.globl fixture_i386_got32x
fixture_i386_got32x:
  movl external_data@GOT(%ebx), %eax
