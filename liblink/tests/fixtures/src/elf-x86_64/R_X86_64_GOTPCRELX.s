.text
.globl fixture_x86_64_gotpcrelx
fixture_x86_64_gotpcrelx:
  movl external_data@GOTPCREL(%rip), %eax
