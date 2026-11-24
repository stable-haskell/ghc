.text
.globl fixture_x86_64_rex_gotpcrelx
fixture_x86_64_rex_gotpcrelx:
  movq external_data@GOTPCREL(%rip), %rax
