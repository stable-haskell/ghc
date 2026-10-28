.text
.globl fixture_aarch64_ldst128
fixture_aarch64_ldst128:
  ldr q0, [x1, :lo12:external_data]
