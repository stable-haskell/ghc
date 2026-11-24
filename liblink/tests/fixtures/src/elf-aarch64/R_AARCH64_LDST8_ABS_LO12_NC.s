.text
.globl fixture_aarch64_ldst8
fixture_aarch64_ldst8:
  ldrb w0, [x1, :lo12:external_data]
