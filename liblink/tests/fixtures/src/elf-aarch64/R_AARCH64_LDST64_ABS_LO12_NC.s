.text
.globl fixture_aarch64_ldst64
fixture_aarch64_ldst64:
  ldr x0, [x1, :lo12:external_data]
