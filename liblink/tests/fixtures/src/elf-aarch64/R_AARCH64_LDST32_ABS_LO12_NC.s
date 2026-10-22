.text
.globl fixture_aarch64_ldst32
fixture_aarch64_ldst32:
  ldr w0, [x1, :lo12:external_data]
