.text
.globl fixture_aarch64_ldst16
fixture_aarch64_ldst16:
  ldrh w0, [x1, :lo12:external_data]
