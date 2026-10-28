.text
.globl fixture_aarch64_add_lo12
fixture_aarch64_add_lo12:
  add x0, x0, :lo12:external_data
