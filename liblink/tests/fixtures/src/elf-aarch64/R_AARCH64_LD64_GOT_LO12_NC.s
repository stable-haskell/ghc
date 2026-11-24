.text
.globl fixture_aarch64_ld64_got_lo12
fixture_aarch64_ld64_got_lo12:
  ldr x0, [x0, :got_lo12:external_data]
