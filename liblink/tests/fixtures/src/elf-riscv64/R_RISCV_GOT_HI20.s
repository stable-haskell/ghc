.text
.globl fixture_riscv_got_hi20
fixture_riscv_got_hi20:
  auipc a0, %got_pcrel_hi(external_data)
