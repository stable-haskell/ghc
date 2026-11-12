.text
.globl fixture_riscv_pcrel_hi20
fixture_riscv_pcrel_hi20:
.Lpcrel_hi20:
  auipc a0, %pcrel_hi(external_data)
