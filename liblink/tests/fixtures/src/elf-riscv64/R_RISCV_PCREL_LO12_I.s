.text
.globl fixture_riscv_pcrel_lo12_i
fixture_riscv_pcrel_lo12_i:
.Lpcrel_lo_i_hi:
  auipc a0, %pcrel_hi(external_data)
  addi a0, a0, %pcrel_lo(.Lpcrel_lo_i_hi)
