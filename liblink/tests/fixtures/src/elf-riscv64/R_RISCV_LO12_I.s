.text
.globl fixture_riscv_lo12_i
fixture_riscv_lo12_i:
  addi a0, a0, %lo(external_data)
