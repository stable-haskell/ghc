.text
.globl fixture_riscv_pcrel_lo12_s
fixture_riscv_pcrel_lo12_s:
.Lpcrel_lo_s_hi:
  auipc a0, %pcrel_hi(external_data)
  sw a1, %pcrel_lo(.Lpcrel_lo_s_hi)(a0)
