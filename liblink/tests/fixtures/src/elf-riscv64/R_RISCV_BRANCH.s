.text
.globl fixture_riscv_branch
fixture_riscv_branch:
  beq a0, a1, .Lbranch_target
  nop
.Lbranch_target:
  nop
.reloc fixture_riscv_branch, R_RISCV_BRANCH, external_function
