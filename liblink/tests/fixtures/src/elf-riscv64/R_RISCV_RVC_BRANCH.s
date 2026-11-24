.option rvc
.text
.globl fixture_riscv_rvc_branch
fixture_riscv_rvc_branch:
  c.beqz a0, .Lrvc_branch_target
.Lrvc_branch_target:
  c.nop
.reloc fixture_riscv_rvc_branch, R_RISCV_RVC_BRANCH, external_function
