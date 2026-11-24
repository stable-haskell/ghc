.option rvc
.text
.globl fixture_riscv_rvc_jump
fixture_riscv_rvc_jump:
  c.j .Lrvc_jump_target
.Lrvc_jump_target:
  c.nop
.reloc fixture_riscv_rvc_jump, R_RISCV_RVC_JUMP, external_function
